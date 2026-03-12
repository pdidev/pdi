/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <veloc.h>

#include <iostream>

using PDI::Context;
using PDI::each;
using PDI::opt_each;
using PDI::Error;
using PDI::Config_error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;
using std::cout;
using std::endl;
using std::tie; 

// using namespace decl_hdf5;

class veloc_plugin : public Plugin
{
private:

	string veloc_file; 

	PDI::Expression when;

	long int failure_value;

    string cp_label; 

    string iter_name; 

	vector<string> protected_data; 

    unordered_map<string, Ref> protected_data_refs;
	
    bool restore_from_last_checkpoint;

	unordered_map<string, bool> register_memory_regions;

    PC_tree_t saved_config;

    string checkpoint_event_name; 

    long int iter;

public:
    veloc_plugin(Context& ctx, PC_tree_t config)
        : Plugin(ctx), failure_value(-1), when(1L), restore_from_last_checkpoint(false), iter(0) // construct and initialize 
    {
        saved_config = PC_get(config, ".checkpoint_data");   

		// pass 1

        opt_each(config, [&](PC_tree_t item) { 
            each(item, [&](PC_tree_t key_tree, PC_tree_t value) {
                
				string key = to_string(key_tree);
				
                if (key == "failure") {
                    failure_value = to_long(value);
                } 
                else if (key == "checkpoint_label") {
                    cp_label = to_string(value);
                }
                else if (key == "veloc_cfg_path"){
                    veloc_file = to_string(value);
                }
				else if (key == "when"){
					when = to_string(value);
				}
				else if (key == "checkpoint_data"){
					// parsed in pass 2 
				}
                else if(key == "iteration_name_in_cp_file" ){
                    iter_name = to_string(value);
                }
                else if(key == "on_event" ){
                    checkpoint_event_name = to_string(value);
                }
				else {
					throw Config_error{config, "Unknown key in VeloC plugin configuration: `{}'", key};
				}
			});
		});

		// pass 2 

		PC_tree_t protect_tree = PC_get(config, ".checkpoint_data");
        
        if(failure_value==1){
			restore_from_last_checkpoint = true; 
		}

		if (!PC_status(PC_get(protect_tree, "[0]"))) { // it's a list of names only
			each(protect_tree, [&](PC_tree_t tree) {

				string data_name = to_string(tree);
				
                protected_data.push_back(data_name);

                register_memory_regions[data_name] = true;
			});
		}
   
		// conformity checks 

		if(failure_value!=0 && failure_value!=1){
            throw Error{PDI_ERR_CONFIG, 
                "VeloC plugin: the failure key must be 1 or 0 depening if a failure occurred or not"};
        }

		if (cp_label.empty()){
			throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The name of the checkpoint label must be defined"};
		}

        if (checkpoint_event_name.empty()){
			throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The name of the checkpoint event must be defined"};
		}

		if (protected_data.size() ==0 ){
			throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The data to be included in checkpoints must be defined"};
		}

        if(iter_name.empty()){
            throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The name of the iteration number in the PDI data store must be defined"};
        }

        if (std::find(protected_data.begin(), protected_data.end(), 
                iter_name) == protected_data.end()){  
            throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The iteration number must be included in checkpoint_data "
            };
        }

		if (VELOC_Init(MPI_COMM_WORLD, veloc_file.c_str()) != VELOC_SUCCESS) {
			printf("Error initializing VELOC! Aborting...\n");
			exit(2);
		}
    
        ctx.callbacks().add_event_callback([this](const std::string& name) { 

           // cout << "before event(name) call" << endl; 
            event(name); 
        });

        ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) {


            // Check if name is a protected_data
            auto it_vec = std::find(protected_data.begin(), protected_data.end(), name);
            if (it_vec == protected_data.end()) {
                cout << " Returning 2" << endl; 
                return;
            }

            // Check if name is a mem region to be protected 
            auto it_map = register_memory_regions.find(name);
            if (it_map == register_memory_regions.end()) {
                cout << " Returning 1" << endl; 
                return;
            }

        size_t index = std::distance(protected_data.begin(), it_vec);

            if (it_map->second){ //if memory needs to be registered 

                // for sure  
                // "ref.type()->datasize() returns the nr of bytes of a type "  (Dataset.cxx)
                // ref.type()->subsize() returns number of actual elements in the array 

                if(ref.type()->dense()){
                    Ref_r read_ref{ref};
                    // size_t n = ref.type()->subsize();

                    void* ptr = const_cast<void*>(read_ref.get());
                   

                    size_t bytes = ref.type()-> datasize();

                    printf("REGISTER %s: ptr=%p, bytes=%zu\n", name.c_str(), ptr, bytes);

                    VELOC_Mem_protect(index, const_cast<void*>(read_ref.get()), 1, bytes);
                    cout << " memory registered for " << name << "at " << index << endl;
                }

                else{
                    // size_t n = ref.type()->subsize();
                    Ref_r read_ref{ref};

                    void* ptr = const_cast<void*>(read_ref.get());
                    

                    size_t bytes = ref.type()->buffersize();

                    printf("REGISTER %s: ptr=%p, bytes=%zu\n", name.c_str(), ptr, bytes);

                    VELOC_Mem_protect(index, const_cast<void*>(read_ref.get()), 1, bytes);
                    cout << " memory registered for " << name << endl;
                }

                register_memory_regions[name] = false;

            }

            if(restore_from_last_checkpoint && memoryRegionsWereRegistered()){

                load_checkpoint();

            }

            // TO DO : make checkpointing only on-event bc like otherwise I get too many checpoints file written and then overwritten 
            //  like if there are more arrays exposed independently in one iter. 

        });
    }

    ~veloc_plugin(){
        // if (0 > H5close()) handle_hdf5_err("Cannot finalize HDF5 library");
        VELOC_Finalize(1); 
        context().logger().info("Closing plugin");
    }

private:
    void write_checkpoint(){

		if(when.to_long(context())){

            Ref_r ref_r_iter = context().desc(iter_name).ref();

            auto i = ref_r_iter.scalar_value<int>();

            cout << " in wc iter from ref is equal to = " <<  i << endl;

			if (VELOC_Checkpoint(cp_label.c_str(), i) != VELOC_SUCCESS) {
				printf("Error checkpointing! Aborting...\n");
				exit(2);
            }

            cout << " checkpoint wrote " << endl;
		}
    }

    void load_checkpoint(){
        
        int v = VELOC_Restart_test(cp_label.c_str(), 0); // 0 means restart from latest checkpoint 
        if (v > 0) {
        printf("Previous checkpoint found at iteration %d, initiating restart...\n", v);
        // v can be any version, independent of what VELOC_Restart_test is returning
            if (VELOC_Restart(cp_label.c_str(), v) != VELOC_SUCCESS) {
                printf("Error restarting from checkpoint! Aborting...\n");
                exit(2);
            }
        }

        Ref_r ref_r_iter = context().desc(iter_name).ref();

        auto i = ref_r_iter.scalar_value<int>();

        cout << " in lc iter from ref is equal to = " <<  i << endl;

        restore_from_last_checkpoint = false; 

    }
	
    void event(const std::string& event)
	{
		if (event == "simulate_failure"){

            cout << "handling failure simulation event " << endl; 

            // for(int i=0; i < protected_data.size();i++){
            //     VELOC_Mem_unprotect(i);
            // }

            restore_from_last_checkpoint = true; 

           for(auto &key_value : register_memory_regions){
                auto &value = key_value.second;
                value = true;
           };

           for(auto &key_value : register_memory_regions){

                auto key = key_value.first;
                auto value = key_value.second;
                cout << "key : " << key << " value : " << value << endl;
           };

            cout << " failure simulated"<< endl;

        }

        else if(event == checkpoint_event_name){


            cout << " checkpoint event called "  << endl;

            // TO DO: COUDL PACK THESE LINES OF CODE SOMEWHERE SINCE THEY ARE REPLICATED BETWEEN DATA CALLS AND EVENT CALLS 

            if(restore_from_last_checkpoint == true){

                cout << " restoring before checkpointing -- should not happen " << endl;

                if(memoryRegionsWereRegistered()){

                    this->load_checkpoint();

                }

                else {
                    throw Error{PDI_ERR_VALUE, 
                        "VeloC plugin: Not all data to be included in checkpoints "
                            "has been exposed before calling the checkpoint event"}; 
                }
            }

            // TO DO END 
 
            write_checkpoint(); 
    
        }
	}

    bool memoryRegionsWereRegistered() {

        return std::all_of(register_memory_regions.begin(), register_memory_regions.end(),
                       [](const auto& pair) { return pair.second == false; });
    }
	
};

PDI_PLUGIN(veloc)
