#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <veloc.h>

// #include <hdf5.h>
#include <iostream>
// #include "../decl_hdf5/dataset_op.h"
// #include "../decl_hdf5/collision_policy.h"
// #include "../decl_hdf5/file_op.h"
// #include "../decl_hdf5/hdf5_wrapper.h"

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
	
    unordered_map<string, bool> restore_from_last_checkpoint;

	unordered_map<string, bool> save_memory_regions;

public:
    veloc_plugin(Context& ctx, PC_tree_t config)
        : Plugin(ctx), failure_value(-1), when(1L) // construct and initialize 
    {

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
                else if(key == "iteration_name" ){
                    iter_name = to_string(value);
                }
				else {
					throw Config_error{config, "Unknown key in VeloC plugin configuration: `{}'", key};
				}
			});
		});

		// pass 2 

		PC_tree_t protect_tree = PC_get(config, ".checkpoint_data");

		if (!PC_status(PC_get(protect_tree, "[0]"))) { // it's a list of names only
			each(protect_tree, [&](PC_tree_t tree) {

				string data_name = to_string(tree);
				protected_data.push_back(data_name);

				if(failure_value==1){
					restore_from_last_checkpoint[data_name] = true; 
				}

                save_memory_regions[data_name] = true;
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

		if (protected_data.size() ==0 ){
			throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The data to be included in checkpoints must be defined"};
		}

        if(iter_name.empty()){
            throw Error{PDI_ERR_CONFIG, 
				"VeloC plugin: The name used to indicate the iteration number in the PDI data store must be defined"};
        }

		if (VELOC_Init(MPI_COMM_WORLD, veloc_file.c_str()) != VELOC_SUCCESS) {
			printf("Error initializing VELOC! Aborting...\n");
			exit(2);
		}
    

        ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) {

				if(save_memory_regions[name]){

					// for sure : 
					// "ref.type()->datasize() returns the nr of bytes of a type "  (Dataset.cxx)
					// ref.type()->subsize() returns number of actual elements in the array 

                    auto it = std::find(protected_data.begin(), protected_data.end(), name);
                    size_t index = -1;
                    
                    if (it != protected_data.end()) {
                        index = it - protected_data.begin();  
                    }

                
                    if(ref.type()->dense()){
                        Ref_r read_ref{ref};
                        // size_t n = ref.type()->subsize();
                        size_t bytes = ref.type()-> datasize();
                        VELOC_Mem_protect(index, const_cast<void*>(read_ref.get()), 1, bytes);
                    }

                    else{
                        // size_t n = ref.type()->subsize();
                        Ref_r read_ref{ref};
                        size_t bytes = ref.type()->buffersize();
                        VELOC_Mem_protect(index, const_cast<void*>(read_ref.get()), 1, bytes);
                    }

                    save_memory_regions[name] = false;

				}

                if(restore_from_last_checkpoint[name] == true){

                    this->load_checkpoint(name, ref);

                    // keep it now for testing purposes 
                    this->write_checkpoint(name, ref); 
                }
                else if (std::find(protected_data.begin(), protected_data.end(), name) != protected_data.end()){
                    this->write_checkpoint(name, ref); 
                }
        });
    }

    ~veloc_plugin(){
        // if (0 > H5close()) handle_hdf5_err("Cannot finalize HDF5 library");
        VELOC_Finalize(1); 
        context().logger().info("Closing plugin");
    }

private:
    void write_checkpoint(const std::string& name, Ref ref){

		if(when.to_long(context())){

            Ref_r ref_r_iter = context().desc(iter_name).ref();

            auto i = ref_r_iter.scalar_value<int>();

			if (VELOC_Checkpoint(cp_label.c_str(), i) != VELOC_SUCCESS) {
				printf("Error checkpointing! Aborting...\n");
				exit(2);
            }
		}
    }

    void load_checkpoint(const std::string& name, Ref ref){


        int v = VELOC_Restart_test(cp_label.c_str(), 0); // 0 means restart from latest checkpoint 
        if (v > 0) {
        printf("Previous checkpoint found at iteration %d, initiating restart...\n", v);
        // v can be any version, independent of what VELOC_Restart_test is returning
            if (VELOC_Restart(cp_label.c_str(), v) != VELOC_SUCCESS) {
                printf("Error restarting from checkpoint! Aborting...\n");
                exit(2);
            }
        }

        restore_from_last_checkpoint[name] = false; 

    }
	// if(dset_op.when().to_long(ctx))
	// const PDI::Expression& when() const { return m_when; }

	
};

PDI_PLUGIN(veloc)
