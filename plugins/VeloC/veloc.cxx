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
#include <pdi/ref_any.h>
#include <veloc.h>

#include <fstream>
#include <iostream>

#include "veloc_wrapper.h"
#include "veloc_cfg.h"

using PDI::Context;
using PDI::Datatype_sptr;
using PDI::each;
using PDI::opt_each;
using PDI::Error;
using PDI::Config_error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_w;
using PDI::Ref_r;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;
using std::cout;
using std::endl;
using std::tie; 


class veloc_plugin : public Plugin
{
    Veloc_cfg m_config; 

    int recovered_iter;

    int status; 

    int cp_counter; 

    // protect all variables to be included in checkpoints 
    void protect_all_for_read(){
        for (auto&& data: m_config.protected_data()) {
            if (Ref_r ref = context().desc(data.second).ref()) {

                const Datatype_sptr type = ref.type();
                size_t n = 1;  
                size_t bytes = type-> datasize();

                if(auto* array_type = // If Datatype is an array 
                    dynamic_cast<const PDI::Array_datatype*>(type.get())) { 
                    n = array_type->subsize();
                }

                size_t sub_bytes = bytes/n; 

                if (!type->dense()) {
                    context().logger().warn("Sparse types are not supported (`{}')", data.second);
                    continue;
                }

                protect_data(context(), data.first, const_cast<void*>(ref.get()), n, sub_bytes);   

            } 
            else{
                //  context().logger().warn("Protected variable `{}' (id: {}) not available for reading", data.second, data.first);
                // unprotect_data(data.first);
                throw Error{PDI_ERR_VALUE, 
                        "Protected variable `{}' (id: {}) not available for reading", data.second, data.first};  
            }
        }
    }

    void unprotect_all(){
        for (auto&& data: m_config.protected_data()) {
            unprotect_data(context(), data.first);   
        }
    }

    // protect all variables to be restored 
    void protect_all_for_write(){
        for (auto&& data: m_config.protected_data()) {
            if (Ref_w ref = context().desc(data.second).ref()) {

                const Datatype_sptr type = ref.type();
                size_t n = 1;  
                size_t bytes = type-> datasize();

                if(auto* array_type = // If Datatype is an array 
                    dynamic_cast<const PDI::Array_datatype*>(type.get())) { 
                    n = array_type->subsize();
                }
                
                size_t sub_bytes = bytes/n; 

                if (!type->dense()) {
                    context().logger().warn("Sparse types are not supported (`{}')", data.second);
                    continue;
                }
                protect_data(context(),data.first, const_cast<void*>(ref.get()), n, sub_bytes);   
            } 
            else{
                throw Error{PDI_ERR_VALUE, 
                        "Protected variable `{}' (id: {}) not available for writing", data.second, data.first}; 
            }
        }
    }
    

    public : 
        veloc_plugin(Context& ctx, PC_tree_t config)
            : Plugin(ctx), m_config{ctx, config} 
        {

            if(m_config.failure()==1){
                status = 0;
            }
            else{
                status = 1; 
            }

            cp_counter = 0; 

            recovered_iter = -1; 
            
            // Initialize VeloC 
            init(MPI_COMM_WORLD, m_config.config());
            
            for (auto&& desc: m_config.descs()) {
                if (desc.second == Desc_type::STATUS) {
                    context().callbacks().add_data_callback([this](const string& name, Ref ref) {
                        if (Ref_w wref = ref) {
                            *static_cast<int*>(wref.get()) = status; 
                        }
                    },
                    desc.first);
                }
                else if (desc.second == Desc_type::COUNTER_CP) {
                    context().callbacks().add_data_callback([this](const string& name, Ref ref) {
                        if (Ref_w wref = ref) {
                            *static_cast<int*>(wref.get()) = cp_counter; 
                        }
                    },
                    desc.first);
                }
                else{
                    assert(false &&  "Unexpected desc type");
                }
            } // data call backs for descriptors 

            // event callbacks 
            for (auto&& event: m_config.events()) { // event call backs
                switch (event.second) {
                case Event_type::RECOVER_VAR: {
                	context().callbacks().add_event_callback([this](const string& event_name) {
                		for (int var_id: m_config.recover_var().at(event_name)) {
                			string desc_name = m_config.protected_data().at(var_id);
                            std::cout <<  "desc_name : "<< desc_name << std::endl << " var_id : " << var_id <<std::endl; 
                			if (Ref_w ref = context().desc(desc_name).ref()) {
                                const Datatype_sptr type = ref.type();
                                size_t n = 1;  
                                size_t bytes = type-> datasize();

                                if(auto* array_type = // If Datatype is an array 
                                    dynamic_cast<const PDI::Array_datatype*>(type.get())) { 
                                    n = array_type->subsize();
                                }

                                size_t sub_bytes = bytes/n; 

                                if (!type->dense()) {
                                    context().logger().warn("Sparse types are not supported (`{}')", desc_name);
                                    continue;
                                }

                                protect_data(context(),var_id, const_cast<void*>(ref.get()), n, sub_bytes);   
                            } 
                            else{
                                throw Error{PDI_ERR_VALUE, 
                                        "Protected variable `{}' (id: {}) not available for reading", desc_name, var_id};  
                            }
                		}
                        const auto& var_ids = m_config.recover_var().at(event_name);
                        for (int id : var_ids) {
                            std::cout << " var_ids : " << id << std::endl; 
                        }
                        std::vector<int> var_ids_vector(var_ids.begin(), var_ids.end());
                        std::cout << "var_ids_vector.data() = " << var_ids_vector.data() << std::endl;
                        for(int i=0; i <var_ids_vector.size(); i++) {
                            std::cout << " var_ids_vector[" << i << "] = " << var_ids_vector[i] <<std::endl;
                        }
                        selective_load(m_config.label(), var_ids_vector.data(), var_ids_vector.size());

                        for (auto&& var_id: var_ids) {
                            unprotect_data(context(),var_id);
                        }
                    },
                	event.first);
                } break;
                case Event_type::RECOVER: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        protect_all_for_write();
                        int result = load_checkpoint(context(), m_config.label(), m_config.requested_checkpoint()); 
                        recovered_iter = result;
                        status = 1; 
                        unprotect_all();
                    },
                    event.first);
                } break;
                case Event_type::STATE_SYNC: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        if (!status) { // recovery needed
                            protect_all_for_write();
                            int result = load_checkpoint(context(), m_config.label(),m_config.requested_checkpoint()); 
                            recovered_iter = result;
                            status = 1; 
                            unprotect_all();
                        } 
                        else if(status) { // recovery done or not needed 
                            protect_all_for_read();
                            int result = write_checkpoint(context(), m_config.when(),m_config.label(), m_config.iter_name()); 
                            if (result){
                                cp_counter++; 
                            }
                            unprotect_all();
                        }
                    },
                    event.first);
                } break;
                case Event_type::CHECKPOINT: {
                    Event_type event_type = event.second; // it this needed? 
                    context().callbacks().add_event_callback([this, event_type](const string& event_name) {

                        if(!status){
                            context().logger().warn("A checkpoint event was launched before a recovery event");
                        }

                        protect_all_for_read(); // will throw error if iter_name is not exposed 

                        std::cout << "in ckpt event handler " << std::endl; 

                        Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
                        auto new_iter = new_iter_r.scalar_value<int>();
                        if(new_iter!= recovered_iter){  
                            int result = write_checkpoint(context(), m_config.when(), m_config.label(), m_config.iter_name()); 
                            std::cout << "in if new inter condition " << std::endl; 
                            if (result){
                                cp_counter++; 
                            }
                        }
                        unprotect_all();
                    },
                    event.first);
                } break;
                case Event_type::START_CHECKPOINT: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        std::cout << "inside strat_checkpoint event handler" << std::endl; 
                        protect_all_for_read();
                        init_checkpoint(context(), m_config.label(), m_config.iter_name());
                    },
                    event.first);
                }break;
                case Event_type::START_RECOVERY: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        std::cout << "inside strat_checkpoint event handler" << std::endl; 
                        protect_all_for_read();
                        init_restart(context(), m_config.label());
                    },
                    event.first);
                }break;
                case Event_type::ROUTE_FILE_FOR_CP: {
                context().callbacks().add_event_callback([this](const string& event_name) {
                    std::cout << "insie route_file event handler" << std::endl; 
                    Ref_w wref = context().desc(m_config.manual_cp().routed_file).ref(); // ← Ref_w
                    std::cout << "m_config.manual_cp().routed_file = " << m_config.manual_cp().routed_file << std::endl;
                    if (!wref) {
                        throw Error{PDI_ERR_VALUE,
                            "Variable `{}' not available for writing", m_config.manual_cp().routed_file};
                    }
                    char* routed_chars = static_cast<char*>(wref.get()); // no const_cast needed
                    std::cout << "routed_chars = " << routed_chars << std::endl;
                    std::cout << "m_config.manual_cp().original_file = " << m_config.manual_cp().original_file << std::endl;
                    route_file(m_config.manual_cp().original_file, routed_chars);
                },
                event.first);
                } break;
                case Event_type::ROUTE_FILE_FOR_REC: {
                context().callbacks().add_event_callback([this](const string& event_name) {
                    std::cout << "insie route_file event handler" << std::endl; 
                    Ref_w wref = context().desc(m_config.manual_rec().routed_file).ref(); // ← Ref_w
                    std::cout << "m_config.manual_rec().routed_file = " << m_config.manual_rec().routed_file << std::endl;
                    if (!wref) {
                        throw Error{PDI_ERR_VALUE,
                            "Variable `{}' not available for writing", m_config.manual_rec().routed_file};
                    }
                    char* routed_chars = static_cast<char*>(wref.get()); // no const_cast needed
                    std::cout << "routed_chars = " << routed_chars << std::endl;
                    std::cout << "m_config.manual_rec().original_file = " << m_config.manual_rec().original_file << std::endl;
                    route_file(m_config.manual_rec().original_file, routed_chars);
                },
                event.first);
                } break;
                case Event_type::END_CHECKPOINT: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        end_checkpoint();
                        unprotect_all();
                    },
                    event.first);
                }break;
                case Event_type::END_RECOVERY: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        end_restart();
                        unprotect_all();
                    },
                    event.first);
                }break;
                default:
                    assert(false &&  "Unexpected event type");
                }
            } // event call backs
        }

        ~veloc_plugin(){
            finalize();
            context().logger().info("{} checkpoints were written", cp_counter);
            context().logger().info("Closing plugin");
        }
};
PDI_PLUGIN(veloc)
	

