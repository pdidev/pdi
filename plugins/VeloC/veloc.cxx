/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

#include <fstream>
#include <iostream>

#include "veloc_cfg.h"
#include "veloc_wrapper.h"

using PDI::Context;
using PDI::Datatype_sptr;
using PDI::each;
using PDI::Error;
using PDI::opt_each;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::tie;
using std::unordered_map;
using std::vector;

// Same logic as in scalar_datatype.cxx
namespace {
inline bool nulltype(const PDI::Datatype_sptr& d)
{
	const auto* scalar = dynamic_cast<const PDI::Scalar_datatype*>(d.get());
	if (!scalar) return false;
	if (scalar->buffersize()) return false;
	if (scalar->datasize()) return false;
	if (scalar->alignment()) return false;
	if (scalar->kind() != PDI::Scalar_kind::UNKNOWN) return false;
	return true;
}
} // anonymous namespace

class veloc_plugin: public Plugin
{
	Veloc_cfg m_config;

	int recovered_iter;
	int status;
	int cp_counter;

	void protect_all_for_read()
	{
		for (auto&& data: m_config.managed().protected_data) {
			Ref_r ref = context().desc(data.second).ref();
			if (nulltype(ref.type())) {
				throw Error{
					PDI_ERR_SPECTREE,
					"VELOC PLUGIN YAML: Protected data `{}' (id: `{}') has no valid type — "
					"check that the name in protect_data matches the data/metadata section",
					data.second,
					data.first
				};
			}
			if (ref) {
				const Datatype_sptr type = ref.type();
				size_t n = 1;
				size_t bytes = type->datasize();

				if (auto* array_type = dynamic_cast<const PDI::Array_datatype*>(type.get())) {
					n = array_type->subsize();
				}

				size_t sub_bytes = bytes / n;

				if (!type->dense()) {
					context().logger().warn("Sparse types are not supported (`{}')", data.second);
					continue;
				}

				protect_data(context(), data.first, const_cast<void*>(ref.get()), n, sub_bytes);
			}
		}
	}

	void unprotect_all()
	{
		for (auto&& data: m_config.managed().protected_data) {
			unprotect_data(context(), data.first);
		}
	}

	void protect_all_for_write()
	{
		for (auto&& data: m_config.managed().protected_data) {
			Ref_w ref = context().desc(data.second).ref();
			if (nulltype(ref.type())) {
				throw Error{
					PDI_ERR_SPECTREE,
					"VELOC PLUGIN YAML: Protected data `{}' (id: `{}') has no valid type — "
					"check that the name in protect_data matches the data/metadata section",
					data.second,
					data.first
				};
			}
			if (ref) {
				const Datatype_sptr type = ref.type();
				size_t n = 1;
				size_t bytes = type->datasize();

				if (auto* array_type =
				    dynamic_cast<const PDI::Array_datatype*>(type.get()))
				{
					n = array_type->subsize();
				}

				size_t sub_bytes = bytes / n;

				if (!type->dense()) {
					context().logger().warn("Sparse types are not supported (`{}')", data.second);
					continue;
				}

				protect_data(context(), data.first, const_cast<void*>(ref.get()), n, sub_bytes);
			}
		}
	}

public:
	veloc_plugin(Context& ctx, PC_tree_t config)
		: Plugin(ctx)
		, m_config{ctx, config}
		, cp_counter{0}
		, recovered_iter{-1}
	{
		status = m_config.failure() == 1 ? 0 : 1;

		init(context(), MPI_COMM_WORLD, m_config.config());

		for (auto&& desc: m_config.descs()) {
			if (desc.second == Desc_type::STATUS) {
				context().callbacks().add_data_callback(
					[this](const string&, Ref ref) {
						if (Ref_w wref = ref) {
							*static_cast<int*>(wref.get()) = status;
						}
					},
					desc.first
				);
			} else if (desc.second == Desc_type::COUNTER_CP) {
				context().callbacks().add_data_callback(
					[this](const string&, Ref ref) {
						if (Ref_w wref = ref) {
							*static_cast<int*>(wref.get()) = cp_counter;
						}
					},
					desc.first
				);
			} else {
				throw Error{PDI_ERR_IMPL, "Unexpected event type"};
			}
		}

            for (auto&& event: m_config.events()) { 
                switch (event.second) {
                    case Event_type::CHECKPOINT: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            if(!status){
                                context().logger().warn("A checkpoint event was launched before a recovery event");
                            }
							if (m_config.managed().when.to_long(context())){
								protect_all_for_read(); 
                            	Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
								auto new_iter = new_iter_r.scalar_value<int>();
								if(new_iter!= recovered_iter){  
									write_checkpoint(context(), m_config.label(), new_iter); 
									cp_counter++; 
								}
								unprotect_all();
							}
                        },
                        event.first);
                    } break;
                    case Event_type::RECOVER: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            protect_all_for_write();
                            int result = read_checkpoint(context(), m_config.label(), m_config.managed().requested_checkpoint); 
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
                                int result = read_checkpoint(context(), m_config.label(),m_config.managed().requested_checkpoint); 
                                recovered_iter = result;
                                status = 1; 
                                unprotect_all();
                            } 
                            else if(status) { // recovery not needed 
                                if (m_config.managed().when.to_long(context())){
									protect_all_for_read(); 
									Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
									auto new_iter = new_iter_r.scalar_value<int>();
									if(new_iter!= recovered_iter){  
										write_checkpoint(context(), m_config.label(), new_iter); 
										cp_counter++; 
									}
									unprotect_all();
								}
                            }
                        },
                        event.first);
                    } break;
                    case Event_type::START_CHECKPOINT: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
                            auto new_iter = new_iter_r.scalar_value<int>();
                            init_checkpoint(context(), m_config.label(), new_iter);
                        },
                        event.first);
                    }break;
                    case Event_type::START_RECOVERY: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            init_restart(context(), m_config.label(), m_config.custom().manual_rec.requested_checkpoint);
                        },
                        event.first);
                    }break;
                    case Event_type::ROUTE_FILE_FOR_CP: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        Ref_w wref = context().desc(m_config.custom().routed_file).ref(); 
                        if (wref) {
                            char* routed_chars = static_cast<char*>(wref.get()); 
                            route_file(context(),m_config.manual_cp().original_file, routed_chars);
                        }
                    },
                    event.first);
                    } break;
                    case Event_type::ROUTE_FILE_FOR_REC: {
                    context().callbacks().add_event_callback([this](const string& event_name) {
                        Ref_w wref = context().desc(m_config.custom().routed_file).ref();
                        if (wref) {
                            char* routed_chars = static_cast<char*>(wref.get()); 
                            route_file(context(),m_config.manual_rec().original_file, routed_chars);
                        }
                    },
                    event.first);
                    } break;
                    case Event_type::END_CHECKPOINT: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            end_checkpoint(context());
                            unprotect_all();
                        },
                        event.first);
                    }break;
                    case Event_type::END_RECOVERY: {
                        context().callbacks().add_event_callback([this](const string& event_name) {
                            end_restart(context());
                            unprotect_all();
                        },
                        event.first);
                    }break;
                    default:
                        throw Error{PDI_ERR_IMPL, "Unexpected event type"};
                    }
            } // event call backs
        }

	~veloc_plugin()
	{
		finalize(context());
		context().logger().info("{} checkpoints were written", cp_counter);
		context().logger().info("Closing plugin");
	}
};

PDI_PLUGIN(veloc)