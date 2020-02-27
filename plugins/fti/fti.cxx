/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <mpi.h>
#include <fti.h>
#include <cassert>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

#include "fti_cfg.h"
#include "fti_wrapper.h"

namespace {

using PDI::Datatype;
using PDI::Context;
using PDI::Error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_w;
using PDI::Ref_r;

using std::get;
using std::pair;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;

using namespace fti;

struct fti_plugin: Plugin {

	const Fti_cfg m_config;
	
	int m_ckpt_id;
	
	unique_ptr<Fti_wrapper> m_fti;
	
	unordered_map<string, int> m_stage_status;
	
	static pair<unordered_set<string>, unordered_set<string>> dependencies()
	{
		return {{"mpi"}, {"mpi"}};
	}
	
	void protect_for_read()
	{
		for (auto&& data: m_config.dataset()) {
			if (Ref_r ref = context().desc(data.second).ref()) {
				const Datatype& type = ref.type();
				if (!type.dense()) {
					context().logger()->warn("Sparse types are not supported (`{}')", data.second);
					continue;
				}
				m_fti->protect(data.first, const_cast<void*>(ref.get()), type.datasize(), FTI_CHAR);
			} else {
				context().logger()->warn("Protected variable `{}' (id: {}) not available for reading", data.second, data.first);
				m_fti->protect(data.first, nullptr, 0, FTI_CHAR);
			}
		}
	}
	
	void protect_for_write()
	{
		for (auto&& data: m_config.dataset()) {
			if (Ref_w ref = context().desc(data.second).ref()) {
				const Datatype& type = ref.type();
				if (!type.dense()) {
					context().logger()->warn("Sparse types are not supported (`{}')", data.second);
					continue;
				}
				m_fti->protect(data.first, ref.get(), type.datasize(), FTI_CHAR);
			} else {
				context().logger()->warn("Protected variable `{}' (id: {}) not available for writing", data.second, data.first);
				m_fti->protect(data.first, nullptr, 0, FTI_CHAR);
			}
		}
	}
	
	fti_plugin(Context& ctx, PC_tree_t config):
		Plugin{ctx},
		m_config{ctx, config},
		m_ckpt_id{0}
	{
		context().logger()->set_pattern("[PDI][FTI][%T] *** %^%l%$: %v");
		for (auto&& desc: m_config.descs()) {
			switch (desc.second) {
			case Desc_type::MPI_COMM: {
				context().add_data_callback([this](const string& name, Ref ref) {
					if (Ref_r rref = ref) {
						if (!m_config.init_on_event()) {
							if (!m_fti) {
								m_fti.reset(new Fti_wrapper{context(), m_config,
								        *static_cast<const MPI_Comm*>(rref.get())});
								context().logger()->info("Plugin initialized successfully");
							} else {
								context().logger()->warn("Trying to initialize plugin again after plugin initialization");
							}
						}
					}
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get FTI_COMM_WORLD before plugin initialization (`{}')", name);
						} else {
							context().logger()->debug("Writing FTI_COMM_WORLD to `{}'", name);
							*static_cast<MPI_Comm*>(wref.get()) = m_fti->fti_comm_world();
						}
					}
				},
				desc.first);
			} break;
			case Desc_type::STATUS: {
				context().add_data_callback([this](const string& name, Ref ref) {
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get FTI_status before plugin initialization (`{}')", name);
							return;
						}
						*static_cast<int*>(wref.get()) = m_fti->status();
					}
				},
				desc.first);
			} break;
			case Desc_type::DATA_SIZE: {
				int desc_id = m_config.dataset_sizes().at(desc.first);
				context().add_data_callback([this, desc_id](const string& name, Ref ref) {
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get size of variable (id: {}) before plugin initialization (`{}')", desc_id, name);
							return;
						}
						*static_cast<long*>(wref.get()) = m_fti->stored_size(desc_id);
					}
				},
				desc.first);
			} break;
			case Desc_type::HEAD: {
				context().add_data_callback([this](const string& name, Ref ref) {
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get head status before plugin initialization (`{}')", name);
							return;
						}
						*static_cast<int*>(wref.get()) = m_fti->head();
					}
				},
				desc.first);
			} break;
			case Desc_type::STAGE_DIR: {
				context().add_data_callback([this](const string& name, Ref ref) {
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get stage dir before plugin initialization (`{}')", name);
							return;
						}
						m_fti->stage_dir(static_cast<char*>(wref.get()), wref.type().datasize());
					}
				},
				desc.first);
			} break;
			case Desc_type::STAGE_STATUS: {
				context().add_data_callback([this](const string& name, Ref ref) {
					if (Ref_w wref = ref) {
						if (!m_fti) {
							context().logger()->warn("Trying to get stage status before plugin initialization (`{}')", name);
							return;
						}
						auto&& status = m_stage_status.find(name);
						if (status != m_stage_status.end()) {
							context().logger()->debug("Getting stage status (`{}' id: `{}')", name, status->second);
							*static_cast<int*>(wref.get()) = m_fti->stage_status(status->second);
						} else {
							context().logger()->debug("Could not find stage status (`{}')", name);
							*static_cast<int*>(wref.get()) = FTI_SI_NINI;
						}
					}
				},
				desc.first);
			} break;
			default:
				assert(false &&  "Unexpected desc type");
			}
		}
		
		for (auto&& event: m_config.events()) {
			switch (event.second) {
			case Event_type::INIT: {
				context().add_event_callback([this](const string& event_name) {
					if (!m_fti) {
						MPI_Comm comm = *static_cast<const MPI_Comm*>(Ref_r{context()[m_config.communicator()].ref()}.get());
						m_fti.reset(new Fti_wrapper{context(), m_config, comm});
						context().logger()->info("Plugin initialized successfully");
					} else {
						context().logger()->warn("Trying to initialize plugin again after plugin initialization (`{}')", event_name);
					}
				},
				event.first);
			} break;
			case Event_type::SEND_FILE: {
				context().add_event_callback([this](const string& event_name) {
					if (!m_fti) {
						context().logger()->warn("Trying to send file before plugin initialization (`{}')", event_name);
						return;
					}
					for (auto&& file: m_config.send_file().at(event_name)) {
						string src = get<0>(file).to_string(context());
						string dest = get<1>(file).to_string(context());
						string status = get<2>(file);
						context().logger()->debug("FTI_SendFile, src: `{}', dest: `{}', status: `{}'", src, dest, status);
						int file_id = m_fti->send_file(const_cast<char*>(src.c_str()), const_cast<char*>(dest.c_str()));
						if (file_id == FTI_NSCS) {
							context().logger()->warn("Could not send file `{}' to `{}'", src, dest);
						} else if (!status.empty()) {
							m_stage_status[status] = file_id;
						}
					}
				},
				event.first);
			} break;
			case Event_type::RECOVER_VAR: {
				context().add_event_callback([this](const string& event_name) {
					if (!m_fti) {
						context().logger()->warn("Trying to recover variable before plugin initialization (`{}')", event_name);
						return;
					}
					for (auto&& var_id: m_config.recover_var().at(event_name)) {
						string desc_name = m_config.dataset().at(var_id);
						if (Ref_w ref = context().desc(desc_name).ref()) {
							const Datatype& type = ref.type();
							if (!type.dense()) {
								context().logger()->warn("Sparse types are not supported (`{}')", desc_name);
								continue;
							}
							context().logger()->debug("FTI_Recover, var id: `{}', desc `{}'", var_id, desc_name);
							m_fti->protect(var_id, ref.get(), type.datasize(), FTI_CHAR);
							m_fti->recover_var(var_id);
						} else {
							context().logger()->warn("Variable `{}' (id: {}) unavailable", desc_name, var_id);
						}
					}
				},
				event.first);
			} break;
			case Event_type::RECOVER: {
				context().add_event_callback([this](const string& event_name) {
					if (!m_fti) {
						context().logger()->warn("Trying to recover before plugin initialization (`{}')", event_name);
						return;
					}
					protect_for_write();
					m_fti->recover();
				},
				event.first);
			} break;
			case Event_type::SNAPSHOT: {
				context().add_event_callback([this](const string& event_name) {
					if (!m_fti) {
						context().logger()->warn("Trying to snapshot before plugin initialization (`{}')", event_name);
						return;
					}
					if (m_fti->status()) {
						protect_for_write();
					} else {
						protect_for_read();
					}
					m_fti->snapshot();
				},
				event.first);
			} break;
			case Event_type::CHECKPOINT_L1:
			case Event_type::CHECKPOINT_L2:
			case Event_type::CHECKPOINT_L3:
			case Event_type::CHECKPOINT_L4: {
				Event_type event_type = event.second;
				context().add_event_callback([this, event_type](const string& event_name) {
					if (!m_fti) {
						context().logger()->warn("Trying to checkpoint before plugin initialization (`{}')", event_name);
						return;
					}
					protect_for_read();
					m_fti->checkpoint(++m_ckpt_id, static_cast<int>(event_type));
				},
				event.first);
			} break;
			default:
				assert(false &&  "Unexpected event type");
			}
		}
		
		context().logger()->info("Plugin loaded successfully");
		auto&& comm_desc = context()[m_config.communicator()];
		if (!m_config.init_on_event() && !comm_desc.empty()) {
			MPI_Comm comm = *static_cast<const MPI_Comm*>(Ref_r{comm_desc.ref()}.get());
			m_fti.reset(new Fti_wrapper{context(), m_config, comm});
			context().logger()->info("Plugin initialized successfully");
		}
	}
}; // struct fti_plugin

} // namespace <anonymous>

PDI_PLUGIN(fti)
