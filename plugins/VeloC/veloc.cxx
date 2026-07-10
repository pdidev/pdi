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

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/error.h>

#include "veloc_cfg.h"
#include "veloc_wrapper.h"

using PDI::Context;
using PDI::Datatype_sptr;
using PDI::Spectree_error;
using PDI::Impl_error;
using PDI::Type_error;
using PDI::Value_error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::to_long;
using std::string;

// Same logic as in scalar_datatype.cxx
namespace {
inline bool nulltype(const PDI::Datatype_sptr& datatype)
{
	const auto* scalar = dynamic_cast<const PDI::Scalar_datatype*>(datatype.get());
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
	int partial_counter; 

	template <typename RefType>
	void protect_all()
	{
		for (auto&& data: m_config.managed().protected_data) {
			RefType ref = context().desc(data.second).ref();

			if (nulltype(ref.type())) {
				throw Spectree_error{
					m_config.tree(),
					"VELOC PLUGIN YAML: Protected data `{}' (id: `{}') has no valid type, "
					"check that the name in protect_data matches the data/metadata section",
					data.second,
					data.first
				};
			}

			if (ref) {
				const Datatype_sptr type = ref.type();
				size_t n_elements = 1;
				size_t total_bytes = type->datasize(); 

				if (!type->dense()) { 
					throw Impl_error{
						fmt::format("Sparse types are not supported (`{}`)", data.second)
					};
				}
				
				if (auto* array_type = dynamic_cast<const PDI::Array_datatype*>(type.get())) {
					n_elements = array_type->subsize();
				}

				size_t element_bytes = total_bytes / n_elements;

				protect_data(context(), data.first, ref.get(), n_elements, element_bytes);
			}
		}
	}

	void unprotect_all()
	{
		for (auto&& data: m_config.managed().protected_data) {
			unprotect_data(context(), data.first);
		}
	}

public:
	veloc_plugin(Context& ctx, PC_tree_t config)
		: Plugin(ctx)
		, m_config{ctx, config}
		, cp_counter{0}
		, recovered_iter{-1}
	{
		// by default, status = 1 => recovery is done and app only wants to checkpoint 
		status = 1; 

		init(context(), MPI_COMM_WORLD, m_config.config());

		for (auto&& desc: m_config.descs()) {
			if (desc.second == Desc_type::STATUS) {
				context().callbacks().add_data_callback(
					[this](const string&, Ref ref) {
						// if app wants to read the status, therefore plugin writes it 
						if (Ref_w w_ref = ref) {
							*static_cast<int*>(w_ref.get()) = status;
						}
						// if app wants to write the status, therefore plugin reads it 
						else if (Ref_r r_ref = ref) {
							int status_value = *static_cast<const int*>(r_ref.get());
							if(status_value !=0 && status_value != 1){
								throw Value_error{
									fmt::format("Invalid status value: {} (expected 0 or 1)", status_value)
								};
							}
							status = status_value;
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
				throw Type_error{"Unexpected Desc Type"};
			}
		} // data call backs

		for (auto&& event: m_config.events()) {
			switch (event.second) {
			case Event_type::CHECKPOINT: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						if (!status) {
							context().logger().warn("A checkpoint event was launched before a recovery event");
						}
						if (m_config.managed().when.to_long(context())) {
							protect_all<Ref_r>();
							Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
							auto new_iter = new_iter_r.scalar_value<int>();
							if (new_iter != recovered_iter) {
								write_checkpoint(context(), m_config.label(), new_iter);
								cp_counter++;
							}
							unprotect_all();
						}
					},
					event.first
				);
			} break;
			case Event_type::RECOVER: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						protect_all<Ref_w>();
						int result = read_checkpoint(context(), m_config.label(), m_config.managed().requested_checkpoint);
						recovered_iter = result;
						status = 1;
						unprotect_all();
					},
					event.first
				);
			} break;
			case Event_type::STATE_SYNC: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						if (!status) { // recovery needed
							protect_all<Ref_w>();
							int result = read_checkpoint(context(), m_config.label(), m_config.managed().requested_checkpoint);
							recovered_iter = result;
							status = 1;
							unprotect_all();
						} else if (status) { // recovery not needed
							if (m_config.managed().when.to_long(context())) {
								protect_all<Ref_r>();
								Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
								auto new_iter = new_iter_r.scalar_value<int>();
								if (new_iter != recovered_iter) {
									write_checkpoint(context(), m_config.label(), new_iter);
									cp_counter++;
								}
								unprotect_all();
							}
						}
					},
					event.first
				);
			} break;
			case Event_type::START_CHECKPOINT: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
						auto new_iter = new_iter_r.scalar_value<int>();
						init_checkpoint(context(), m_config.label(), new_iter);
						partial_counter++; 
					},
					event.first
				);
			} break;
			case Event_type::START_RECOVERY: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						init_restart(context(), m_config.label(), m_config.custom().manual_rec.requested_checkpoint);
					},
					event.first
				);
			} break;
			case Event_type::ROUTE_FILE_FOR_CP: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						Ref_w wref = context().desc(m_config.custom().routed_file).ref();
						if (wref) {
							char* routed_chars = static_cast<char*>(wref.get());
							route_file(context(), m_config.manual_cp().original_file, routed_chars);
						}
					},
					event.first
				);
			} break;
			case Event_type::ROUTE_FILE_FOR_REC: {
				context().callbacks().add_event_callback(
					[this](const string& event_name) {
						Ref_w wref = context().desc(m_config.custom().routed_file).ref();
						if (wref) {
							char* routed_chars = static_cast<char*>(wref.get());
							route_file(context(), m_config.manual_rec().original_file, routed_chars);
						}
					},
					event.first
				);
			} break;
			case Event_type::END_CHECKPOINT: {
				context().callbacks().add_event_callback([this](const string& event_name) { 
					end_checkpoint(context()); 
					if(partial_counter==1){
						cp_counter++;
						partial_counter--;
					}
				}, event.first);
			} break;
			case Event_type::END_RECOVERY: {
				context().callbacks().add_event_callback([this](const string& event_name) { end_restart(context()); }, event.first);
			} break;
			default:
				throw Type_error{"Unexpected event type"};
			}
		} // event call backs
	}

	~veloc_plugin()
	{
		context().logger().info("{} checkpoints were written", cp_counter);
		context().logger().info("Closing plugin");
		finalize(context());
	}
};

PDI_PLUGIN(veloc)
