/*******************************************************************************
 * Copyright (C) 2026 Centre National de la Recherche Scientifique (CNRS)
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
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

#include "veloc_cfg.h"
#include "veloc_wrapper.h"

using PDI::Context;
using PDI::Datatype_sptr;
using PDI::Impl_error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Spectree_error;
using PDI::to_long;
using PDI::Type_error;
using PDI::Value_error;

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
	bool m_veloc_initialized;
	bool not_in_veloc_comm = false; // this rank is outside the VeloC communicator (MPI_COMM_NULL)
	int m_recovered_iter;
	int m_status;
	int m_cp_counter;
	int m_partial_counter;

	/// Returns true if rank is in veloc comm and if veloc has been initialized
	bool veloc_ready() const
	{
		if (not_in_veloc_comm) return false;
		if (!m_veloc_initialized) {
			throw Impl_error{"VeloC is not initialized: the communicator has not been shared yet"};
		}
		return true;
	}

	template <typename RefType>
	void protect_all()
	{
		for (auto&& data: m_config.managed().protected_data) {
			RefType ref = context().desc(data.first).ref();

			if (nulltype(ref.type())) {
				throw Spectree_error{
					m_config.tree(),
					"VELOC PLUGIN YAML: Protected data '{}' has no valid type, "
					"check that the name in protect_data matches the data/metadata section",
					data.first
				};
			}

			if (ref) {
				const Datatype_sptr type = ref.type();
				size_t n_elements = 1;
				size_t total_bytes = type->datasize();

				if (!type->dense()) {
					throw Impl_error{fmt::format("Sparse types are not supported: the data '{}' is sparse", data.first)};
				}

				if (auto* array_type = dynamic_cast<const PDI::Array_datatype*>(type.get())) {
					n_elements = array_type->subsize();
				}

				if (n_elements == 0) {
					throw Impl_error{fmt::format("The data '{}' has no element to protect", data.first)};
				}

				size_t element_bytes = total_bytes / n_elements;

				protect_data(context(), data.second, ref.get(), n_elements, element_bytes, data.first);
			}
		}
	}

	void unprotect_all()
	{
		for (auto&& data: m_config.managed().protected_data) {
			unprotect_data(context(), data.second, data.first);
		}
	}

public:
	veloc_plugin(Context& ctx, PC_tree_t config)
		: Plugin(ctx)
		, m_config{ctx, config}
		, m_veloc_initialized{false}
		, m_recovered_iter{-1}
		, m_status{1} // by default, m_status = 1 => recovery is not needed, app only wants to checkpoint
		, m_cp_counter{0}
		, m_partial_counter{0}
	{
		// if a comm was defined, initialize VeloC when the comm is shared
		// (otherwise VeloC is initialized with MPI_COMM_WORLD at the end of the constructor)
		if (!m_config.communicator().empty()) {
			ctx.on_data(
				[this](const std::string&, Ref ref) {
					if (m_veloc_initialized || not_in_veloc_comm) return;
					if (Ref_r r_ref = ref) {
						MPI_Comm comm = *static_cast<const MPI_Comm*>(r_ref.get());
						if (comm == MPI_COMM_NULL) {
							not_in_veloc_comm = true;
							return;
						}
						init(context(), comm, m_config.config());
						m_veloc_initialized = true;
					} else {
						throw Impl_error{"The MPI communicator can only be written to the plugin"};
					}
				},
				m_config.communicator()
			);
		}

		for (auto&& desc: m_config.descs()) {
			if (desc.second == Desc_type::STATUS) {
				ctx.on_data(
					[this](const std::string&, Ref ref) {
						// if app wants to read the status, therefore plugin writes it
						if (Ref_w w_ref = ref) {
							*static_cast<int*>(w_ref.get()) = m_status;
						}
						// if app wants to write the status, therefore plugin reads it
						else if (Ref_r r_ref = ref)
						{
							int status_value = *static_cast<const int*>(r_ref.get());
							if (status_value != 0 && status_value != 1) {
								throw Value_error{fmt::format("Invalid status value: {} (expected 0 or 1)", status_value)};
							}
							m_status = status_value;
						}
					},
					desc.first
				);
			} else if (desc.second == Desc_type::COUNTER_CP) {
				ctx.on_data(
					[this](const std::string&, Ref ref) {
						if (Ref_w wref = ref) {
							*static_cast<int*>(wref.get()) = m_cp_counter;
						} else {
							throw Impl_error{"The checkpoint counter can only be read from the plugin"};
						}
					},
					desc.first
				);
			} else {
				throw Type_error{"Unexpected Descriptor Type"};
			}
		} // data call backs

		for (auto&& event: m_config.events()) {
			switch (event.second) {
			case Event_type::CHECKPOINT: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						if (m_config.managed().when.to_long(context())) {
							PDI::TimerEventHandler veloc_timer(context(), "veloc");
							protect_all<Ref_r>();
							Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
							auto new_iter = new_iter_r.scalar_value<int>();
							if (new_iter != m_recovered_iter) {
								write_checkpoint(context(), m_config.label(), new_iter);
								m_cp_counter++;
							}
							unprotect_all();
						}
					},
					event.first
				);
			} break;
			case Event_type::RECOVER: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						protect_all<Ref_w>();
						int result = read_checkpoint(context(), m_config.label(), m_config.managed().requested_checkpoint);
						m_recovered_iter = result;
						m_status = 1;
						unprotect_all();
					},
					event.first
				);
			} break;
			case Event_type::STATE_SYNC: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						if (!m_status) { // recovery needed
							protect_all<Ref_w>();
							int result = read_checkpoint(context(), m_config.label(), m_config.managed().requested_checkpoint);
							m_recovered_iter = result;
							m_status = 1;
							unprotect_all();
						} else { // recovery not needed
							if (m_config.managed().when.to_long(context())) {
								protect_all<Ref_r>();
								Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
								auto new_iter = new_iter_r.scalar_value<int>();
								if (new_iter != m_recovered_iter) {
									write_checkpoint(context(), m_config.label(), new_iter);
									m_cp_counter++;
								}
								unprotect_all();
							}
						}
					},
					event.first
				);
			} break;
			case Event_type::START_CHECKPOINT: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						Ref_r new_iter_r = context().desc(m_config.iter_name()).ref();
						auto new_iter = new_iter_r.scalar_value<int>();
						init_checkpoint(context(), m_config.label(), new_iter);
						m_partial_counter++;
					},
					event.first
				);
			} break;
			case Event_type::START_RECOVERY: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						init_restart(context(), m_config.label(), m_config.custom().manual_rec.requested_checkpoint);
					},
					event.first
				);
			} break;
			case Event_type::ROUTE_FILE_FOR_CP: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
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
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
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
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						end_checkpoint(context());
						if (m_partial_counter == 1) {
							m_cp_counter++;
							m_partial_counter--;
						}
					},
					event.first
				);
			} break;
			case Event_type::END_RECOVERY: {
				ctx.on_event(
					[this](const std::string&) {
						if (!veloc_ready()) return;
						PDI::TimerEventHandler veloc_timer(context(), "veloc");
						end_restart(context());
					},
					event.first
				);
			} break;
			default:
				throw Type_error{"Unexpected event type"};
			}
		} // event call backs

		// VeloC is initialized last to avoid the constructor throwing without finalizing VeloC
		if (m_config.communicator().empty()) {
			init(ctx, MPI_COMM_WORLD, m_config.config());
			m_veloc_initialized = true;
		}
	}

	~veloc_plugin()
	{
		context().logger().info("{} checkpoints were written", m_cp_counter);
		context().logger().info("Closing plugin");
		if (m_veloc_initialized) {
			finalize(context());
		}
	}
};

PDI_PLUGIN(veloc)
