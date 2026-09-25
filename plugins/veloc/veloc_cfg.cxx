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
#include <veloc.h>
#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include <unordered_map>

#include "veloc_cfg.h"

using PDI::Context;
using PDI::each;
using PDI::Expression;
using PDI::Impl_error;
using PDI::len;
using PDI::opt_each;
using PDI::Spectree_error;
using PDI::to_long;
using PDI::to_string;
using PDI::Type_error;

namespace {
/**
	* @brief Used to register event names of a given type into the events map (m_events)
*/
bool load_events(std::unordered_map<std::string, Event_type>& events, Context& ctx, PC_tree_t tree, Event_type event_type)
{
	const std::map<Event_type, std::string> event_names
		= {{Event_type::CHECKPOINT, "checkpoint_on_event"},
	       {Event_type::RECOVER, "recover_on_event"},
	       {Event_type::STATE_SYNC, "synchronize_on_event"},
	       {Event_type::START_CHECKPOINT, "start_cp_on_event"},
	       {Event_type::END_CHECKPOINT, "end_cp_on_event"},
	       {Event_type::ROUTE_FILE_FOR_CP, "route_file_for_cp_on_event"},
	       {Event_type::ROUTE_FILE_FOR_REC, "route_file_for_rec_on_event"},
	       {Event_type::START_RECOVERY, "start_rec_on_event"},
	       {Event_type::END_RECOVERY, "end_rec_on_event"}};

	bool inserted = false;

	auto insert_event = [&](PC_tree_t subtree) {
		auto&& result = events.emplace(to_string(subtree), event_type);
		if (result.second) {
			inserted = true;
		} else {
			throw Spectree_error{
				tree,
				"Duplicate event name `{}' in `{}' (previously defined in `{}')",
				result.first->first,
				event_names.at(event_type),
				event_names.at(result.first->second)
			};
		}
	};

	if (!PC_status(PC_get(tree, "[0]"))) {
		each(tree, insert_event);
	} else {
		insert_event(tree);
	}
	return inserted;
}

/**
	* @brief Used to register a descriptor name (status / counter) into the descs map.
*/
bool load_desc(std::unordered_map<std::string, Desc_type>& descs, Context& ctx, const std::string& name, Desc_type desc_type)
{
	const std::map<Desc_type, std::string> desc_names = {
		{Desc_type::STATUS, "status"},
		{Desc_type::COUNTER_CP, "counter"},
	};
	auto&& result = descs.emplace(name, desc_type);
	if (!result.second) {
		ctx.logger().warn(
			"Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')",
			name,
			desc_names.at(desc_type),
			desc_names.at(result.first->second)
		);
	}
	return result.second;
}

/**
	* @brief checks whether the parameters of a ManualCheckpoint/ManualRecovery 
		have been defined and all the required events have been registered
*/
template <typename OpType>
bool validate_manual_op(PC_tree_t tree, const std::unordered_map<std::string, Event_type>& events, OpType cfg)
{
	if (cfg.original_file.empty()) {
		throw Spectree_error{tree, "'filename' is undefined in custom checkpoint or recovery"};
	}

	const std::map<Event_type, std::string> event_names
		= {{Event_type::START_CHECKPOINT, "start_on_event"},
	       {Event_type::END_CHECKPOINT, "end_on_event"},
	       {Event_type::ROUTE_FILE_FOR_CP, "route_file_on_event"},
	       {Event_type::ROUTE_FILE_FOR_REC, "route_file_on_event"},
	       {Event_type::START_RECOVERY, "start_on_event"},
	       {Event_type::END_RECOVERY, "end_on_event"}};

	if (std::is_same_v<ManualCheckpoint, OpType>) {
		std::array<Event_type, 3> required{Event_type::START_CHECKPOINT, Event_type::ROUTE_FILE_FOR_CP, Event_type::END_CHECKPOINT};

		for (auto event_type: required) {
			bool defined = std::any_of(events.begin(), events.end(), [event_type](const auto& event) { return event.second == event_type; });

			if (!defined) {
				throw Spectree_error{tree, "'{}' is undefined in 'custom_checkpoint'", event_names.at(event_type)};
			}
		}
	} else if (std::is_same_v<ManualRecovery, OpType>) {
		std::array<Event_type, 3> required{Event_type::START_RECOVERY, Event_type::ROUTE_FILE_FOR_REC, Event_type::END_RECOVERY};

		for (auto event_type: required) {
			bool defined = std::any_of(events.begin(), events.end(), [event_type](const auto& event) { return event.second == event_type; });

			if (!defined) {
				throw Spectree_error{tree, "'{}' is undefined in 'custom_recover'", event_names.at(event_type)};
			}
		}
	} else {
		throw Type_error{"Unknown manual operation type"};
	}

	return true;
}

/**
	* @brief checks whether the parameters of a CustomCheckpointingCfg have been defined 
*/
bool validate_custom_config(
	Context& ctx,
	PC_tree_t tree,
	CustomCheckpointingCfg cfg,
	std::unordered_map<std::string, Event_type> events,
	bool status_defined,
	ManualCheckpoint manual_cp,
	ManualRecovery manual_rec
)
{
	if (cfg.routed_file.empty()) {
		throw Spectree_error{tree, "'veloc_file' is undefined defined in 'custom_checkpointing' "};
	}
	if (!manual_cp.is_valid && !manual_rec.is_valid) {
		throw Spectree_error{
			tree,
			"no custom checkpoint or recovery configurations have been "
			"defined in 'custom_checkpointing'"
		};
	}
	return true;
}

/**
	* @brief checks whether the parameters of a ManagedCheckpointingCfg have been defined 
*/
bool validate_managed_config(
	Context& ctx,
	PC_tree_t tree,
	ManagedCheckpointingCfg cfg,
	std::unordered_map<std::string, Event_type> events,
	std::string iter_name,
	bool status_defined
)
{
	if (cfg.protected_data.size() == 0) {
		throw Spectree_error{tree, "'protected_data' is undefined defined in 'managed_checkpointing'"};
	}

	// iteration must be included in protect_data
	bool iter_protected
		= std::any_of(cfg.protected_data.begin(), cfg.protected_data.end(), [&iter_name](const auto& data) { return data.first == iter_name; });
	if (!iter_protected) {
		throw Spectree_error{
			tree,
			"the iteration variable `{}' is not included in "
			"'protected_data'",
			iter_name
		};
	}

	bool cp_events_defined = std::any_of(events.begin(), events.end(), [](const auto& event) { return event.second == Event_type::CHECKPOINT; });
	bool rec_events_defined = std::any_of(events.begin(), events.end(), [](const auto& event) { return event.second == Event_type::RECOVER; });
	bool sync_events_defined = std::any_of(events.begin(), events.end(), [](const auto& event) { return event.second == Event_type::STATE_SYNC; });

	if (!cp_events_defined && !rec_events_defined && !sync_events_defined) {
		throw Spectree_error{
			tree,
			"no checkpoint, recovery, or synchronization events have "
			"been configured in 'managed_checkpointing'"
		};
	}

	if (sync_events_defined && !status_defined) {
		throw Spectree_error{
			tree,
			"synchronization events have been defined in `managed_checkpointing' "
			"but no 'status' key has been defined. "
		};
	}

	// Warn : "when" key defined without checkpoint events
	if (!cp_events_defined) {
		if (cfg.when != 1L) {
			ctx.logger().warn("No checkpoint events have been defined "
			                  "in `managed_checkpointing'. Ignoring 'when' key");
		}
	}

	// Warn : "recover_at_or_before_iteration" defined without recovery events
	if (!rec_events_defined && cfg.requested_checkpoint != -1) {
		ctx.logger().warn("No recovery events have been defined "
		                  "in 'managed_checkpointing'. Ignoring 'recover_at_or_before_iteration' key");
	}

	return true;
}

} // anonymous namespace

Veloc_cfg::Veloc_cfg(Context& ctx, PC_tree_t tree)
{
	//  STEP 1
	PC_tree_t m_tree = tree;

	bool status_key_defined = false;

	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		std::string key = to_string(key_tree);

		if (key == "config_file") {
			m_config_file = to_string(value);
		} else if (key == "checkpoint_label") {
			m_cp_label = to_string(value);
		} else if (key == "iteration") {
			m_iter_name = to_string(value);
		} else if (key == "status") {
			load_desc(m_descs, ctx, to_string(value), Desc_type::STATUS);
			status_key_defined = true;
		} else if (key == "counter") {
			load_desc(m_descs, ctx, to_string(value), Desc_type::COUNTER_CP);
		} else if (key == "managed_checkpointing") {
			// parsed in step 2
		} else if (key == "custom_checkpointing") {
			// parsed in step 3
		} else {
			throw Spectree_error{tree, "Unknown key in VeloC plugin configuration: `{}'", key};
		}
	});

	//  Step 2
	PC_tree_t managed_tree = PC_get(tree, ".managed_checkpointing");
	if (!PC_status(managed_tree)) {
		each(managed_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
			std::string key = to_string(key_tree);

			if (key == "protect_data") {
				// list of data names; position in the list becomes the id
				if (!PC_status(PC_get(value, "[0]"))) {
					int data_id = 0;
					each(value, [&](PC_tree_t item) {
						std::string data_name = to_string(item);
						if (!m_managed.protected_data.emplace(data_name, data_id).second) {
							ctx.logger().warn("Duplicate data ('{}') in 'protected_data'", data_name);
						}
						data_id++;
					});
				}
			} else if (key == "checkpoint_on_event") {
				load_events(m_events, ctx, value, Event_type::CHECKPOINT);
			} else if (key == "recover_on_event") {
				load_events(m_events, ctx, value, Event_type::RECOVER);
			} else if (key == "synchronize_on_event") {
				load_events(m_events, ctx, value, Event_type::STATE_SYNC);
			} else if (key == "when") {
				m_managed.when = to_string(value);
			} else if (key == "recover_at_or_before_iteration") {
				m_managed.requested_checkpoint = to_long(value);
			} else {
				throw Spectree_error{tree, "VeloC config: unknown key `{}' in `managed_checkpointing', ignoring.", key};
			}
		});
		m_managed.is_valid = validate_managed_config(ctx, managed_tree, m_managed, m_events, m_iter_name, status_key_defined);
	}

	// Step 3
	PC_tree_t custom_tree = PC_get(tree, ".custom_checkpointing");

	if (!PC_status(custom_tree)) {
		each(custom_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
			std::string key = to_string(key_tree);
			if (key == "veloc_file") {
				m_custom.routed_file = to_string(value);
			} else if (key == "custom_checkpoint") {
				// parsed in step 4
			} else if (key == "custom_recover") {
				// parsed in step 5
			} else {
				throw Spectree_error{custom_tree, "VeloC config: unknown key `{}' in `custom_checkpointing', ignoring.", key};
			}
		});

		// Step 4
		PC_tree_t custom_cp_tree = PC_get(custom_tree, ".custom_checkpoint");
		if (!PC_status(custom_cp_tree)) {
			each(custom_cp_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
				std::string key = to_string(key_tree);
				if (key == "filename") {
					m_custom.manual_cp.original_file = to_string(value);
				} else if (key == "start_on_event") {
					load_events(m_events, ctx, value, Event_type::START_CHECKPOINT);
				} else if (key == "end_on_event") {
					load_events(m_events, ctx, value, Event_type::END_CHECKPOINT);
				} else if (key == "route_file_on_event") {
					load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_CP);
				} else {
					throw Spectree_error{custom_cp_tree, "VeloC config: unknown key `{}' in `custom_checkpoint', ignoring.", key};
				}
			});

			manual_cp().is_valid = validate_manual_op<ManualCheckpoint>(custom_cp_tree, m_events, manual_cp());
		}

		// Step 5
		PC_tree_t custom_rec_tree = PC_get(custom_tree, ".custom_recover");
		if (!PC_status(custom_rec_tree)) {
			each(custom_rec_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
				std::string key = to_string(key_tree);
				if (key == "filename") {
					m_custom.manual_rec.original_file = to_string(value);
				} else if (key == "start_on_event") {
					load_events(m_events, ctx, value, Event_type::START_RECOVERY);
				} else if (key == "end_on_event") {
					load_events(m_events, ctx, value, Event_type::END_RECOVERY);
				} else if (key == "route_file_on_event") {
					load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_REC);
				} else if (key == "recover_at_or_before_iteration") {
					m_custom.manual_rec.requested_checkpoint = to_long(value);
				} else {
					throw Spectree_error{custom_cp_tree, "VeloC config: unknown key `{}' in `custom_recover', ignoring.", key};
				}
			});

			manual_rec().is_valid = validate_manual_op<ManualRecovery>(custom_rec_tree, m_events, manual_rec());
		}

		m_custom.is_valid = validate_custom_config(ctx, custom_tree, m_custom, m_events, status_key_defined, manual_cp(), manual_rec());
	}

	check_conformity(ctx, status_key_defined);
}

void Veloc_cfg::check_conformity(Context& ctx, bool status_key_defined)
{
	// --- mandatory fields ---

	if (m_config_file.empty()) {
		throw Spectree_error{m_tree, "the path to VeloC configuration file is undefined"};
	}

	if (m_cp_label.empty()) {
		throw Spectree_error{m_tree, "the name of the checkpoint label is undefined"};
	}

	if (m_iter_name.empty()) {
		throw Spectree_error{m_tree, "the name of the iteration number in the PDI data store is undefined"};
	}

	// unavailable feature
	if (m_managed.is_valid && m_custom.is_valid) {
		throw Impl_error{"'managed_checkpointing ' and 'custom_checkpointing' "
		                 "cannot both be defined in the current version of the plugin. "};
	}

	// Warn : no checkpointing/recover behaviour defined
	if (!m_managed.is_valid && !m_custom.is_valid) {
		ctx.logger().warn("No checkpointing configuration has been defined");
	}
}
