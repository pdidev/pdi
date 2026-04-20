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
#include <pdi/paraconf_wrapper.h>
#include <veloc.h>

#include <map>
#include <set>
#include <tuple>
#include <unordered_map>
#include <unordered_set>
#include <iostream>

#include "veloc_cfg.h"

using PDI::Context;
using PDI::Error;
using PDI::Config_error;
using PDI::Expression;
using PDI::Impl_error;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using PDI::each;
using PDI::opt_each;

using std::function;
using std::map;
using std::string;
using std::set;
using std::tuple;
using std::unordered_map;
using std::unordered_set;


namespace {

    // Used to register event names of a given type into the events map (m_events)
    // TO DO : REFACTOR TO AVOID REPEATING CODE 
    bool load_events(unordered_map<string, Event_type>& events, Context& ctx, PC_tree_t tree, Event_type event_type,
        function<void(const string&)> on_load_func = function<void(const string&)>())
    {
        const map<Event_type, string> event_names = {
            {Event_type::CHECKPOINT,        "checkpoint_on"},
            {Event_type::RECOVER,           "recover_on"},
            {Event_type::STATE_SYNC,        "synchronize_on"},
            {Event_type::START_CHECKPOINT,  "start_cp_on"},
            {Event_type::END_CHECKPOINT,    "end_cp_on"},
            {Event_type::ROUTE_FILE_FOR_CP, "route_file_for_cp_on"},
            {Event_type::ROUTE_FILE_FOR_REC,"route_file_for_rec_on"},
            {Event_type::START_RECOVERY,    "start_rec_on"},
            {Event_type::END_RECOVERY,      "end_rec_on"}
        };

        bool inserted = false;
        if (!PC_status(PC_get(tree, "[0]"))) { // list
            each(tree, [&](PC_tree_t subtree) {
                auto&& result = events.emplace(to_string(subtree), event_type);
                if (result.second) {
                    inserted = true;
                    if (on_load_func) on_load_func(result.first->first);
                } else {
                    throw Error{PDI_ERR_CONFIG,
                        "Duplicate event name `{}' in `{}' (previously defined in `{}')",
                        result.first->first,
                        event_names.at(event_type),
                        event_names.at(result.first->second)};
                }
            });
        } else {
            auto&& result = events.emplace(to_string(tree), event_type);
            if (result.second) {
                inserted = true;
                if (on_load_func) on_load_func(result.first->first);
            } else {
                    throw Error{PDI_ERR_CONFIG,
                        "Duplicate event name `{}' in `{}' (previously defined in `{}')",
                        result.first->first,
                        event_names.at(event_type),
                        event_names.at(result.first->second)};
            }
        }
        return inserted;
    }


    
    // Used to register a descriptor name (status / counter) into the descs map.
    bool load_desc(unordered_map<string, Desc_type>& descs, Context& ctx, const string& name, Desc_type desc_type)
    {
        const map<Desc_type, string> desc_names = {
            {Desc_type::STATUS,    "status"},
            {Desc_type::COUNTER_CP,"counter"},
        };
        auto&& result = descs.emplace(name, desc_type);
        if (!result.second) {
            ctx.logger().warn("Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')",
                name, desc_names.at(desc_type), desc_names.at(result.first->second));
        }
        return result.second;
    }



    template<Event_type... RequiredEvents>
    bool validate_manual_op(const unordered_map<string, Event_type>& events, std::string original_file) {
        const map<Event_type, string> event_names = {
            {Event_type::START_CHECKPOINT,  "start_on"},
            {Event_type::END_CHECKPOINT,    "end_on"},
            {Event_type::ROUTE_FILE_FOR_CP, "route_file_on"},
            {Event_type::ROUTE_FILE_FOR_REC,"route_file_on"},
            {Event_type::START_RECOVERY,    "start_on"},
            {Event_type::END_RECOVERY,      "end_on"}
        };

        std::array<Event_type, sizeof...(RequiredEvents)> required{RequiredEvents...};

        for (auto event_type : required) {
            bool defined = std::any_of(events.begin(), events.end(),  // searches the actual events map
                [event_type](const auto& pair) { return pair.second == event_type; });

            if (!defined) {
                throw Error{PDI_ERR_CONFIG,
                    "VeloC Specification Tree: '{}' must be defined in manual checkpoint/recover",
                    event_names.at(event_type)};
            }
        }
        if(original_file.empty()){
             throw Error{PDI_ERR_CONFIG,
                    "VeloC Specification Tree: 'original_file' must be defined in manual checkpoint/recover"};
        }
        return true;
    }



    bool validate_custom_config(CustomCheckpointingCfg cfg){
        if(cfg.routed_file.empty()){
            throw Error{PDI_ERR_CONFIG, 
                "VeloC Specification Tree: 'veloc_file' must be defined in 'custom_checkpointing' "}; 
        }
        return true;
    }


    bool validate_managed_config(ManagedCheckpointingCfg cfg){
        if(cfg.protected_data.size()==0){
            throw Error{PDI_ERR_CONFIG, 
                "VeloC Specification Tree: 'protected data' must be defined in 'managed_checkpointing' "}; 
        }
        return true;
    }
} // anonymous namespace

Veloc_cfg::Veloc_cfg(Context& ctx, PC_tree_t tree) 
{
	//  STEP 1
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		string key = to_string(key_tree);

		if (key == "config_file") {
			m_config_file = to_string(value);
		} else if (key == "failure") {
			m_failure = to_long(value);
		} else if (key == "checkpoint_label") {
			m_cp_label = to_string(value);
		} else if (key == "iteration") {
			m_iter_name = to_string(value);
		} else if (key == "status") {
			load_desc(m_descs, ctx, to_string(value), Desc_type::STATUS);
		} else if (key == "counter") {
			load_desc(m_descs, ctx, to_string(value), Desc_type::COUNTER_CP);
		} else if (key == "managed_checkpointing") {
			// parsed in step 2
		} else if (key == "custom_checkpointing") {
			// parsed in step 3
		} else {
			throw Config_error{tree, "Unknown key in VeloC plugin configuration: `{}'", key};
		}
	});

	//  STEP 2
	PC_tree_t managed_tree = PC_get(tree, ".managed_checkpointing");
	if (!PC_status(managed_tree)) {
		each(managed_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
			string key = to_string(key_tree);

			if (key == "protect_data") {
				// list of data names; position in the list becomes the id
				if (!PC_status(PC_get(value, "[0]"))) {
					int data_id = 0;
					each(value, [&](PC_tree_t item) {
						string data_name = to_string(item);
						if (!m_managed.protected_data.emplace(data_id, data_name).second) {
							ctx.logger().warn("Duplicate data id (`{}')", data_id);
						}
						data_id++;
					});
				}
			} else if (key == "checkpoint_on") {
				load_events(m_events, ctx, value, Event_type::CHECKPOINT);
			} else if (key == "recover_on") {
				load_events(m_events, ctx, value, Event_type::RECOVER);
			} else if (key == "synchronize_on") {
				load_events(m_events, ctx, value, Event_type::STATE_SYNC);
			} else if (key == "when") {
				m_managed.when = to_string(value);
            }else if (key == "checkpoint_nr") {
				m_managed.requested_checkpoint = to_long(value);
			}else {
                throw Config_error{tree, "VeloC config: unknown key `{}' in `managed_checkpointing', ignoring.", key};
			}  
		});

        m_managed.is_valid = validate_managed_config(m_managed) ? true : false;
	}

	// Step 3 
	PC_tree_t custom_tree = PC_get(tree, ".custom_checkpointing");

    if (!PC_status(custom_tree)) {
        each(custom_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
            string key = to_string(key_tree);
            if (key == "veloc_file") {
                m_custom.routed_file = to_string(value);
            }
            else if(key == "custom_checkpoint"){
                // parsed in step 4 
            }
            else if( key == "custom_recover"){
                // parsed in step 5
            }
        });

        // Step 4 
        PC_tree_t manual_cp_tree = PC_get(custom_tree, ".custom_checkpoint");
        if (!PC_status(manual_cp_tree)) {
            each(manual_cp_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
                string key = to_string(key_tree);
                if (key == "original_file") {
                    m_custom.manual_cp.original_file = to_string(value);
                } else if (key == "start_on") {
                    load_events(m_events, ctx, value, Event_type::START_CHECKPOINT);
                } else if (key == "end_on") {
                    load_events(m_events, ctx, value, Event_type::END_CHECKPOINT);
                } else if (key == "route_file_on") {
                    load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_CP);
                } else {
                    ctx.logger().warn("VeloC config: unknown key `{}' in `custom_checkpoint', ignoring.", key);
                }
            });

            manual_cp().is_valid = validate_manual_op<Event_type::START_CHECKPOINT, 
                    Event_type::ROUTE_FILE_FOR_CP, Event_type::END_CHECKPOINT>(m_events, manual_cp().original_file) ? true : false;
        }

        // Step 5
        PC_tree_t manual_rec_tree = PC_get(custom_tree, ".custom_recover");
        if (!PC_status(manual_rec_tree)) {
            each(manual_rec_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
                string key = to_string(key_tree);
                if (key == "original_file") {
                    m_custom.manual_rec.original_file = to_string(value);
                } else if (key == "start_on") {
                    load_events(m_events, ctx, value, Event_type::START_RECOVERY);
                } else if (key == "end_on") {
                    load_events(m_events, ctx, value, Event_type::END_RECOVERY);
                } else if (key == "route_file_on") {
                    load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_REC);
                }else if (key == "checkpoint_nr") {
                    m_custom.manual_rec.requested_checkpoint = to_long(value);
                } else {
                    ctx.logger().warn("VeloC config: unknown key `{}' in `custom_recover', ignoring.", key);
                }
            });
            manual_rec().is_valid = validate_manual_op<Event_type::START_RECOVERY, 
                    Event_type::ROUTE_FILE_FOR_REC, Event_type::END_RECOVERY>(m_events,manual_rec().original_file) ? true : false;
        }

        m_custom.is_valid = validate_custom_config(m_custom) ? true : false;

    }

	check_conformity(ctx);
}


// ---------------------------------------------------------------------------
// check_conformity
// ---------------------------------------------------------------------------

void Veloc_cfg::check_conformity(Context& ctx)
{
	// --- mandatory fields ---

	if (m_config_file.empty()) {
		throw Error{PDI_ERR_CONFIG,
			"VeloC Plugin Spectree: The VeloC configuration file is undefined"};
	}

	if (m_cp_label.empty()) {
		throw Error{PDI_ERR_CONFIG,
			"VeloC Plugin Spectree: The name of the checkpoint label must be defined"};
	}

	if (m_iter_name.empty()) {
		throw Error{PDI_ERR_CONFIG,
			"VeloC Plugin Spectree: The name of the iteration number in the PDI data store must be defined"};
	}

	if (m_failure != 0 && m_failure != 1) {
		throw Error{PDI_ERR_CONFIG,
			"VeloC Plugin Spectree: The `failure' key must be 0 or 1"};
	}

	if (!m_managed.is_valid && !m_custom.is_valid) {
		ctx.logger().warn("VeloC plugin Spectree: no checkpointing configuration has been defined");
	}

    // user must choose between a managed and custom configuration
    if(m_managed.is_valid && m_custom.is_valid){
        throw Error{PDI_ERR_CONFIG,
				"VeloC plugin Spectree: 'managed_checkpointing ' and 'custom_checkpointing' "
                "cannot both be defined "};
    }

    /* ---------------------------------------------
	    managed_checkpointing 
    ------------------------------------------------ */

    if (m_managed.is_valid) {

        // iteration must be in protect_data
		bool iter_protected = std::any_of(
			m_managed.protected_data.begin(), m_managed.protected_data.end(),
			[this](const auto& p) { return p.second == m_iter_name; });
		if (!iter_protected) {
			throw Error{PDI_ERR_CONFIG,
				"VeloC plugin Spectree: The iteration variable `{}' must be included in "
				"`protect_data'", m_iter_name};
		}

		bool cp_events_defined = std::any_of(m_events.begin(), m_events.end(),
			[](const auto& p) { return p.second == Event_type::CHECKPOINT; });
		bool rec_events_defined = std::any_of(m_events.begin(), m_events.end(),
			[](const auto& p) { return p.second == Event_type::RECOVER; });
		bool sync_events_defined = std::any_of(m_events.begin(), m_events.end(),
			[](const auto& p) { return p.second == Event_type::STATE_SYNC; });

		// Warn : nothing useful was configured inside the block
		if (!cp_events_defined && !rec_events_defined && !sync_events_defined) {
			ctx.logger().warn("VeloC Plugin Spectree: no checkpoint, recovery, or synchronization events have"
				 "been configured inside `managed_checkpointing'");
		}

		// Warn : "when" key defined without checkpoint events 
		if (!cp_events_defined) {
			if (m_managed.when != 1L ) {
				ctx.logger().warn("VeloC Plugin Spectree: No checkpoint events have been defined "
					"inside `managed_checkpointing'. Ignoring `when' key");
			} 
		} 

		// Warn : "checkpoint_nr" defined without recovery events
		if (!rec_events_defined && m_managed.requested_checkpoint != -1) {
            ctx.logger().warn("VeloC Plugin Spectree: No recovery events have been defined "
                "inside `managed_checkpointing'. Ignoring `checkpoint_nr' key");
		}

		// Warn : failure=1 without recovery/sync events 
		if (m_failure == 1 && !rec_events_defined && !sync_events_defined) {
			ctx.logger().warn("VeloC Plugin Spectree: `failure' is set to 1 but no recovery or "
				"synchronization events have been defined inside `managed_checkpointing'");
		}
    }

    /* ---------------------------------------------
	    custom_checkpointing 
    ------------------------------------------------ */
	if (m_custom.is_valid) {
		if (!m_custom.manual_cp.is_valid && !m_custom.manual_rec.is_valid) {
			ctx.logger().warn("VeloC Plugin Spectree: `custom_checkpointing' is defined but "
				"neither `custom_checkpoint' nor `custom_recover' have been configured");
		}
	}
}