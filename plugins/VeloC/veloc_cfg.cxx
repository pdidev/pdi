/*******************************************************************************
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

// saves in m_events the name of events of different types 
bool load_events(unordered_map<string, Event_type>& events, Context& ctx, PC_tree_t tree, Event_type event_type, 
    function<void(const string&)> on_load_func = function<void(const string&)>())
{
	const map<Event_type, string> event_names = {
		{Event_type::CHECKPOINT, "checkpoint_on"},
		{Event_type::RECOVER,       "recover_on"},
		{Event_type::RECOVER_VAR,   "recover_var"},
		{Event_type::STATE_SYNC,     "synchronize_on"},
        {Event_type::START_CHECKPOINT,  "start_cp_on"},
        {Event_type::END_CHECKPOINT,  "end_cp_on"},
        {Event_type::ROUTE_FILE_FOR_CP,  "route_file_for_cp_on"},
        {Event_type::ROUTE_FILE_FOR_REC,  "route_file_for_rec_on"},
        {Event_type::START_RECOVERY,  "start_rec_on"},
        {Event_type::END_RECOVERY,  "end_rec_on"}
	};
	
	bool inserted = false;
    if (!PC_status(PC_get(tree, "[0]"))){ // if it is a list 
        std::cout << "list of events " << std::endl;
        each(tree, [&](PC_tree_t subtree) {
            auto&& result = events.emplace(to_string(subtree), event_type);
            if (result.second) {
                inserted = true;
                if (on_load_func) on_load_func(result.first->first);
            } else {
                ctx.logger().warn("Duplicate event name `{}' in `{}' (previously defined in `{}')",
                    result.first->first,
                    event_names.at(event_type),
                    event_names.at(result.first->second));
            }
        });
    }

    else{
        auto&& result = events.emplace(to_string(tree), event_type);
        if (result.second) {
            inserted = true;
            if (on_load_func) on_load_func(result.first->first);
        } 
        else{
            ctx.logger().warn("Duplicate event name `{}' in `{}' (previously defined in `{}')",
            result.first->first,
            event_names.at(event_type),
            event_names.at(result.first->second));
        }
    }
	return inserted;
}

// saves in m_desc the name in PDI store for descriptors like STATUS anf CP COUNTER 
bool load_desc(unordered_map<string, Desc_type>& descs, Context& ctx, const string& name, Desc_type desc_type)
{
	const map<Desc_type, string> desc_names = {
		{Desc_type::STATUS,       "status"},
		{Desc_type::COUNTER_CP,    "counter"},
	};
	// inserts the element and returns a pair: first (iterator to the element) 
	// and second (bool if insertion successful or if key already existed)
	auto&& result = descs.emplace(name, desc_type); 
	if (!result.second) {
		ctx.logger().warn("Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')", 
            name, desc_names.at(desc_type), desc_names.at(result.first->second));
	}
	return result.second;
}

// takes the ".var" entries of the recover_var tree 

set<int> load_vars(Context& ctx, PC_tree_t tree, std::unordered_map<int, std::string> protected_data)
{
    // Build a reverse lookup: name -> id
    unordered_map<string, int> name_to_id;
    for (auto& [id, name] : protected_data) {
        name_to_id[name] = id;
    }

    // for each var name, checks if present in protected_data. Is this necessary?
    // if so, it adds the variable index to an unordered set
    // which then in the constructor is used as a value of a key-value map where the key is the event name 
    set<int> vars;
    if (!PC_status(PC_get(tree, "[0]"))){ //list 
        each(tree, [&](PC_tree_t subtree) -> void {
            string var_name = to_string(subtree);
            auto it = name_to_id.find(var_name);
            if (it == name_to_id.end()) {
                ctx.logger().warn("Unknown variable name `{}' in `recover_var'", var_name);
                return ;
            }
            auto&& result = vars.emplace(it->second);
            if (!result.second) {
                ctx.logger().warn("Duplicate variable name `{}' in `recover_var'", var_name);
            }
        });
    }
    else{ //single value 
        string var_name = to_string(tree);
        auto it = name_to_id.find(var_name);
        if (it == name_to_id.end()) {
            ctx.logger().warn("Unknown variable name `{}' in `recover_var'", var_name);
            return set<int> {};
        }
        auto&& result = vars.emplace(it->second);
        if (!result.second) {
            ctx.logger().warn("Duplicate variable name `{}' in `recover_var'", var_name);
        }
    }

    return vars;
}
} // namespace <anonymous>

Veloc_cfg::Veloc_cfg(Context& ctx, PC_tree_t tree) : m_failure{-1}, 
manual_cp_defined{false}, manual_rec_defined{false}, m_requested_checkpoint{-1}
{
    //pass 1 
    each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
        
        string key = to_string(key_tree);

        if (key == "config_file") {
            m_config_file = to_string(value);
        } 
        else if (key == "failure") {
            m_failure = to_long(value);
        } 
        else if (key == "checkpoint_label") {
            m_cp_label = to_string(value);
        }
        else if(key == "checkpoint_nr"){ 
            m_requested_checkpoint = to_long(value);
        }
        else if (key == "when"){ 
            m_when = to_string(value);
        }
        else if (key == "status") {
            string status_name = to_string(value);
            load_desc(m_descs, ctx, status_name, Desc_type::STATUS); 
        } 
        else if (key == "counter") {
            string counter_name = to_string(value);
            load_desc(m_descs, ctx, counter_name, Desc_type::COUNTER_CP); 
        } 
        else if(key == "iteration"){
            m_iter_name = to_string(value);
        }
        else if (key == "synchronize_on") {
            load_events(m_events, ctx, value, Event_type::STATE_SYNC);
        } 
        else if (key == "recover_on") {
            load_events(m_events, ctx, value, Event_type::RECOVER);
        }
        else if(key == "checkpoint_on"){ 
            load_events(m_events, ctx, value, Event_type::CHECKPOINT);
        }
        else if (key == "protect_data") {
            // parsed in pass 2 
        }
        else if (key == "recover_var"){
            // parsed in pass 3
        }
        else if (key == "manual_checkpoint"){
            // parsed in pass 4
        }
        else if (key == "manual_recovery"){
            // parsed in pass 5
        }
        else {
            throw Config_error{tree, "Unknown key in VeloC plugin configuration: `{}'", key};
        }
    });

    // pass 2
    PC_tree_t protect_tree = PC_get(tree, ".protect_data");
    if (!PC_status(protect_tree)) {                                      
        if (!PC_status(PC_get(protect_tree, "[0]"))) {
            int data_id = 0;
            each(protect_tree, [&](PC_tree_t value) {
                string data_name = to_string(value);
                auto&& result = m_protected_data.emplace(data_id, data_name);
                if (!result.second) {
                    ctx.logger().warn("Duplicate data id (`{}')", data_id);
                }
                data_id++;
            });
        }
    }

    // pass 3
    PC_tree_t recover_var_tree = PC_get(tree, ".recover_var");
    if (!PC_status(recover_var_tree)) {
        if (!PC_status(PC_get(recover_var_tree, "[0]"))) {
            // list of recover_var entries
            each(recover_var_tree, [&](PC_tree_t recover_var_subtree) {
                PC_tree_t var_tree = PC_get(recover_var_subtree, ".var");
                if (PC_status(var_tree)) {                               // <-- guard: .var may be absent
                    ctx.logger().warn("VeloC config: `recover_var' entry missing `.var', skipping.");
                    return;
                }
                auto&& vars = load_vars(ctx, var_tree, m_protected_data);

                PC_tree_t event_tree = PC_get(recover_var_subtree, ".on_event");
                if (PC_status(event_tree)) {                             // <-- guard: .on_event may be absent
                    ctx.logger().warn("VeloC config: `recover_var' entry missing `.on_event', skipping.");
                    return;
                }
                load_events(m_events, ctx, event_tree, Event_type::RECOVER_VAR, [this, vars](const string& event_name) {
                    m_recover_var.emplace(event_name, vars);
                });
            });
        }
        else {
            // single recover_var entry
            PC_tree_t var_tree = PC_get(recover_var_tree, ".var");
            if (PC_status(var_tree)) {                                   // <-- guard: .var may be absent
                ctx.logger().warn("VeloC config: `recover_var' missing `.var', skipping.");
            }
            else {
                auto&& vars = load_vars(ctx, var_tree, m_protected_data);

                PC_tree_t event_tree = PC_get(recover_var_tree, ".on_event");
                if (PC_status(event_tree)) {                             // <-- guard: .on_event may be absent
                    ctx.logger().warn("VeloC config: `recover_var' missing `.on_event', skipping.");
                }
                else {
                    load_events(m_events, ctx, event_tree, Event_type::RECOVER_VAR, [this, vars](const string& event_name) {
                        m_recover_var.emplace(event_name, vars);
                    });
                }
            }
        }
    }

    // pass 4
    PC_tree_t manual_cp_tree = PC_get(tree, ".manual_checkpoint");
    if (!PC_status(manual_cp_tree)) {
        manual_cp_defined = true; 
        each(manual_cp_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
            string key = to_string(key_tree);

            if (key == "original_file") {
                m_manual_cp.original_file = to_string(value);
            }
            else if (key == "veloc_file") {
                m_manual_cp.routed_file = to_string(value);
            }
            else if (key == "start_on") {
                load_events(m_events, ctx, value, Event_type::START_CHECKPOINT);
            }
            else if (key == "end_on") {
                load_events(m_events, ctx, value, Event_type::END_CHECKPOINT);
            }
            else if (key == "route_file_on") {
                load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_CP);
            }
            else {
                ctx.logger().warn("VeloC config: unknown key `{}' in `manual_checkpoint', ignoring.", key);
            }
        });
    }

    // pass 5
    PC_tree_t manual_rec_tree = PC_get(tree, ".manual_recovery");
    if (!PC_status(manual_rec_tree)) {
        manual_rec_defined = true; 
        each(manual_rec_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
            string key = to_string(key_tree);

            if (key == "original_file") {
                m_manual_rec.original_file = to_string(value);
            }
            else if (key == "veloc_file") {
                m_manual_rec.routed_file = to_string(value);
            }
            else if (key == "start_on") {
                load_events(m_events, ctx, value, Event_type::START_RECOVERY);
            }
            else if (key == "end_on") {
                load_events(m_events, ctx, value, Event_type::END_RECOVERY);
            }
            else if (key == "route_file_on") {
                load_events(m_events, ctx, value, Event_type::ROUTE_FILE_FOR_REC);
            }
            else {
                ctx.logger().warn("VeloC config: unknown key `{}' in `manual_recovery', ignoring.", key);
            }
        });
    }

    check_conformity(ctx);
}

void Veloc_cfg::check_conformity(Context& ctx){

    if(m_config_file.empty()){
         throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML : The VeloC configuration file is undefined"};
    }

    if(m_protected_data.size()==0){
        throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML: Data to be checkpointed/recovered must be defined"};
    }

    bool cp_events_defined = std::any_of(
        m_events.begin(), m_events.end(),
        [](const auto& pair) {
            return pair.second == Event_type::CHECKPOINT;
        }
    );

    bool rec_events_defined = std::any_of(
        m_events.begin(), m_events.end(),
        [](const auto& pair) {
            return pair.second == Event_type::RECOVER;
        }
    );
    bool sync_events_defined = std::any_of(
        m_events.begin(), m_events.end(),
        [](const auto& pair) {
            return pair.second == Event_type::STATE_SYNC;
        }
    );

    if(!cp_events_defined && !rec_events_defined && !sync_events_defined && 
        !manual_cp_defined && !manual_rec_defined){
            throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML: No events have been defined"};      
    }

    if (m_cp_label.empty()){
        throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML : The name of the checkpoint label must be defined"};
    }

    if(m_failure!=0 && m_failure!=1){
        throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML : The failure key must defined as equal to 1 or 0 "
            "depening if a failure occurred or not"};
    }

    if(m_iter_name.empty()){
        throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML: The name of the iteration number in the PDI data store must be defined"};
    }
    else if (std::find_if(m_protected_data.begin(), m_protected_data.end(),
        [this](const auto& p){ return p.second == this->m_iter_name; }) 
        == m_protected_data.end()) {  
        throw Error{PDI_ERR_CONFIG, 
            "VELOC PLUGIN YAML: The iteration number must be included in the data to be protected"};
    }

    if(m_requested_checkpoint)

    if(!rec_events_defined){
        if(m_requested_checkpoint!=-1){
            ctx.logger().warn("VELOC PLUGIN YAML : No recovery events have been defined. "
                "Ignoring 'checkpoint_nr' key\n");
        }
        else{
            ctx.logger().warn("VELOC PLUGIN YAML : No recovery events have been defined\n");
        }
    }

    if(!cp_events_defined){
        if(m_when){
            ctx.logger().warn("VELOC PLUGIN YAML : No checkpoints events have been defined. "
                "Ignoring 'when' key\n");
        }
        else{
            ctx.logger().warn("VELOC PLUGIN YAML : No checkpoints events have been defined\n");
        }
    }
    else{  
        if(!m_when){
            m_when = 1L; 
        }
    }

    if(m_failure==1 && !rec_events_defined && !sync_events_defined){
        ctx.logger().warn("VELOC PLUGIN YAML : The failure key has been set to 1 " 
            "but no recovery or synchronization events have been defined\n");
    }

    if(manual_cp_defined){

        bool start_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::START_CHECKPOINT;
            }
        );

        bool route_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::ROUTE_FILE_FOR_CP;
            }
        );

        bool end_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::END_CHECKPOINT;
            }
        );

        if(m_manual_cp.routed_file.empty()){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the name of the routed filename buffer in the PDI data store must be defined"};
        }
        if(!start_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to start a checkpoint phase must be defined "};
        }
        if(!route_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to route a filename for checkpointing must be defined "};
        }
        if(!end_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to end a checkpoint phase must be defined "};
        }
    }

   if(manual_rec_defined){

        bool start_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::START_RECOVERY;
            }
        );

        bool route_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::ROUTE_FILE_FOR_REC;
            }
        );

        bool end_event_defined = std::any_of(
            m_events.begin(), m_events.end(),
            [](const auto& pair) {
                return pair.second == Event_type::END_RECOVERY;
            }
        );

        if(m_manual_rec.routed_file.empty()){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the name of the routed filename buffer in the PDI data store must be defined"};
        }
        if(!start_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to start a recovery phase must be defined "};
        }
        if(!route_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to route a filename for recovery must be defined "};
        }
        if(!end_event_defined){
            throw Error{PDI_ERR_CONFIG, 
                "VELOC PLUGIN YAML: the event on which to end a recovery phase must be defined "};
        }
    }

}


