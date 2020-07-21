/*******************************************************************************
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

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include <spdlog/spdlog.h>

#include "fti_cfg.h"

using PDI::Context;
using PDI::Config_error;
using PDI::Expression;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using std::function;
using std::map;
using std::string;
using std::set;
using std::tuple;
using std::unordered_map;
using std::unordered_set;



namespace fti {

namespace {

bool load_events(unordered_map<string, Event_type>& events, Context& ctx, PC_tree_t tree, Event_type event_type, function<void(const string&)> on_load_func = function<void(const string&)>())
{
	const map<Event_type, string> event_names = {
		{Event_type::CHECKPOINT_L1, "checkpoint.L1_on"},
		{Event_type::CHECKPOINT_L2, "checkpoint.L2_on"},
		{Event_type::CHECKPOINT_L3, "checkpoint.L3_on"},
		{Event_type::CHECKPOINT_L4, "checkpoint.L4_on"},
		{Event_type::RECOVER,       "recover_on"},
		{Event_type::RECOVER_VAR,   "recover_var"},
		{Event_type::SNAPSHOT,      "snapshot_on"},
		{Event_type::SEND_FILE,     "send_file"},
		{Event_type::INIT,          "init_on"}
	};
	
	bool insterted = false;
	if (!PC_status(PC_get(tree, "[0]"))) {
		int nb_elems = len(tree);
		for (int elem_id = 0; elem_id < nb_elems; elem_id++) {
			auto&& result = events.emplace(to_string(PC_get(tree, "[%d]", elem_id)), event_type);
			if (result.second) {
				insterted = true;
				if (on_load_func) on_load_func(result.first->first);
			} else {
				ctx.logger()->warn("Duplicate event name `{}' in `{}' (previously defined in `{}')", result.first->first, event_names.at(event_type), event_names.at(result.first->second));
			}
		}
	} else {
		auto&& result = events.emplace(to_string(tree), event_type);
		if (result.second) {
			insterted = true;
			if (on_load_func) on_load_func(result.first->first);
		} else {
			ctx.logger()->warn("Duplicate event name `{}' in `{}' (previously defined in `{}')", result.first->first, event_names.at(event_type), event_names.at(result.first->second));
		}
	}
	return insterted;
}

bool load_desc(unordered_map<string, Desc_type>& descs, Context& ctx, const string& name, Desc_type desc_type)
{
	const map<Desc_type, string> desc_names = {
		{Desc_type::MPI_COMM,     "communicator"},
		{Desc_type::STATUS,       "status"},
		{Desc_type::DATA_SIZE,    "dataset"},
		{Desc_type::HEAD,         "head"},
		{Desc_type::STAGE_DIR,    "stage_dir"},
		{Desc_type::STAGE_STATUS, "file.status"}
	};
	
	auto&& result = descs.emplace(name, desc_type);
	if (!result.second) {
		ctx.logger()->warn("Duplicate use of a descriptor `{}' in `{}' (previously used in `{}')", name, desc_names.at(desc_type), desc_names.at(result.first->second));
	}
	return result.second;
}

unordered_set<int> load_vars(Context& ctx, PC_tree_t tree)
{
	unordered_set<int> vars;
	if (!PC_status(PC_get(tree, "[0]"))) {
		int nb_vars = len(tree);
		for (int var_id = 0; var_id < nb_vars; var_id++) {
			auto&& result = vars.emplace(to_long(PC_get(tree, "[%d]", var_id)));
			if (!result.second) {
				ctx.logger()->warn("Duplicate variable id `{}' in `recover_var'", *result.first);
			}
		}
	} else {
		vars.emplace(to_long(tree));
	}
	return vars;
}

set<tuple<Expression, Expression, string>> load_files(unordered_map<string, Desc_type>& descs, Context& ctx, PC_tree_t tree)
{
	set<tuple<Expression, Expression, string>> files;
	if (!PC_status(PC_get(tree, "[0]"))) {
		int nb_vars = len(tree);
		for (int var_id = 0; var_id < nb_vars; var_id++) {
			PC_tree_t subtree = PC_get(tree, "[%d]", var_id);
			PC_tree_t desc_tree = PC_get(subtree, ".status");
			string status;
			if (!PC_status(desc_tree)) {
				if (load_desc(descs, ctx, to_string(desc_tree), Desc_type::STAGE_STATUS)) {
					status = to_string(desc_tree);
				}
			}
			files.emplace(Expression{to_string(PC_get(subtree, ".src"))}, Expression{to_string(PC_get(subtree, ".dest"))}, status);
		}
	} else {
		PC_tree_t desc_tree = PC_get(tree, ".status");
		string status;
		if (!PC_status(desc_tree)) {
			if (load_desc(descs, ctx, to_string(desc_tree), Desc_type::STAGE_STATUS)) {
				status = to_string(desc_tree);
			}
		}
		files.emplace(Expression{to_string(PC_get(tree, ".src"))}, Expression{to_string(PC_get(tree, ".dest"))}, status);
	}
	return files;
}

} // namespace <anonymous>

Fti_cfg::Fti_cfg(Context& ctx, PC_tree_t tree):
	m_communicator{"MPI_COMM_WORLD"},
	m_init_on_event{false}
{
	int nb_key = len(tree);
	for (int key_id = 0; key_id < nb_key; key_id++) {
		string key = to_string(PC_get(tree, "{%d}", key_id));
		if (key == "config_file") {
			m_config_file = to_string(PC_get(tree, ".config_file"));
		} else if (key == "communicator") {
			m_communicator = to_string(PC_get(tree, ".communicator"));
			load_desc(m_descs, ctx, m_communicator, Desc_type::MPI_COMM);
		} else if (key == "status") {
			load_desc(m_descs, ctx, to_string(PC_get(tree, ".status")), Desc_type::STATUS);
		} else if (key == "init_on") {
			PC_tree_t init_tree = PC_get(tree, ".init_on");
			m_init_on_event = load_events(m_events, ctx, init_tree, Event_type::INIT);
		} else if (key == "dataset") {
			PC_tree_t dataset_tree = PC_get(tree, ".dataset");
			int nb_subkey = len(dataset_tree);
			for (int subkey_id = 0; subkey_id < nb_subkey; subkey_id++) {
				int dataset_id = to_long(PC_get(dataset_tree, "{%d}", subkey_id));
				PC_tree_t data_tree = PC_get(dataset_tree, "<%d>", subkey_id);
				if (!PC_status(PC_get(data_tree, "{0}"))) {
					auto&& result = m_dataset.emplace(dataset_id, to_string(PC_get(data_tree, ".name")));
					if (result.second) {
						PC_tree_t data_size_tree = PC_get(data_tree, ".size");
						if (!PC_status(data_size_tree)) {
							string size_desc = to_string(data_size_tree);
							if (load_desc(m_descs, ctx, size_desc, Desc_type::DATA_SIZE)) {
								m_dataset_sizes.emplace(size_desc, result.first->first);
							}
						}
					} else {
						ctx.logger()->warn("Duplicate dataset id (`{}')", dataset_id);
					}
				} else {
					auto&& result = m_dataset.emplace(dataset_id, to_string(data_tree));
					if (!result.second) {
						ctx.logger()->warn("Duplicate dataset id (`{}')", dataset_id);
					}
				}
			}
		} else if (key == "snapshot_on") {
			PC_tree_t snapshot_tree = PC_get(tree, ".snapshot_on");
			load_events(m_events, ctx, snapshot_tree, Event_type::SNAPSHOT);
		} else if (key == "recover_on") {
			PC_tree_t recover_tree = PC_get(tree, ".recover_on");
			load_events(m_events, ctx, recover_tree, Event_type::RECOVER);
		} else if (key == "checkpoint") {
			PC_tree_t checkpoint_tree = PC_get(tree, ".checkpoint");
			int nb_subkey = len(checkpoint_tree);
			for (int subkey_id = 0; subkey_id < nb_subkey; subkey_id++) {
				string subkey = to_string(PC_get(checkpoint_tree, "{%d}", subkey_id));
				if (subkey == "L1_on") {
					PC_tree_t ckpt_tree = PC_get(checkpoint_tree, ".L1_on");
					load_events(m_events, ctx, ckpt_tree, Event_type::CHECKPOINT_L1);
				} else if (subkey == "L2_on") {
					PC_tree_t ckpt_tree = PC_get(checkpoint_tree, ".L2_on");
					load_events(m_events, ctx, ckpt_tree, Event_type::CHECKPOINT_L2);
				} else if (subkey == "L3_on") {
					PC_tree_t ckpt_tree = PC_get(checkpoint_tree, ".L3_on");
					load_events(m_events, ctx, ckpt_tree, Event_type::CHECKPOINT_L3);
				} else if (subkey == "L4_on") {
					PC_tree_t ckpt_tree = PC_get(checkpoint_tree, ".L4_on");
					load_events(m_events, ctx, ckpt_tree, Event_type::CHECKPOINT_L4);
				} else {
					throw Config_error{"Unknown key for FTI checkpoint configuration: `%s'", subkey.c_str()};
				}
			}
		} else if (key == "recover_var") {
			PC_tree_t recover_var_tree = PC_get(tree, ".recover_var");
			if (!PC_status(PC_get(recover_var_tree, "[0]"))) {
				int nb_data = len(recover_var_tree);
				for (int data_id = 0; data_id < nb_data; data_id++) {
					PC_tree_t recover_var_subtree = PC_get(recover_var_tree, "[%d]", data_id);
					
					PC_tree_t var_tree = PC_get(recover_var_subtree, ".var");
					auto&& vars = load_vars(ctx, var_tree);
					PC_tree_t event_tree = PC_get(recover_var_subtree, ".on_event");
					load_events(m_events, ctx, event_tree, Event_type::RECOVER_VAR, [this, &vars](const string& event_name) {
						this->m_recover_var.emplace(event_name, vars);
					});
				}
			} else {
				PC_tree_t var_tree = PC_get(recover_var_tree, ".var");
				auto&& vars = load_vars(ctx, var_tree);
				
				PC_tree_t event_tree = PC_get(recover_var_tree, ".on_event");
				load_events(m_events, ctx, event_tree, Event_type::RECOVER_VAR, [this, &vars](const string& event_name) {
					this->m_recover_var.emplace(event_name, vars);
				});
			}
		} else if (key == "send_file") {
			throw Impl_error{"send_file not supported yet"}; //FTI bug
			PC_tree_t send_file_tree = PC_get(tree, ".send_file");
			if (!PC_status(PC_get(send_file_tree, "[0]"))) {
				int nb_data = len(send_file_tree);
				for (int data_id = 0; data_id < nb_data; data_id++) {
					PC_tree_t send_file_subtree = PC_get(send_file_tree, "[%d]", data_id);
					PC_tree_t files_tree = PC_get(send_file_subtree, ".file");
					auto&& files = load_files(m_descs, ctx, files_tree);
					PC_tree_t event_tree = PC_get(send_file_subtree, ".on_event");
					load_events(m_events, ctx, event_tree, Event_type::SEND_FILE, [this, &files] (const string& event_name) {
						this->m_send_file.emplace(event_name, files);
					});
				}
			} else {
				PC_tree_t files_tree = PC_get(send_file_tree, ".file");
				auto&& files = load_files(m_descs, ctx, files_tree);
				PC_tree_t event_tree = PC_get(send_file_tree, ".on_event");
				load_events(m_events, ctx, event_tree, Event_type::SEND_FILE, [this, &files] (const string& event_name) {
					this->m_send_file.emplace(event_name, files);
				});
			}
		} else if (key == "stage_dir") {
			load_desc(m_descs, ctx, to_string(PC_get(tree, ".stage_dir")), Desc_type::STAGE_DIR);
		} else if (key == "head") {
			load_desc(m_descs, ctx, to_string(PC_get(tree, ".head")), Desc_type::HEAD);
		} else {
			throw Config_error{"Unknown key for FTI configuration: `%s'", key.c_str()};
		}
	}
	if (!m_config_file) {
		throw Config_error{"Missing `config_file' key for FTI configuration"};
	}
}

string Fti_cfg::config(Context& ctx) const
{
	return m_config_file.to_string(ctx);
}

const string& Fti_cfg::communicator() const
{
	return m_communicator;
}

const unordered_map<int, string>& Fti_cfg::dataset() const
{
	return m_dataset;
}

const unordered_map<string, int>& Fti_cfg::dataset_sizes() const
{
	return m_dataset_sizes;
}

const unordered_map<string, Desc_type>& Fti_cfg::descs() const
{
	return m_descs;
}

const unordered_map<string, Event_type>& Fti_cfg::events() const
{
	return m_events;
}

bool Fti_cfg::init_on_event() const
{
	return m_init_on_event;
}

const unordered_map<string, unordered_set<int>>& Fti_cfg::recover_var() const
{
	return m_recover_var;
}

const unordered_map<string, set<tuple<Expression, Expression, string>>>& Fti_cfg::send_file() const
{
	return m_send_file;
}

} // namespace fti_plugin