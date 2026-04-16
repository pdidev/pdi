/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "config.h"

#include <cstdlib>
#include <functional>
#include <map>
#include <memory>
#include <vector>

#include <dlfcn.h>
#include <unistd.h>

#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/version.h"

#include "data_descriptor_impl.h"

#include "global_context.h"

#include <filesystem>
#include <fstream>
#include <stdexcept>
#include <unordered_set>

namespace fs = std::filesystem;

using std::exception;
using std::forward_as_tuple;
using std::map;
using std::pair;
using std::piecewise_construct;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

#if defined(PARACONF_VERSION)
#if (PARACONF_UNTYPED_VERSION >= PARACONF_UNTYPED_COMPUTE_VERSION(1, 1, 0))
#define PDI_HAS_PC_PATH 1
#else
#define PDI_HAS_PC_PATH 0
#endif
#else
#define PDI_HAS_PC_PATH 0
#endif

namespace PDI {

namespace {

void load_data(Global_context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);
	for (int map_id = 0; map_id < map_len; ++map_id) {
		PC_tree_t key_node = PC_get(node, "{%d}", map_id);
		Data_descriptor& dsc = ctx.make_and_check_descriptor(key_node); // Both defining a descriptor and checking for duplicate
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}
	ctx.logger().trace("Loaded {} {}", map_len, is_metadata ? "metadata" : "data");
}

} // namespace

unique_ptr<Global_context> Global_context::s_context;

void Global_context::init(PC_tree_t conf)
{
	s_context.reset(new Global_context(conf));
}

bool Global_context::initialized()
{
	return static_cast<bool>(s_context);
}

Global_context& Global_context::context()
{
	if (!s_context) throw State_error{"PDI not initialized"};
	return *s_context;
}

void Global_context::finalize()
{
	s_context.reset();
}

Global_context::Global_context(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_plugins{*this, conf}
	, m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);

	load_pdi_config(conf); // single tree traversal + two flat sub-passes (one for the plugins, one for the types/metadata/data)

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(*this);

	m_callbacks.call_init_callbacks();
	m_logger.info("Initialization successful");
}

Data_descriptor& Global_context::desc(const char* name)
{
	return *(m_descriptors.emplace(name, unique_ptr<Data_descriptor>{new Data_descriptor_impl{*this, name}}).first->second);
}

Data_descriptor& Global_context::desc(const string& name)
{
	return desc(name.c_str());
}

Data_descriptor& Global_context::operator[] (const char* name)
{
	return desc(name);
}

Data_descriptor& Global_context::operator[] (const string& name)
{
	return desc(name.c_str());
}

Global_context::Iterator Global_context::begin()
{
	return Context::get_iterator(m_descriptors.begin());
}

Global_context::Iterator Global_context::end()
{
	return Context::get_iterator(m_descriptors.end());
}

PDI::Context::Iterator Global_context::find(const string& name)
{
	return Context::get_iterator(m_descriptors.find(name));
}

void Global_context::event(const char* name)
{
	m_callbacks.call_event_callbacks(name);
}

Logger& Global_context::logger()
{
	return m_logger;
}

Datatype_template_sptr Global_context::datatype(PC_tree_t node)
{
	string type;
	try {
		type = to_string(PC_get(node, ".type"));
	} catch (const Error& e) {
		type = to_string(node);
	}

	// check if someone didn't mean to create an array with the old syntax
	if (type != "array") {
		if (!PC_status(PC_get(node, ".size"))) {
			logger().warn("In line {}: Non-array type with a `size' property", node.node->start_mark.line);
		}
		if (!PC_status(PC_get(node, ".sizes"))) {
			logger().warn("In line {}: Non-array type with a `sizes' property", node.node->start_mark.line);
		}
	}

	auto&& func_it = m_datatype_parsers.find(type);
	if (func_it != m_datatype_parsers.end()) {
		return (func_it->second)(*this, node);
	}
	throw Config_error{node, "Unknown data type: `{}'", type};
}

void Global_context::add_datatype(const string& name, Datatype_template_parser parser)
{
	if (!m_datatype_parsers.emplace(name, move(parser)).second) {
		//if a datatype with the given name already exists
		throw Type_error{"Datatype already defined `{}'", name};
	}
}

Callbacks& Global_context::callbacks()
{
	return m_callbacks;
}

void Global_context::collect_ordered_nodes(
	PC_tree_t conf,
	std::unordered_set<std::string>& globally_loaded,
	std::unordered_set<std::string>& include_chain,
	std::vector<std::pair<std::string, PC_tree_t>>& ordered_nodes,
	const std::string& known_path,
	PC_tree_t include_directive,
	const std::string& parent_id
)
{
	std::size_t no_path_counter = 0;
	collect_ordered_nodes_impl(conf, globally_loaded, include_chain, ordered_nodes, no_path_counter, known_path, include_directive, parent_id);
}

void Global_context::collect_ordered_nodes_impl(
	PC_tree_t conf,
	std::unordered_set<std::string>& globally_loaded, ///< all files loaded so far across all branches, to detect diamond includes
	std::unordered_set<std::string>& include_chain, ///< files currently open in the active include branch, to detect circular includes
	std::vector<std::pair<std::string, PC_tree_t>>& ordered_nodes,
	std::size_t& no_path_counter, // to be sure that we do not reuse a unique counter/id, in the case we do not have paths
	const std::string& known_path,
	PC_tree_t include_directive, // exact line
	const std::string& parent_id // includer id
)
{
	// ── Step 1: identity ─────────────────────────────────────────────────────
	std::string current_id;
	bool has_real_path = false;

	if (!known_path.empty()) {
		current_id = known_path;
		has_real_path = true;
	}
#if PDI_HAS_PC_PATH
	else
	{
		const char* raw_path = PC_path(conf);
		if (raw_path && fs::exists(raw_path)) {
			current_id = fs::canonical(raw_path).string();
			has_real_path = true;
		}
	}
#endif
	if (!has_real_path) {
		current_id = "<no-path:" + std::to_string(no_path_counter++) + ">";
	}

	// ── Step 2: circular detection ───────────────────────────────────────────
	if (has_real_path && include_chain.count(current_id)) {
		if (!parent_id.empty()) {
			throw Config_error(
				include_directive,
				"Circular include detected: '{}' is already being loaded (included from '{}')",
				current_id,
				parent_id
			);
		} else {
			throw Config_error(include_directive, "Circular include detected: '{}' is already being loaded", current_id);
		}
	}

	// ── Step 3: diamond detection ────────────────────────────────────────────
	if (globally_loaded.count(current_id)) {
		if (!parent_id.empty()) {
			m_logger.warn("Diamond include: '{}' has already been loaded, skipping (included again from '{}')", current_id, parent_id);
		} else {
			m_logger.warn("Diamond include: '{}' has already been loaded, skipping", current_id);
		}
		return;
	}

	include_chain.insert(current_id);
	globally_loaded.insert(current_id);

	// ── Step 4: recurse into includes (post-order: children before parent) ───
	PC_tree_t includes = PC_get(conf, ".include");
	if (!PC_status(includes)) {
		PDI::each(includes, [&](PC_tree_t node) {
			// `node` is the scalar YAML node of the include entry
			// Config_error(node, ...) gives the exact line of the offending directive
			const std::string inc = PDI::to_string(node);
			const bool is_absolute = fs::path(inc).is_absolute();

#if PDI_HAS_PC_PATH
			if (!is_absolute && !has_real_path) {
				throw Config_error(
					node,
					"Relative include '{}' cannot be resolved: "
					"the root config was parsed from a string (no file path available)",
					inc
				);
			}
			fs::path full_path = is_absolute ? fs::path(inc) : fs::path(fs::path(current_id).parent_path()) / inc;
#else
			if (!is_absolute) {
				throw Config_error(node,
					"Relative include '{}' is not supported: "
					"Paraconf >= 1.1 is required for relative path resolution",
					inc
				);
			}
			fs::path full_path = fs::path(inc);
#endif

			if (!fs::exists(full_path)) {
				// has_real_path tells us whether we can name the includer file
				if (has_real_path) {
					throw Config_error(node, "Included file not found: '{}' (included from '{}')", full_path.string(), current_id);
				} else {
					throw Config_error(node, "Included file not found: '{}'", full_path.string());
				}
			}
			full_path = fs::canonical(full_path);

			PC_tree_t sub = PC_parse_path(full_path.string().c_str());
			if (PC_status(sub) != PC_OK) {
				throw Config_error(node, "Failed to parse included file: '{}'", full_path.string());
			}

			collect_ordered_nodes(sub, globally_loaded, include_chain, ordered_nodes, full_path.string(), node, current_id);
		});
	}

	// ── Step 5: append this node after its children (post-order) ────────────
	ordered_nodes.emplace_back(current_id, conf);
	include_chain.erase(current_id);
}

void Global_context::finalize_and_exit()
{
	Global_context::finalize();
	exit(0);
}

void Global_context::load_pdi_config(PC_tree_t conf)
{
	std::unordered_set<std::string> globally_loaded;
	std::unordered_set<std::string> include_chain;
	std::vector<std::pair<std::string, PC_tree_t>> ordered_nodes;
	collect_ordered_nodes(conf, globally_loaded, include_chain, ordered_nodes);

	// Sub-pass A: register + load plugins so custom types (e.g. MPI_Comm)
	// are available before metadata/data are parsed.
	for (auto& [id, node]: ordered_nodes)
		m_plugins.register_plugins(node);
	m_plugins.load_plugins();

	// Sub-pass B: types, metadata, data, in include order.
	for (auto& [id, node]: ordered_nodes) {
		Datatype_template::load_user_datatypes(*this, PC_get(node, ".types"));
		if (auto m = PC_get(node, ".metadata"); !PC_status(m)) load_data(*this, m, true);
		if (auto d = PC_get(node, ".data"); !PC_status(d)) load_data(*this, d, false);
	}
}

Data_descriptor& Global_context::make_and_check_descriptor(PC_tree_t key_node)
{
	std::string descriptor_name = to_string(key_node);

	auto [descriptor_entry, is_new_entry] = m_descriptors.emplace(
		descriptor_name,
		std::unique_ptr<Data_descriptor>(new Data_descriptor_impl(*this, descriptor_name.c_str(), key_node))
	);

	if (!is_new_entry) {
		// first_definition_node: from the deepest included file (first encountered in post-order)
		PC_tree_t first_definition_node = static_cast<Data_descriptor_impl&>(*descriptor_entry->second).m_source_node;

		// redefinition_node: from the parent/root config (encountered later in post-order)
		PC_tree_t redefinition_node = key_node;

#if PDI_HAS_PC_PATH
		const char* first_definition_path = PC_path(first_definition_node);
		const char* redefinition_path = PC_path(redefinition_node);
		bool first_has_path = first_definition_path && fs::exists(first_definition_path);
		bool redef_has_path = redefinition_path && fs::exists(redefinition_path);

		if (first_has_path && redef_has_path) {
			throw Config_error(
				redefinition_node,
				"Duplicate definition of '{}', first defined in '{}', then redefined in '{}'",
				descriptor_name,
				redefinition_path, // root/parent file = first from user's perspective
				first_definition_path // deepest included file = redefinition from user's perspective
			);
		} else if (first_has_path) {
			throw Config_error(
				redefinition_node,
				"Duplicate definition of '{}', first defined in a string config, then redefined in '{}'",
				descriptor_name,
				first_definition_path
			);
		} else if (redef_has_path) {
			throw Config_error(
				redefinition_node,
				"Duplicate definition of '{}', first defined in '{}', then redefined in a string config",
				descriptor_name,
				redefinition_path
			);
		}
#endif
		throw Config_error(redefinition_node, "Duplicate definition of '{}'", descriptor_name);
	}

	return *descriptor_entry->second;
}

Global_context::~Global_context()
{
	m_logger.info("Finalization");
}

} // namespace PDI
