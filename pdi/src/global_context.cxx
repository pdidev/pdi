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
#include <filesystem>
#include <fstream>
#include <functional>
#include <map>
#include <memory>
#include <queue>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
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

namespace fs = std::filesystem;

using std::cref;
using std::exception;
using std::forward_as_tuple;
using std::function;
using std::map;
using std::pair;
using std::piecewise_construct;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

namespace PDI {

namespace {

/** A class that represents a path to include a yaml subtree, i.e. both the path of the file itself and the subtree in
 * the file.
 */
class Include_path
{
	/// The path of the file
	fs::path m_file_path;

	/// The path of the subtree inside the file
	std::string m_ypath;

public:
	/** Builds an Include_path from a PC_tree_t
	 * \param include_directive either a scalar file path or a mapping with `file` and `subtree` keys
	 */
	Include_path(PC_tree_t include_directive)
	{
		if (is_map(include_directive)) {
			bool found_file = false;
			bool found_subtree = false;
			each(include_directive, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
				std::string key = PDI::to_string(key_tree);
				std::string value = PDI::to_string(value_tree);
				if (key == "file") {
					m_file_path = value;
					found_file = true;
				} else if (key == "subtree") {
					m_ypath = value;
					found_subtree = true;
				} else {
					throw Spectree_error(key_tree, "unexpected key in include directive: `{}', only `file' and `subtree' allowed", key);
				}
			});
			if (!found_file) {
				throw Spectree_error(include_directive, "missing `'file` key in include directive");
			}
			if (!found_subtree) {
				throw Spectree_error(include_directive, "missing `'subtree` key in include directive");
			}
		} else {
			m_file_path = PDI::to_string(include_directive);
		}
	}

	/** Returns the path of the file
	 * \returns the path of the file
	 */
	const fs::path& file_path() const { return m_file_path; }

	/** Returns the path of the subtree inside the file
	 * \returns the path of the subtree inside the file
	 */
	const std::string& ypath() const { return m_ypath; }

	// define the operator '<', '=' and '>'
	auto operator<=> (const Include_path&) const = default;

	/** Converts the path to a string representation
	 * \returns a string representation of the path
	 */
	std::string to_string() const { return fmt::format("yaml://{}{}{}", file_path().string(), ((ypath() != "") ? "#" : ""), ypath()); }

	/** Loads the subtree identified by this path
	*/
	PC_tree_t pc_tree() const
	{
		PC_tree_t result = PC_parse_path(file_path().string().c_str());
		if (PC_status(result)) {
			throw System_error("Unable to include file `{}': {}", file_path().string(), PC_errmsg());
		}
		result = PC_get(result, ypath().c_str());
		if (PC_status(result)) {
			throw System_error("Unable to include subtree `{}' from file `{}': {}", ypath(), file_path().string(), PC_errmsg());
		}
		return result;
	}
};


} // namespace
} // namespace PDI

namespace std {
template <>
struct hash<PDI::Include_path> {
	std::size_t operator() (const PDI::Include_path& path) const
	{
		// Computes the hash of an inc_path using boost strategy
		std::size_t result = std::hash<fs::path>()(path.file_path());
		result ^= std::hash<std::string>()(path.ypath()) + 0x9e3779b9 + (result << 6) + (result >> 2);
		return result;
	}
};
} // namespace std

namespace PDI {
namespace {

/** Gather the files included by the provided configuration.
 * 
 * The result is as an ordered list where elements at the end of the list can depend on those coming before.
 * 
 * \param logger a logger
 * \param conf the configuration where to look for included files
 * \param parents the set of subtree path that are in the include chain of conf (including conf)
 * \param result_path the path of all files already in result
 * \param result the ordered list of (transitively) included files to which conf and its requirements will be added
 */
void get_includes(
	Logger& logger,
	PC_tree_t conf,
	std::unordered_set<Include_path>& parents,
	std::unordered_set<Include_path>& result_path,
	std::vector<PC_tree_t>& result
)
{
	PC_tree_t inc_tree = PC_get(conf, ".include");
	if (!PC_status(inc_tree)) {
		opt_each(inc_tree, [&](PC_tree_t include_directive) {
			Include_path subconf_path{include_directive};
			if (parents.contains(subconf_path)) {
				// if we are in the include chain, this is a recursive include and an error
				throw Spectree_error(include_directive, "Circular include of `({}){}'", subconf_path.file_path().string(), subconf_path.ypath());
			}
			if (result_path.contains(subconf_path)) return; // if we were already included, nothing to do
			parents.emplace(subconf_path);
			try {
				logger.trace("Including {}", subconf_path.to_string());
				get_includes(logger, subconf_path.pc_tree(), parents, result_path, result);
			} catch (const Spectree_error& e) {
				rethrow_with_context(std::current_exception(), "included from ({}){}", subconf_path.file_path().string(), subconf_path.ypath());
			}
			parents.erase(subconf_path);
			result_path.emplace(subconf_path);
		});
	}
	result.emplace_back(conf);
}

/** Gather the files included by the provided configuration.
 * 
 * Returns the result as an ordered list where elements at the end of the list can depend on those coming before.
 * 
 * \param logger a logger
 * \param conf the configuration where to look for included files
 *
 * \returns the ordered list of (transitively) included confs, including `conf`
 */
std::vector<PC_tree_t> get_includes(Logger& logger, PC_tree_t conf)
{
	std::unordered_set<Include_path> parents;
	std::unordered_set<Include_path> result_path;
	std::vector<PC_tree_t> result;
	get_includes(logger, conf, parents, result_path, result);
	return result;
}

/** Loads the data (or metadata) from a yaml tree
 * \param ctx the context in which to load
 * \param node the tree from where to load
 * \param is_metadata whether this is a metadata subtree instead of a data one
 * \param def_location the location of all loaded data/metadata for duplicate detection
 */
void load_data(Context& ctx, PC_tree_t node, bool is_metadata, std::map<std::string, std::optional<Yaml_region>>& def_location)
{
	int nb_desc = 0;
	each(node, [&](PC_tree_t key_node, PC_tree_t value_node) {
		auto&& [location_it, is_new] = def_location.emplace(to_string(key_node), Yaml_region::make(value_node));
		auto&& [data_name, region] = *location_it;
		if (!is_new) {
			throw Spectree_error(
				key_node,
				"redefinition of '{}'{}{}",
				data_name,
				(region ? " previously defined in `" : ""),
				to_string(region),
				(region ? "'" : "")
			);
		}
		auto&& descriptor = ctx[data_name];
		descriptor.metadata(is_metadata);
		descriptor.default_type(ctx.datatype(value_node));
		++nb_desc;
	});
	auto&& region = Yaml_region::make(node);
	ctx.logger()
		.trace("Loaded {} {}{}{}{}", nb_desc, (is_metadata ? "metadata" : "data"), (region ? " from `" : ""), to_string(region), (region ? "'" : ""));
}

/** Loads the data (or metadata) from a yaml tree
 * \param ctx the context in which the direct dependencies are define
 * \param node the tree from where the direct dependencies are define
 * \param data_dependencies the map of direct dependencies for each data
 */
void compute_direct_dependencies_data(
	Context& ctx,
	PC_tree_t node,
	std::unordered_map<std::string, std::unordered_set<std::string>>& data_dependencies
)
{
	int map_len = len(node);

	for (int map_id = 0; map_id < map_len; ++map_id) {
		ctx.logger().trace("create direct dependencies for :: id {}, name={}", map_id, to_string(PC_get(node, "{%d}", map_id)).c_str());
		std::string dataname = to_string(PC_get(node, "{%d}", map_id));

		Data_descriptor& dsc = ctx.desc(dataname.c_str());

		Datatype_template_sptr data_template = dsc.default_type();

		data_template->get_dependencies(ctx, data_dependencies[dataname]);
	}

	ctx.logger().trace("Compute direct dependencies {} (meta)data", map_len);
}

} // namespace

/** Information about the dependencies of data
 */
template <typename TT>
class Data_dependencies
{
	/** A load status of the data dependencies
	 */
	enum Init_state {
		INIT_DEPS, ///<
		COMPUTE_DEPS, ///< found a cyclic dependencies between datas
		ALL_DEPS ///< the plugin is already initialized
	};

	/// list of direct dependencies
	std::unordered_set<TT> m_deps;

	/// list of all dependencies of the data
	std::unordered_set<TT> m_all_deps;

	/// The name of this data
	TT m_name;

	/// state of dependencies
	Init_state m_state;

public:
	/** Define a element of Data_dependencies
	 * \param name the name of the data
	 * \param direct_dependencies direct_dependencies of the data
	 */
	Data_dependencies(TT name, std::unordered_set<TT> direct_dependencies);

	/** Evaluate all dependencies inside a set of Data_dependencies
	 * \param data_in_store the set of Data_dependencies
	 */
	void all_dependencies_resolved(std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store);

	/** Add to the stack the data if no cyclic dependencies is found
	 * \param data_in_store the set of Data_dependencies
	 * \param ordering_stack the stack to update
	 */
	void local_topological_sort(std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store, std::stack<TT>& ordering_stack);

	/** Evaluate all dependencies inside a set of Data_dependencies 
	 *  And Add to the stack the data if no cyclic dependencies is found
	 * \param data_in_store the set of Data_dependencies
	 * \param ordering_stack the stack to update
	 */
	void topological_sort_and_evaluate_all_dependencies(
		std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store,
		std::stack<TT>& ordering_stack
	);

	TT& get_name() { return m_name; }

	std::unordered_set<TT> get_dependencies() { return m_all_deps; }
};

template <typename TT>
Data_dependencies<TT>::Data_dependencies(TT name, std::unordered_set<TT> direct_dependencies)
	: m_name(name)
	, m_deps(direct_dependencies)
{
	m_state = INIT_DEPS;
}

template <typename TT>
void Data_dependencies<TT>::all_dependencies_resolved(std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store)
{
	switch (m_state) {
	case ALL_DEPS:
		return;
	case COMPUTE_DEPS:
		throw Impl_error{"Error while evaluate dependencies: circular dependency between data"};
	case INIT_DEPS:
		m_state = COMPUTE_DEPS;
		auto&& direct_dependencies = m_deps;

		for (auto&& elem: direct_dependencies) {
			auto&& data_info_it = data_in_store.find(elem);

			if (data_info_it == data_in_store.end()) {
				// if we have a dependencies with unknown data
				// example:
				// data:
				//     our_data: {type:array, subtype: double, size:"$our_size"}
				//
				// without define "our_size" in (meta)data section

				m_all_deps.insert(elem);
			} else {
				if (m_all_deps.insert(elem).second) {
					;
					data_info_it->second->all_dependencies_resolved(data_in_store);

					// insert indirect dependencies
					for (auto&& elem_dependencies: data_info_it->second->get_dependencies()) {
						m_all_deps.insert(elem_dependencies);
					}
				}
			}
		}
		m_state = ALL_DEPS;
	}
}

template <typename TT>
void Data_dependencies<TT>::local_topological_sort(
	std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store,
	std::stack<TT>& ordering_stack
)
{
	switch (m_state) {
	case ALL_DEPS:
		return;
	case COMPUTE_DEPS:
		throw Impl_error{"Error while evaluate dependencies: circular dependency between data"};
	case INIT_DEPS:
		m_state = COMPUTE_DEPS;
		auto&& direct_dependencies = m_deps;

		for (auto&& elem: direct_dependencies) {
			auto&& data_info_it = data_in_store.find(elem);

			if (data_info_it == data_in_store.end()) {
				// if we have a dependencies with unknown data
				// example:
				// data:
				//     our_data: {type:array, subtype: double, size:"$our_size"}
				//
				// without define "our_size" in (meta)data section

				// The ordering_stack contains only variables that are in metadata and data section
				// ==> nothing todo
			} else {
				data_info_it->second->local_topological_sort(data_in_store, ordering_stack);
			}
		}
		ordering_stack.push(m_name);
		m_state = ALL_DEPS;
	}
}

template <typename TT>
void Data_dependencies<TT>::topological_sort_and_evaluate_all_dependencies(
	std::map<TT, std::unique_ptr<Data_dependencies<TT>>>& data_in_store,
	std::stack<TT>& ordering_stack
)
{
	switch (m_state) {
	case ALL_DEPS:
		return;
	case COMPUTE_DEPS:
		throw Impl_error{"Error while evaluate dependencies: circular dependency between data"};
	case INIT_DEPS:
		m_state = COMPUTE_DEPS;
		auto&& direct_dependencies = m_deps;

		for (auto&& elem: direct_dependencies) {
			auto&& data_info_it = data_in_store.find(elem);

			if (data_info_it == data_in_store.end()) {
				// if we have a dependencies with unknown data
				// example:
				// data:
				//     our_data: {type:array, subtype: double, size:"$our_size"}
				//
				// without define "our_size" in (meta)data section

				m_all_deps.insert(elem);
			} else {
				if (m_all_deps.insert(elem).second) {
					// case: The "elem" is inserted in m_all_deps and doesn't exist before.
					data_info_it->second->topological_sort_and_evaluate_all_dependencies(data_in_store, ordering_stack);

					// insert dependencies
					for (auto&& elem_dependencies: data_info_it->second->get_dependencies()) {
						m_all_deps.insert(elem_dependencies);
					}
				}
			}
		}
		ordering_stack.push(m_name);
		m_state = ALL_DEPS;
	}
}

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

void Global_context::notify_init() const
{
	for (auto&& init_callback: m_init_callbacks) {
		init_callback();
	}
}

void Global_context::notify_data(const string& name, Ref ref)
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&, Ref)>>> data_callbacks;
	// add named callbacks
	auto callback_it_pair = m_named_data_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		data_callbacks.emplace_back(std::cref(it->second));
	}
	// add the unnamed callbacks
	for (auto it = m_data_callbacks.begin(); it != m_data_callbacks.end(); it++) {
		data_callbacks.emplace_back(std::cref(*it));
	}
	logger().trace("Calling `{}' share. Callbacks to call: {}", name, data_callbacks.size());
	// call gathered callbacks
	vector<std::exception_ptr> errors;
	for (const std::function<void(const std::string&, Ref)>& callback: data_callbacks) {
		try {
			callback(name, ref);
			//TODO: remove the faulty plugin in case of error?
		} catch (...) {
			errors.emplace_back(std::current_exception());
		}
	}
	rethrow_with_context(errors, "while sharing `{}', ", name);
}

void Global_context::notify_data_remove(const string& name, Ref ref)
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&, Ref)>>> data_remove_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_data_remove_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		data_remove_callbacks.emplace_back(std::cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_data_remove_callbacks.begin(); it != m_data_remove_callbacks.end(); it++) {
		data_remove_callbacks.emplace_back(std::cref(*it));
	}
	logger().trace("Calling `{}' data remove. Callbacks to call: {}", name, data_remove_callbacks.size());
	//call gathered callbacks
	vector<std::exception_ptr> errors;
	for (const std::function<void(const std::string&, Ref)>& callback: data_remove_callbacks) {
		try {
			callback(name, ref);
			//TODO: remove the faulty plugin in case of error?
		} catch (...) {
			errors.emplace_back(std::current_exception());
		}
	}
	rethrow_with_context(errors, "while removing `{}', ", name);
}

void Global_context::notify_event(const string& name)
{
	vector<std::reference_wrapper<const function<void(const string&)>>> event_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_event_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		event_callbacks.emplace_back(cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_event_callbacks.begin(); it != m_event_callbacks.end(); it++) {
		event_callbacks.emplace_back(cref(*it));
	}
	logger().trace("Calling `{}' event. Callbacks to call: {}", name, event_callbacks.size());
	//call gathered callbacks
	vector<std::exception_ptr> errors;
	for (const function<void(const string&)>& callback: event_callbacks) {
		try {
			callback(name);
			//TODO: remove the faulty plugin in case of error?
		} catch (...) {
			errors.emplace_back(std::current_exception());
		}
	}
	rethrow_with_context(errors, "while triggering `{}', ", name);
}

void Global_context::notify_missing_data(const string& name)
{
	std::vector<std::reference_wrapper<const std::function<void(const std::string&)>>> empty_desc_callbacks;
	//add named callbacks
	auto callback_it_pair = m_named_empty_desc_access_callbacks.equal_range(name);
	for (auto it = callback_it_pair.first; it != callback_it_pair.second; it++) {
		empty_desc_callbacks.emplace_back(std::cref(it->second));
	}
	//add the unnamed callbacks
	for (auto it = m_empty_desc_access_callbacks.begin(); it != m_empty_desc_access_callbacks.end(); it++) {
		empty_desc_callbacks.emplace_back(std::cref(*it));
	}
	logger().trace("Calling `{}' empty desc access. Callbacks to call: {}", name, empty_desc_callbacks.size());
	//call gathered callbacks
	vector<std::exception_ptr> errors;
	for (const std::function<void(const std::string&)>& callback: empty_desc_callbacks) {
		try {
			callback(name);
			//TODO: remove the faulty plugin in case of error?
		} catch (...) {
			errors.emplace_back(std::current_exception());
		}
	}
	rethrow_with_context(errors, "while populating `{}', ", name);
}

Global_context::Global_context(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_plugins{*this}
{
	// Handle includes and gather all files
	std::vector<PC_tree_t> confs = get_includes(logger(), conf);

	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);
	// load user datatypes
	for (auto&& conf: confs) {
		Datatype_template::load_user_datatypes(*this, PC_get(conf, ".types"));
	}

	m_plugins.load_plugins(confs);

	// evaluate pattern after loading plugins
	m_logger.evaluate_pattern(*this);

	std::map<std::string, std::optional<Yaml_region>> data_definition_location;

	for (auto&& conf: confs) {
		PC_tree_t metadata = PC_get(conf, ".metadata");
		if (!PC_status(metadata)) {
			load_data(*this, metadata, true, data_definition_location);
		}
	}

	for (auto&& conf: confs) {
		PC_tree_t data = PC_get(conf, ".data");
		if (!PC_status(data)) {
			load_data(*this, data, false, data_definition_location);
		}
	}
	// no data is spurious, but not an error
	if (data_definition_location.empty()) {
		m_logger.warn("No data (or metadata) defined in specification tree");
	}

	/// list of direct dependencies for a (meta)data defines in the specification tree
	std::unordered_map<std::string, std::unordered_set<std::string>> data_direct_dependencies;

	// create the dependencies between data and metadata
	for (auto&& conf: confs) {
		PC_tree_t metadata = PC_get(conf, ".metadata");
		if (!PC_status(metadata)) {
			m_logger.trace("compute metadata dependencies");
			compute_direct_dependencies_data(*this, metadata, data_direct_dependencies);
		}
	}

	for (auto&& conf: confs) {
		PC_tree_t data = PC_get(conf, ".data");
		if (!PC_status(data)) {
			m_logger.trace("compute data dependencies");
			compute_direct_dependencies_data(*this, data, data_direct_dependencies);
		}
	}

	std::map<std::string, std::unique_ptr<Data_dependencies<std::string>>> data_depend_on;

	size_t counter = 0;
	for (auto& elem: data_direct_dependencies) {
		data_depend_on.emplace(elem.first, std::make_unique<Data_dependencies<std::string>>(elem.first, elem.second));
		counter++;
	}

	m_logger.trace("compute all dependencies and topological sort");
	std::stack<std::string> ordering_stack;
	for (auto& elem: data_depend_on) {
		elem.second->topological_sort_and_evaluate_all_dependencies(data_depend_on, ordering_stack);
	}

	for (auto& elem: data_depend_on) {
		m_data_all_dependencies[elem.second->get_name()] = elem.second->get_dependencies();
	}

	// Compute counter
	counter = ordering_stack.size(); // zero value is for data that is not defined in (meta)data section
	while (!ordering_stack.empty()) {
		m_data_ordering[ordering_stack.top()] = (unsigned int)counter; // zero value is for data that is not defined in (meta)data section
		counter--;
		ordering_stack.pop();
	}

	notify_init();
	m_logger.info("Initialization successful");
}

Global_context::~Global_context()
{
	m_logger.info("Finalization");
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
	notify_event(name);
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
	throw Spectree_error{node, "Unknown data type: `{}'", type};
}

void Global_context::add_datatype(const string& name, Datatype_template_parser parser)
{
	if (!m_datatype_parsers.emplace(name, std::move(parser)).second) {
		//if a datatype with the given name already exists
		throw Type_error{"Datatype already defined `{}'", name};
	}
}

function<void()> Global_context::on_init(const function<void()>& callback)
{
	m_init_callbacks.emplace_back(callback);
	auto it = --m_init_callbacks.end();
	return [it, this]() {
		this->m_init_callbacks.erase(it);
	};
}

function<void()> Global_context::on_data(const function<void(const string&, Ref)>& callback, const string& name)
{
	if (name.empty()) {
		m_data_callbacks.emplace_back(callback);
		auto it = --m_data_callbacks.end();
		return [it, this]() {
			this->m_data_callbacks.erase(it);
		};
	} else {
		auto it = m_named_data_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_data_callbacks.erase(it);
		};
	}
}

function<void()> Global_context::on_data_remove(const function<void(const string&, Ref)>& callback, const string& name)
{
	if (name.empty()) {
		m_data_remove_callbacks.emplace_back(callback);
		auto it = --m_data_remove_callbacks.end();
		return [it, this]() {
			this->m_data_remove_callbacks.erase(it);
		};
	} else {
		auto it = m_named_data_remove_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_data_remove_callbacks.erase(it);
		};
	}
}

function<void()> Global_context::on_event(const function<void(const string&)>& callback, const string& name)
{
	if (name.empty()) {
		m_event_callbacks.emplace_back(callback);
		auto it = --m_event_callbacks.end();
		return [it, this]() {
			this->m_event_callbacks.erase(it);
		};
	} else {
		auto it = m_named_event_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_event_callbacks.erase(it);
		};
	}
}

function<void()> Global_context::on_missing_data(const function<void(const string&)>& callback, const string& name)
{
	if (name.empty()) {
		m_empty_desc_access_callbacks.emplace_back(callback);
		auto it = --m_empty_desc_access_callbacks.end();
		return [it, this]() {
			this->m_empty_desc_access_callbacks.erase(it);
		};
	} else {
		auto it = m_named_empty_desc_access_callbacks.emplace(name, callback);
		return [it, this]() {
			this->m_named_empty_desc_access_callbacks.erase(it);
		};
	}
}


} // namespace PDI
