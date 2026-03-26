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
#include <unordered_set>
#include <stdexcept>

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

namespace PDI {

namespace {

void load_data(Global_context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);

	for (int map_id = 0; map_id < map_len; ++map_id) {
		std::string name = to_string(PC_get(node, "{%d}", map_id));

		ctx.check_duplicate(name);

		Data_descriptor& dsc = ctx.desc(name.c_str());
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}

	if (is_metadata)
		ctx.logger().trace("Loaded {} metadata", map_len);
	else
		ctx.logger().trace("Loaded {} data", map_len);
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

void Global_context::load_pdi_config(PC_tree_t conf, std::unordered_set<std::string>* loaded_files) {
	std::unordered_set<std::string> current_loaded_files;
	if (!loaded_files) loaded_files = &current_loaded_files;

	const char* current_path = PC_path(conf);
	std::string current_file = fs::path(current_path).lexically_normal().string();

	// Check for repeated includes (diamond or circular)
	if (loaded_files->find(current_file) != loaded_files->end()) {
		m_logger.warn("Skipping already included file '{}'", current_file);
		return;
	}
	loaded_files->insert(current_file);

	Datatype_template::load_user_datatypes(*this, PC_get(conf, ".types"));
	if (PC_tree_t metadata = PC_get(conf, ".metadata"); !PC_status(metadata)) {
		load_data(*this, metadata, true);
	}
	if (PC_tree_t data = PC_get(conf, ".data"); !PC_status(data)) {
		load_data(*this, data, false);
	}

	PC_tree_t includes = PC_get(conf, ".include");
	if (!PC_status(includes)) {
		PDI::each(includes, [&](PC_tree_t yaml_subfile) {
			std::string include_path = PDI::to_string(yaml_subfile);
			fs::path full_path = fs::path(include_path).is_absolute()
				? fs::path(include_path)
				: fs::path(current_path).parent_path() / include_path;

			full_path = full_path.lexically_normal();

			if (!fs::exists(full_path)) {
				throw std::runtime_error("Included file not found: " + full_path.string());
			}

			PC_tree_t included_conf = PC_parse_path(full_path.string().c_str());
			if (PC_status(included_conf) != PC_OK) {
				throw std::runtime_error("Failed to parse: " + full_path.string());
			}

			load_pdi_config(included_conf, loaded_files);
		});
	}
}

Global_context::Global_context(PC_tree_t conf)
	: m_logger{"PDI", PC_get(conf, ".logging")}
	, m_plugins{*this, conf}
	, m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);

	m_plugins.load_plugins();

	load_pdi_config(conf);

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

void Global_context::finalize_and_exit()
{
	Global_context::finalize();
	exit(0);
}

void Global_context::check_duplicate(const std::string& name)
{
	if (!m_defined.insert(name).second) {
		// throw System_error instead of std::runtime_error to use assert for test on expected error
		throw System_error("Duplicate definition of '{}'", name);
	}
}

Global_context::~Global_context()
{
	m_logger.info("Finalization");
}

} // namespace PDI
