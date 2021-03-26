/*******************************************************************************
 * Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <spdlog/spdlog.h>

#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"
#include "pdi/version.h"

#include "data_descriptor_impl.h"

#include "global_context.h"


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

void load_data(Context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);
	
	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = ctx.desc(to_string(PC_get(node, "{%d}", map_id)).c_str());
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}
	if (is_metadata) {
		ctx.logger()->trace("Loaded {} metadata", map_len);
	} else {
		ctx.logger()->trace("Loaded {} data", map_len);
	}
	
}

} // namespace <anonymous>

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

Global_context::Global_context(PC_tree_t conf):
	m_logger{configure_logger(PC_get(conf, ".logging"), "global")},
	m_plugins{*this, conf},
	m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);
	
	m_plugins.load_plugins();
	
	// no metadata is not an error
	PC_tree_t metadata = PC_get(conf, ".metadata");
	if (!PC_status(metadata)) {
		load_data(*this, metadata, true);
	} else {
		m_logger->debug("Metadata is not defined in specification tree");
	}
	
	// no data is spurious, but not an error
	PC_tree_t data = PC_get(conf, ".data");
	if (!PC_status(data)) {
		load_data(*this, data, false);
	} else {
		m_logger->warn("Data is not defined in specification tree");
	}
	
	
	m_callbacks.call_init_callbacks();
}

Data_descriptor& Global_context::desc(const char* name)
{
	return *(m_descriptors.emplace(name, unique_ptr<Data_descriptor> {new Data_descriptor_impl{*this, name}}).first->second);
}

Data_descriptor& Global_context::desc(const string& name)
{
	return desc(name.c_str());
}

Data_descriptor& Global_context::operator[](const char* name)
{
	return desc(name);
}

Data_descriptor& Global_context::operator[](const string& name)
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

void Global_context::event(const char* name)
{
	m_callbacks.call_event_callbacks(name);
}

Logger_sptr Global_context::logger() const
{
	return m_logger;
}

Datatype_template_uptr Global_context::datatype(PC_tree_t node)
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
			logger()->warn("In line {}: Non-array type with a `size' property", node.node->start_mark.line);
		}
		if (!PC_status(PC_get(node, ".sizes"))) {
			logger()->warn("In line {}: Non-array type with a `sizes' property", node.node->start_mark.line);
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

}
