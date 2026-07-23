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
#include <stdexcept>
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

#include "pdi/global_context.h"

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

/** Loads the data (or metadata) from a yaml tree
 * \param ctx the context in which to load
 * \param node the tree from where to load
 * \param is_metadata whether this is a metadata subtree instead of a data one
 * \param def_location the location of all loaded data/metadata for duplicate detection
 */
void load_data(Logger& logger, Global_context& ctx, PC_tree_t node, bool is_metadata, std::map<std::string, std::optional<Yaml_region>>& def_location)
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
	logger
		.trace("Loaded {} {}{}{}{}", nb_desc, (is_metadata ? "metadata" : "data"), (region ? " from `" : ""), to_string(region), (region ? "'" : ""));
}

} // namespace

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
	m_logger.trace("Calling `{}' share. Callbacks to call: {}", name, data_callbacks.size());
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
	m_logger.trace("Calling `{}' data remove. Callbacks to call: {}", name, data_remove_callbacks.size());
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
	m_logger.trace("Calling `{}' event. Callbacks to call: {}", name, event_callbacks.size());
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
	m_logger.trace("Calling `{}' empty desc access. Callbacks to call: {}", name, empty_desc_callbacks.size());
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

Global_context::Global_context(Logger& logger)
	: m_logger(logger)
{}

Global_context::~Global_context() = default;

void Global_context::configure(std::vector<PC_tree_t> confs)
{
	std::map<std::string, std::optional<Yaml_region>> data_definition_location;

	for (auto&& conf: confs) {
		PC_tree_t metadata = PC_get(conf, ".metadata");
		if (!PC_status(metadata)) {
			load_data(m_logger, *this, metadata, true, data_definition_location);
		}
	}

	for (auto&& conf: confs) {
		PC_tree_t data = PC_get(conf, ".data");
		if (!PC_status(data)) {
			load_data(m_logger, *this, data, false, data_definition_location);
		}
	}
	// no data is spurious, but not an error
	if (data_definition_location.empty()) {
		m_logger.warn("No data (or metadata) defined in specification tree");
	}

	notify_init();
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
			m_logger.warn("In line {}: Non-array type with a `size' property", node.node->start_mark.line);
		}
		if (!PC_status(PC_get(node, ".sizes"))) {
			m_logger.warn("In line {}: Non-array type with a `sizes' property", node.node->start_mark.line);
		}
	}

	auto&& func_it = m_datatype_parsers.find(type);
	if (func_it != m_datatype_parsers.end()) {
		return (func_it->second)(m_logger, *this, node);
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
