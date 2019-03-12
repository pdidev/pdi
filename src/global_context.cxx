/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <map>
#include <memory>
#include <vector>

#include <dlfcn.h>
#include <spdlog/spdlog.h>

#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"

#include "data_descriptor_impl.h"

#include "global_context.h"


namespace PDI {

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


namespace {

using plugin_loader_f = unique_ptr<Plugin>(*)(Context&, PC_tree_t);
using plugin_dependencies_f = pair<unordered_set<string>, unordered_set<string>>(*)();

void load_data(Context& ctx, PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);
	
	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = ctx.desc(to_string(PC_get(node, "{%d}", map_id)).c_str());
		dsc.metadata(is_metadata);
		dsc.default_type(ctx.datatype(PC_get(node, "<%d>", map_id)));
	}
}

plugin_loader_f PDI_NO_EXPORT get_plugin_ctr(const char* plugin_name)
{
	string plugin_symbol = string{"PDI_plugin_"} + plugin_name + string{"_loader"};
	void* plugin_ctor_uncast = dlsym(NULL, plugin_symbol.c_str());
	
	// case where the library was not prelinked
	if (!plugin_ctor_uncast) {
		string libname = string{"libpdi_"} + plugin_name + string{"_plugin.so"};
		// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
		void* lib_handle = dlopen(libname.c_str(), (RTLD_LAZY|RTLD_GLOBAL));
		if (!lib_handle) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin file: %s", plugin_name, dlerror()};
		}
		plugin_ctor_uncast = dlsym(lib_handle, plugin_symbol.c_str());
	}
	
	if (!plugin_ctor_uncast) {
		throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin from file: %s", plugin_name, dlerror()};
	}
	
	return reinterpret_cast<plugin_loader_f>(plugin_ctor_uncast);
}

plugin_dependencies_f PDI_NO_EXPORT get_plugin_dependencies(const char* plugin_name)
{
	string plugin_symbol = string{"PDI_plugin_"} + plugin_name + string{"_dependencies"};
	void* plugin_deps_uncast = dlsym(NULL, plugin_symbol.c_str());
	
	// case where the library was not prelinked
	if (!plugin_deps_uncast) {
		string libname = string{"libpdi_"} + plugin_name + string{"_plugin.so"};
		void* lib_handle = dlopen(libname.c_str(), RTLD_NOW);
		if (!lib_handle) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin file: %s", plugin_name, dlerror()};
		}
		plugin_deps_uncast = dlsym(lib_handle, plugin_symbol.c_str());
	}
	
	if (!plugin_deps_uncast) {
		throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin dependencies from file: %s", plugin_name, dlerror()};
	}
	
	return reinterpret_cast<plugin_dependencies_f>(plugin_deps_uncast);
}

/** A load status of a plugin
 */
enum class Init_state {
	UNINITIALIZED, ///< the plugin is not initialized yet
	INITIALIZING_PRE_DEPS, ///< the plugin has been visited and its pre-dependencies are being initialized
	INITIALIZED ///< the plugin is already initialized
};

/** Information about the load state of a plugin
 */
struct Plugin_load_info {
	/// A constructor function of the plugin
	plugin_loader_f m_ctr;
	/// A context of the plugin
	Context& m_ctx;
	/// A config of the plugin
	PC_tree_t m_config;
	/// A dependencies function of the plugin
	plugin_dependencies_f m_dependencies;
	/// Whether this plugin is initialized or not
	Init_state m_state;
	/// A dependencies of the plugin
	vector<map<string, Plugin_load_info>::iterator> m_pre_dependencies;
	
	Plugin_load_info(plugin_loader_f plugin_ctr, Context& plugin_ctx, PC_tree_t plugin_config, plugin_dependencies_f plugin_dependencies):
		m_ctr(plugin_ctr),
		m_ctx(plugin_ctx),
		m_config(plugin_config),
		m_dependencies(plugin_dependencies),
		m_state(Init_state::UNINITIALIZED)
	{}
};

/** Initializes a plugin if not done yet and its pre-dependencies if required
 * \param plugin_info a reference to information about the load state of a plugin
 * \param plugins_map a conteiner where loaded plugins should be stored
 */
void initialize_plugin(map<string, Plugin_load_info>::iterator& plugin_info, unordered_map<string, unique_ptr<Plugin>>& plugins_map)
{
	switch (plugin_info->second.m_state) {
	case Init_state::INITIALIZED:
		return;
	case Init_state::INITIALIZING_PRE_DEPS:
		throw Error{PDI_ERR_IMPL, "Error while initializing plugin: circular dependency between plugins"};
	case Init_state::UNINITIALIZED:
		plugin_info->second.m_state = Init_state::INITIALIZING_PRE_DEPS;
		for (auto&& pre_plugin : plugin_info->second.m_pre_dependencies) {
			initialize_plugin(pre_plugin, plugins_map);
		}
		plugin_info->second.m_state = Init_state::INITIALIZED;
		plugins_map.emplace(plugin_info->first, plugin_info->second.m_ctr(plugin_info->second.m_ctx, plugin_info->second.m_config));
	}
}

/** Checks the requirements and pre-dependencies between plugins and initializes them.
 * \param plugins_info a reference to informations about the load state of plugins
 * \param plugins_map a conteiner where loaded plugins should be stored
 */
void initialize_plugins(map<string, Plugin_load_info>& plugins_info, unordered_map<string, unique_ptr<Plugin>>& plugins_map)
{
	for (auto&& plugin_info: plugins_info) {
		auto&& plugin_dependencies = plugin_info.second.m_dependencies();
		
		for (auto&& req_plugin : plugin_dependencies.first) {
			auto&& req_plugin_info_it = plugins_info.find(req_plugin);
			if (req_plugin_info_it == plugins_info.end()) {
				throw Error{PDI_ERR_SYSTEM, "Error while loading plugin `%s': required plugin `%s' is not loaded", plugin_info.first.c_str(), req_plugin.c_str()};
			}
		}
		for (auto&& pre_plugin: plugin_dependencies.second) {
			auto&& plugin_info_it = plugins_info.find(pre_plugin);
			if (plugin_info_it != plugins_info.end()) {
				plugin_info.second.m_pre_dependencies.emplace_back(plugin_info_it);
			}
		}
	}
	for (auto&& plugin_it = plugins_info.begin(); plugin_it != plugins_info.end(); plugin_it++)
		initialize_plugin(plugin_it, plugins_map);
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
	if (!s_context) throw Error{PDI_ERR_STATE, "PDI not initialized"};
	return *s_context;
}

void Global_context::finalize()
{
	s_context.reset();
}

Global_context::Global_context(PC_tree_t conf):
	m_logger{configure_logger(conf)}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);
	
	// first load plugins, because they can add datatypes declared in config
	map<string, Plugin_load_info> plugins_info;
	int nb_plugins = len(PC_get(conf, ".plugins"), 0);
	try {
		for (int plugin_id = 0; plugin_id < nb_plugins; ++plugin_id) {
			//TODO: what to do if a single plugin fails to load?
			string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
			PC_tree_t plugin_node = PC_get(conf, ".plugins<%d>", plugin_id);
			Context_proxy& ctx_proxy = (*m_context_proxy.emplace(plugin_name, Context_proxy{*this, plugin_name, PC_get(plugin_node, ".logging")}).first).second;
			plugins_info.emplace(piecewise_construct,
			    forward_as_tuple(plugin_name),
			    forward_as_tuple(get_plugin_ctr(plugin_name.c_str()), ctx_proxy, plugin_node, get_plugin_dependencies(plugin_name.c_str()))
			);
		}
		initialize_plugins(plugins_info, m_plugins);
	} catch (const exception& e) {
		throw Error{PDI_ERR_SYSTEM, "Error while loading plugins: %s", e.what()};
	}
	
	// no metadata is not an error
	PC_tree_t metadata = PC_get(conf, ".metadata");
	if (!PC_status(metadata)) {
		load_data(*this, metadata, true);
	}
	
	// no data is spurious, but not an error
	PC_tree_t data = PC_get(conf, ".data");
	if (!PC_status(data)) {
		load_data(*this, data, false);
	}
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
	for (auto&& elmnt : m_plugins) {
		vector<Error> errors;
		try {
			elmnt.second->event(name);
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e.status(), "for plugin `%s': %s", elmnt.first.c_str(), e.what());
		} catch (const exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, "for plugin `%s': %s", elmnt.first.c_str(), e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "for plugin `%s'", elmnt.first.c_str());
		}
		if ( 1 == errors.size() ) throw Error{errors.front().status(), "While triggering named event `%s' %s", name, errors.front().what()};
		if ( !errors.empty() ) {
			string errmsg = "multiple errors while triggering named event `";
			errmsg += name;
			errmsg += "':";
			for ( auto&& err: errors ) {
				errmsg += " ";
				errmsg += err.what();
			}
			throw Error{PDI_ERR_PLUGIN, "%s", errmsg.c_str()};
		}
	}
}

Logger_sptr Global_context::logger() const
{
	return m_logger;
}

Datatype_template_uptr Global_context::datatype(PC_tree_t node)
{
	char* type_c;
	if ( PC_string(PC_get(node, ".type"), &type_c) ) {
		if ( PC_string(node, &type_c) ) {
			throw Error{PDI_ERR_TYPE, "Invalid type descriptor"};
		}
	}
	string type = type_c;
	
	// check if someone didn't mean to create an array with the old syntax
	if ( type != "array" && !PC_status(PC_get(node, ".size"))) {
		logger()->warn("Non-array type with a `size' property");
	}
	if ( type != "array" && !PC_status(PC_get(node, ".sizes"))) {
		logger()->warn("Non-array type with a `sizes' property");
	}
	
	auto&& func_it = m_datatype_parsers.find(type);
	if (func_it != m_datatype_parsers.end()) {
		return (func_it->second)(*this, node);
	}
	throw Error{PDI_ERR_TYPE, "Cannot find datatype `%s'", type.c_str()};
}

void Global_context::add_datatype(const string& name, Datatype_template_parser parser)
{
	if (!m_datatype_parsers.emplace(name, move(parser)).second) {
		//if a datatype with the given name already exists
		throw Error{PDI_ERR_TYPE, "Datatype already defined `%s'", name.c_str()};
	}
}

}
