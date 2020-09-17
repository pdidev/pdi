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
	if (is_metadata) {
		ctx.logger()->trace("Loaded {} metadata", map_len);
	} else {
		ctx.logger()->trace("Loaded {} data", map_len);
	}
	
}

string PDI_NO_EXPORT get_plugin_lib_so_path(const std::string& plugin_name, PC_tree_t plugin_path_node)
{
	// STEP 1: get path from yaml file
	if (!PC_status(plugin_path_node)) {
		if (is_list(plugin_path_node)) {
			int len = PDI::len(plugin_path_node);
			for (int i = 0; i < len; i++) {
				string path_to_check = PDI::to_string(PC_get(plugin_path_node, "[%d]", i)) + "/libpdi_" + plugin_name + "_plugin.so";
				if (access(path_to_check.c_str(), F_OK) == 0) {
					return path_to_check;
				}
			}
		} else if (is_scalar(plugin_path_node)) {
			string path_to_check = PDI::to_string(plugin_path_node) + "/libpdi_" + plugin_name + "_plugin.so";
			if (access(path_to_check.c_str(), F_OK) == 0) {
				return path_to_check;
			}
		} else {
			throw Config_error{"plugin_path must be a single path or an array of paths"};
		}
	}

	// STEP 2: get path from PDI_PLUGIN_PATH
	if (const char* const_pdi_plugin_path = std::getenv("PDI_PLUGIN_PATH")) {
		char* pdi_plugin_path_start = new char[strlen(const_pdi_plugin_path)];
		
		char* pdi_plugin_path = pdi_plugin_path_start;
		strcpy(pdi_plugin_path, const_pdi_plugin_path);
		
		char* current = pdi_plugin_path;
		while (*current != '\0') {
			if (*current == ':' && current != pdi_plugin_path_start) {
				if (*(current - 1) == '\\') {
					// move rest of string one char left
					for (int i = 0; i < strlen(current) + 1; i++) {
						*(current + i - 1) = *(current + i);
					}
				} else {
					*current = '\0';
					string path_to_check = string(pdi_plugin_path) + "/libpdi_" + plugin_name + "_plugin.so";
					if (access(path_to_check.c_str(), F_OK) == 0) {
						return path_to_check;
					}
					pdi_plugin_path = current + 1;
				}
			}
			current++;
		}
		if (strlen(pdi_plugin_path)) {
			string path_to_check = string(pdi_plugin_path) + "/libpdi_" + plugin_name + "_plugin.so";
			if (access(path_to_check.c_str(), F_OK) == 0) {
				return path_to_check;
			}
		}
	
		delete[] pdi_plugin_path_start;
	}

	// STEP 3: get from relative path to libpdi.so
	void* libpdi_handle = dlopen("libpdi.so", RTLD_NOW);
	if (libpdi_handle == NULL) {
		throw System_error{"Unable to load libpdi.so file: {}", dlerror()};
	}
	void* pdi_init_addr = dlsym(libpdi_handle, "PDI_init");
	if (pdi_init_addr == NULL) {
		throw System_error{"Unable to get PDI_init symbol from libpdi.so file: {}", dlerror()};
	}
	
	Dl_info libpdi_info;
	int status = dladdr(pdi_init_addr, &libpdi_info);
	if (status == 0) {
		throw System_error{"Unable to get addr from libpdi.so file: {}", dlerror()};
	}
	std::string libpdi_path = libpdi_info.dli_fname;
	libpdi_path = libpdi_path.substr(0, libpdi_path.find_last_of('/') + 1);
	dlclose(libpdi_handle);

	string plugin_lib_so_path = libpdi_path + "pdi/plugins/" + 
								std::to_string(PDI_VERSION_MAJOR) +
								"." + std::to_string(PDI_VERSION_MINOR) +
								"." + std::to_string(PDI_VERSION_PATCH) +
								"/libpdi_" + plugin_name + "_plugin.so";

	// STEP 4: plugin .so is in different place, try to find it separately (last hope)
	if (access(plugin_lib_so_path.c_str(), F_OK) != 0) {
		plugin_lib_so_path = "libpdi_" + plugin_name + "_plugin.so";
	}

	return plugin_lib_so_path;
}

plugin_loader_f PDI_NO_EXPORT get_plugin_ctr(const string& plugin_name, PC_tree_t plugin_path_node)
{
	string plugin_symbol = "PDI_plugin_" + plugin_name + "_loader";
	void* plugin_ctor_uncast = dlsym(NULL, plugin_symbol.c_str());
	// case where the library was not prelinked
	if (!plugin_ctor_uncast) {
		std::string libname = get_plugin_lib_so_path(plugin_name,  plugin_path_node);
		
		// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
		void* lib_handle = dlopen(libname.c_str(), (RTLD_LAZY|RTLD_GLOBAL));
		if (!lib_handle) {
			throw Plugin_error{"Unable to load `{}' plugin file: {}", plugin_name, dlerror()};
		}
		plugin_ctor_uncast = dlsym(lib_handle, plugin_symbol.c_str());
	}
	
	if (!plugin_ctor_uncast) {
		throw Plugin_error{"Unable to load `{}' plugin from file: {}", plugin_name, dlerror()};
	}
	
	return reinterpret_cast<plugin_loader_f>(plugin_ctor_uncast);
}

plugin_dependencies_f PDI_NO_EXPORT get_plugin_dependencies(const string& plugin_name, PC_tree_t plugin_path_node)
{
	string plugin_symbol = "PDI_plugin_" + plugin_name + "_dependencies";
	void* plugin_deps_uncast = dlsym(NULL, plugin_symbol.c_str());
	
	// case where the library was not prelinked
	if (!plugin_deps_uncast) {
		std::string libname = get_plugin_lib_so_path(plugin_name, plugin_path_node);
		void* lib_handle = dlopen(libname.c_str(), RTLD_NOW);
		if (!lib_handle) {
			throw Plugin_error{"Unable to load `{}' plugin file: {}", plugin_name, dlerror()};
		}
		plugin_deps_uncast = dlsym(lib_handle, plugin_symbol.c_str());
	}
	
	if (!plugin_deps_uncast) {
		throw Plugin_error{"Unable to load `{}' plugin dependencies from file: {}", plugin_name, dlerror()};
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
		throw Impl_error{"Error while initializing plugin: circular dependency between plugins"};
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
				throw System_error{"Error while loading plugin `{}': required plugin `{}' is not loaded", plugin_info.first, req_plugin};
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
	if (!s_context) throw State_error{"PDI not initialized"};
	return *s_context;
}

void Global_context::finalize()
{
	s_context.reset();
}

Global_context::Global_context(PC_tree_t conf):
	m_logger{configure_logger(PC_get(conf, ".logging"), "global")},
	m_callbacks{*this}
{
	// load basic datatypes
	Datatype_template::load_basic_datatypes(*this);
	
	// first load plugins, because they can add datatypes declared in config
	map<string, Plugin_load_info> plugins_info;
	int nb_plugins = len(PC_get(conf, ".plugins"), 0);
	try {
		m_logger->trace("Plugins to load: {}", nb_plugins);
		for (int plugin_id = 0; plugin_id < nb_plugins; ++plugin_id) {
			//TODO: what to do if a single plugin fails to load?
			string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
			PC_tree_t plugin_node = PC_get(conf, ".plugins<%d>", plugin_id);
			Context_proxy& ctx_proxy = (*m_context_proxy.emplace(plugin_name, Context_proxy{*this, plugin_name, PC_get(conf, ".logging")}).first).second;
			m_logger->trace("Creating {} plugin", plugin_name);
			plugins_info.emplace(piecewise_construct,
			    forward_as_tuple(plugin_name),
			    forward_as_tuple(get_plugin_ctr(plugin_name, PC_get(conf, ".plugin_path")), ctx_proxy, plugin_node, get_plugin_dependencies(plugin_name, PC_get(conf, ".plugin_path")))
			);
		}
		initialize_plugins(plugins_info, m_plugins);
	} catch (const exception& e) {
		throw System_error{"Error while loading plugins: {}", e.what()};
	}
	
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
	char* type_c;
	if ( PC_string(PC_get(node, ".type"), &type_c) ) {
		if ( PC_string(node, &type_c) ) {
			throw Type_error{"Invalid type descriptor"};
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
	throw Type_error{"Cannot find datatype `{}'", type};
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
