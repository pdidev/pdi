/*******************************************************************************
 * Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <spdlog/fmt/fmt.h>

#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"
#include "pdi/version.h"

#include "data_descriptor_impl.h"
#include "string_tools.h"

#include "plugin_store.h"


namespace PDI {

using fmt::format;
using fmt::join;
using std::exception;
using std::forward_as_tuple;
using std::map;
using std::make_shared;
using std::pair;
using std::piecewise_construct;
using std::shared_ptr;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

Plugin_store::Stored_plugin::Stored_plugin(Context& ctx, Plugin_store& store, string name, PC_tree_t conf):
	m_config{conf},
	m_ctx{ctx},
	m_name{name},
	m_state{PRELOADED}
{
	ctx.logger()->trace("Pre-loading plugin `{}'", name);
	
	string ctor_symbol = "PDI_plugin_" + name + "_loader";
	string deps_symbol = "PDI_plugin_" + name + "_dependencies";
	string pretty_name_symbol = "PDI_plugin_" + name + "_pretty_name";
	
	// case where the library was prelinked
	m_ctr = reinterpret_cast<plugin_factory_f>(dlsym(NULL, ctor_symbol.c_str()));
	m_deps = reinterpret_cast<plugin_deps_f>(dlsym(NULL, deps_symbol.c_str()));
	auto pretty_name_f = reinterpret_cast<std::string(*)()>(dlsym(NULL, pretty_name_symbol.c_str()));

	// case where the library was not prelinked
	void* lib_handle = NULL;
	if (!m_ctr || !m_deps) {
		lib_handle = store.plugin_dlopen(name);
	}
	
	if (!m_ctr) {
		m_ctr = reinterpret_cast<plugin_factory_f>(dlsym(lib_handle, ctor_symbol.c_str()));
		if (!m_ctr) {
			throw Plugin_error{"Unable to load plugin constructor for `{}': {}", name, dlerror()};
		}
	}
	
	if (!m_deps) {
		m_deps = reinterpret_cast<plugin_deps_f>(dlsym(lib_handle, deps_symbol.c_str()));
		if (!m_deps) {
			throw Plugin_error{"Unable to load plugin dependencies for `{}': {}", name, dlerror()};
		}
	}
	if (!pretty_name_f) {
		pretty_name_f = reinterpret_cast<std::string(*)()>(dlsym(lib_handle, pretty_name_symbol.c_str()));
		if (!m_deps) {
			throw Plugin_error{"Unable to load plugin pretty name for `{}': {}", name, dlerror()};
		}
	}

	m_ctx.setup_logger(pretty_name_f(), PC_get(conf, ".logging"));
}

void Plugin_store::Stored_plugin::ensure_loaded(map<string, shared_ptr<Stored_plugin>>& plugins)
{
	switch (m_state) {
	case LOADED:
		return;
	case LOADING:
		throw Impl_error{"Error while initializing plugin: circular dependency between plugins"};
	case PRELOADED:
		m_state = LOADING;
		auto&& plugin_dependencies = m_deps();
		
		for (auto&& req_plugin : plugin_dependencies.first) {
			auto&& plugin_info_it = plugins.find(req_plugin);
			if (plugin_info_it == plugins.end()) {
				throw System_error{"Error while loading plugin `{}': required plugin `{}' is not loaded", m_name, req_plugin};
			}
			m_dependencies.push_back(plugin_info_it->second);
			plugin_info_it->second->ensure_loaded(plugins);
		}
		for (auto&& pre_plugin: plugin_dependencies.second) {
			auto&& plugin_info_it = plugins.find(pre_plugin);
			if (plugin_info_it != plugins.end()) {
				m_dependencies.push_back(plugin_info_it->second);
				plugin_info_it->second->ensure_loaded(plugins);
			}
		}
		m_state = LOADED;
		m_loaded_plugin = m_ctr(m_ctx, m_config);
	}
}

void Plugin_store::initialize_path(PC_tree_t plugin_path_node)
{
	// STEP 1: get path from PDI_PLUGIN_PATH
	if (const char* env_plugin_path = std::getenv("PDI_PLUGIN_PATH")) {
		m_ctx.logger()->trace("Found PDI_PLUGIN_PATH env variable: `{}'", env_plugin_path);
		vector<string> escaped_env_plugin_path = string_array_parse(env_plugin_path);
		for (auto&& one_path: escaped_env_plugin_path) {
			m_ctx.logger()->trace("Escaped PDI_PLUGIN_PATH[] env variable: `{}'", one_path);
		}
		m_plugin_path.insert(m_plugin_path.end(), escaped_env_plugin_path.begin(), escaped_env_plugin_path.end());
	}

	// STEP 2: get path from yaml file
	if (!PC_status(plugin_path_node)) {
		if (is_list(plugin_path_node)) {
			int len = PDI::len(plugin_path_node);
			for (int i = 0; i < len; i++) {
				m_ctx.logger()->trace("Adding plugin path from yaml: `{}'", PDI::to_string(PC_get(plugin_path_node, "[%d]", i)));
				m_plugin_path.push_back(PDI::to_string(PC_get(plugin_path_node, "[%d]", i)));
			}
		} else if (is_scalar(plugin_path_node)) {
			m_ctx.logger()->trace("Adding plugin path from yaml: `{}'", PDI::to_string(plugin_path_node));
			m_plugin_path.push_back(PDI::to_string(plugin_path_node));
		} else {
			throw Config_error{plugin_path_node, "plugin_path must be a single path or an array of paths"};
		}
	}

	// STEP 3: get from relative path to libpdi.so
	if /* constexpr */ (PDI_DEFAULT_PLUGIN_PATH[0] == '/') {
		m_ctx.logger()->trace("Adding plugin path: `{}'", PDI_DEFAULT_PLUGIN_PATH);
		m_plugin_path.push_back(PDI_DEFAULT_PLUGIN_PATH);
	} else {
		Dl_info libpdi_info;
		if (dladdr((reinterpret_cast<void*>(PDI_init)), &libpdi_info)) {
			string path = libpdi_info.dli_fname;
			path = path.substr(0, path.find_last_of('/'));
			path = path + "/" + PDI_DEFAULT_PLUGIN_PATH;
			m_ctx.logger()->trace("Adding plugin path `{}' relative to PDI lib: `{}'", PDI_DEFAULT_PLUGIN_PATH, path);
			m_plugin_path.push_back(path);
		}
	}
}

void* Plugin_store::plugin_dlopen(const std::string& plugin_name)
{
	vector<string> load_errors;
	
	// STEP 1: try using expected path
	for ( auto&& path: m_plugin_path ) {
		string libname = path + "/libpdi_" + plugin_name + "_plugin.so";
		// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
		void* lib_handle = dlopen(libname.c_str(), RTLD_NOW|RTLD_GLOBAL);
		if (lib_handle) {
			m_ctx.logger()->trace("Loaded `{}'", libname);
			return lib_handle;
		} else {
			const string error_msg = dlerror();
			m_ctx.logger()->debug("Unable to load `{}' {}", libname, error_msg);
			load_errors.push_back(format("\n  * unable to load `{}' {}", libname, error_msg));
		}
	}
	
	// STEP 2: get from relative path to system path
	if /* constexpr */ (PDI_DEFAULT_PLUGIN_PATH[0] != '/') {
		string libname = (PDI_DEFAULT_PLUGIN_PATH "/libpdi_") + plugin_name + "_plugin.so";
		// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
		void* lib_handle = dlopen(libname.c_str(), RTLD_NOW|RTLD_GLOBAL);
		if (lib_handle) {
			m_ctx.logger()->trace("Loaded `{}' relative to system path", libname);
			return lib_handle;
		} else {
			const string error_msg = dlerror();
			m_ctx.logger()->debug("Unable to load `{}' relative to system path {}", libname, error_msg);
			load_errors.push_back(format("\n  * unable to load `{}' relative to system path {}", libname, error_msg));
		}
	}
	
	// STEP 3: try system path
	string libname = string("libpdi_") + plugin_name + "_plugin.so";
	// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
	void* lib_handle = dlopen(libname.c_str(), RTLD_NOW|RTLD_GLOBAL);
	if (lib_handle) {
		m_ctx.logger()->trace("Loaded `{}' from system path", libname);
		return lib_handle;
	} else {
		const string error_msg = dlerror();
		m_ctx.logger()->debug("Unable to load `{}' from system path {}", libname, error_msg);
		load_errors.push_back(format("\n  * unable to load `{}' from system path {}", libname, error_msg));
	}
	
	throw Plugin_error{"Unable to load plugin `{}': {}"
		, plugin_name
		, join(load_errors, ", ")
	};
}

Plugin_store::Plugin_store(Context& ctx, PC_tree_t conf):
		m_ctx(ctx)
{
	initialize_path(PC_get(conf, ".plugin_path"));
	
	// pre-load the plugins
	int nb_plugins = len(PC_get(conf, ".plugins"), 0);
	m_ctx.logger()->trace("Loading {} plugin(s)", nb_plugins);
	for (int plugin_id = 0; plugin_id < nb_plugins; ++plugin_id) {
		string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
		m_plugins.emplace(plugin_name, make_shared<Stored_plugin>(m_ctx, *this, plugin_name, PC_get(conf, ".plugins<%d>", plugin_id)));
	}
}

void Plugin_store::load_plugins()
{
	try {
		for (auto&& plugin: m_plugins) {
			//TODO: what to do if a single plugin fails to load?
			plugin.second->ensure_loaded(m_plugins);
		}
	} catch (const Error& e) {
		throw;
	} catch (const exception& e) {
		throw System_error{"Error while loading plugins: {}", e.what()};
	}
}

}
