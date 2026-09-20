/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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
#include <exception>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include <dlfcn.h>

#include "pdi/error.h"
#include "pdi/fmt.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"

#include "string_tools.h"

#include "plugin_store.h"

namespace PDI {

using fmt::join;
using std::current_exception;
using std::exception_ptr;
using std::getenv;
using std::make_shared;
using std::string;
using std::vector;

Plugin_store::Stored_plugin::Stored_plugin(Plugin_store& store, string name, PC_tree_t conf, Symbols symbols)
	: m_config(conf)
	, m_ctr(symbols.ctr)
	, m_deps(symbols.deps)
	, m_name(std::move(name))
	, m_plugin_logger(store.m_logger, std::move(symbols.pretty_name), PC_get(conf, ".logging"))
{}

Plugin_store::Stored_plugin::Stored_plugin(Plugin_store& store, string name, PC_tree_t conf)
	: Stored_plugin(store, name, conf, store.plugin_dlopen(name))
{}

void Plugin_store::Stored_plugin::ensure_loaded(Plugin_store& store, Context& ctx)
{
	switch (m_state) {
	case LOADED:
		return;
	case LOADING:
		throw Impl_error{"Error while initializing plugin: circular dependency between plugins"};
	case PRELOADED:
		m_state = LOADING;
		auto&& plugin_dependencies = m_deps();

		for (auto&& req_plugin: plugin_dependencies.first) {
			auto&& plugin_info_it = store.m_plugins.find(req_plugin);
			if (plugin_info_it == store.m_plugins.end()) {
				throw System_error{"Error while loading plugin `{}': required plugin `{}' is not loaded", m_name, req_plugin};
			}
			m_dependencies.push_back(plugin_info_it->second);
			plugin_info_it->second->ensure_loaded(store, ctx);
		}
		for (auto&& pre_plugin: plugin_dependencies.second) {
			auto&& plugin_info_it = store.m_plugins.find(pre_plugin);
			if (plugin_info_it != store.m_plugins.end()) {
				m_dependencies.push_back(plugin_info_it->second);
				plugin_info_it->second->ensure_loaded(store, ctx);
			}
		}
		m_state = LOADED;
		m_loaded_plugin = m_ctr(m_plugin_logger, ctx, m_config);
	}
}

void Plugin_store::initialize_path(std::vector<PC_tree_t> const & confs)
{
	// STEP 1: get path from PDI_PLUGIN_PATH
	if (const char* env_plugin_path = std::getenv("PDI_PLUGIN_PATH")) {
		m_logger.trace("Found PDI_PLUGIN_PATH env variable: `{}'", env_plugin_path);
		vector<string> escaped_env_plugin_path = string_array_parse(env_plugin_path);
		for (auto&& one_path: escaped_env_plugin_path) {
			m_logger.trace("Escaped PDI_PLUGIN_PATH[] env variable: `{}'", one_path);
		}
		m_plugin_path.insert(m_plugin_path.end(), escaped_env_plugin_path.begin(), escaped_env_plugin_path.end());
	}

	// STEP 2: get path from yaml file
	for (PC_tree_t conf: confs) {
		PC_tree_t plugin_path_node = PC_get(conf, ".plugin_path");
		if (!PC_status(plugin_path_node)) {
			if (is_list(plugin_path_node)) {
				int len = PDI::len(plugin_path_node);
				for (int i = 0; i < len; i++) {
					m_logger.trace("Adding plugin path from yaml: `{}'", PDI::to_string(PC_get(plugin_path_node, "[%d]", i)));
					m_plugin_path.push_back(PDI::to_string(PC_get(plugin_path_node, "[%d]", i)));
				}
			} else if (is_scalar(plugin_path_node)) {
				m_logger.trace("Adding plugin path from yaml: `{}'", PDI::to_string(plugin_path_node));
				m_plugin_path.push_back(PDI::to_string(plugin_path_node));
			} else {
				throw Spectree_error{plugin_path_node, "plugin_path must be a single path or an array of paths"};
			}
		}
	}

	// STEP 3: get from relative path to libpdi.so
	if /* constexpr */ (PDI_DEFAULT_PLUGIN_PATH[0] == '/') {
		m_logger.trace("Adding plugin path: `{}'", PDI_DEFAULT_PLUGIN_PATH);
		m_plugin_path.push_back(PDI_DEFAULT_PLUGIN_PATH);
	} else {
		Dl_info libpdi_info;
		if (dladdr((reinterpret_cast<void*>(PDI_init)), &libpdi_info)) {
			string path = libpdi_info.dli_fname;
			path = path.substr(0, path.find_last_of('/'));
			path = path + "/" + PDI_DEFAULT_PLUGIN_PATH;
			m_logger.trace("Adding plugin path `{}' relative to PDI lib: `{}'", PDI_DEFAULT_PLUGIN_PATH, path);
			m_plugin_path.push_back(path);
		}
	}
}

Plugin_store::Symbols Plugin_store::plugin_dlopen(const string& name)
{
	m_logger.trace("Pre-loading plugin `{}'", name);

	string ctor_symbol = "PDI_plugin_" + name + "_loader";
	string deps_symbol = "PDI_plugin_" + name + "_dependencies";
	string pretty_name_symbol = "PDI_plugin_" + name + "_pretty_name";

	// case where the library was prelinked
	plugin_factory_f ctr = reinterpret_cast<plugin_factory_f>(dlsym(NULL, ctor_symbol.c_str()));
	plugin_deps_f deps = reinterpret_cast<plugin_deps_f>(dlsym(NULL, deps_symbol.c_str()));
	auto pretty_name_func = reinterpret_cast<string (*)()>(dlsym(NULL, pretty_name_symbol.c_str()));

	// case where the library was not prelinked
	void* lib_handle = nullptr;
	if (!ctr || !deps) {
		vector<string> load_errors;

		// STEP 1: try using expected path
		for (auto&& path: m_plugin_path) {
			string libname = path + "/libpdi_" + name + "_plugin.so";
			// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
			lib_handle = dlopen(libname.c_str(), RTLD_NOW | RTLD_GLOBAL);
			if (lib_handle) {
				m_logger.trace("Loaded `{}'", libname);
				break;
			} else {
				const string error_msg = dlerror();
				m_logger.debug("Unable to load `{}' {}", libname, error_msg);
				load_errors.push_back(fmt::format("\n  * unable to load `{}' {}", libname, error_msg));
			}
		}

		// STEP 2: get from relative path to system path
		if (PDI_DEFAULT_PLUGIN_PATH[0] != '/' && !lib_handle) {
			string libname = (PDI_DEFAULT_PLUGIN_PATH "/libpdi_") + name + "_plugin.so";
			// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
			lib_handle = dlopen(libname.c_str(), RTLD_NOW | RTLD_GLOBAL);
			if (lib_handle) {
				m_logger.trace("Loaded `{}' relative to system path", libname);
			} else {
				const string error_msg = dlerror();
				m_logger.debug("Unable to load `{}' relative to system path {}", libname, error_msg);
				load_errors.push_back(fmt::format("\n  * unable to load `{}' relative to system path {}", libname, error_msg));
			}
		}

		// STEP 3: try system path
		if (!lib_handle) {
			string libname = string("libpdi_") + name + "_plugin.so";
			// we'd like to use dlmopen(LM_ID_NEWLM, ...) but this leads to multiple PDI
			lib_handle = dlopen(libname.c_str(), RTLD_NOW | RTLD_GLOBAL);
			if (lib_handle) {
				m_logger.trace("Loaded `{}' from system path", libname);
			} else {
				const string error_msg = dlerror();
				m_logger.debug("Unable to load `{}' from system path {}", libname, error_msg);
				load_errors.push_back(fmt::format("\n  * unable to load `{}' from system path {}", libname, error_msg));
			}
		}

		if (!lib_handle) {
			throw Plugin_error{"Unable to load plugin `{}': {}", name, join(load_errors, ", ")};
		}
	}

	if (!ctr) {
		ctr = reinterpret_cast<plugin_factory_f>(dlsym(lib_handle, ctor_symbol.c_str()));
		if (!ctr) {
			throw Plugin_error{"Unable to load the constructor of plugin `{}': {}", name, dlerror()};
		}
	}

	if (!deps) {
		deps = reinterpret_cast<plugin_deps_f>(dlsym(lib_handle, deps_symbol.c_str()));
		if (!deps) {
			throw Plugin_error{"Unable to load the dependencies of plugin `{}': {}", name, dlerror()};
		}
	}
	if (!pretty_name_func) {
		pretty_name_func = reinterpret_cast<string (*)()>(dlsym(lib_handle, pretty_name_symbol.c_str()));
	}

	// a plugin built without PDI_PLUGIN may provide no pretty name, its name stands for it then
	return {ctr, deps, pretty_name_func ? pretty_name_func() : name};
}

Plugin_store::Plugin_store(Pdi_logger& logger)
	: m_logger(logger)
{}

void Plugin_store::load_plugins(Context& ctx, std::vector<PC_tree_t> const & confs)
{
	initialize_path(confs);

	// pre-load the plugins
	for (auto&& conf: confs) {
		int nb_plugins = len(PC_get(conf, ".plugins"), 0);
		auto&& region = Yaml_region::make(conf);
		m_logger.trace("Loading {} plugin(s){}{}{}", nb_plugins, (region ? " from `" : ""), to_string(region), (region ? "'" : ""));
		for (int plugin_id = 0; plugin_id < nb_plugins; ++plugin_id) {
			string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
			if (m_plugins.contains(plugin_name)) {
				throw Spectree_error(PC_get(conf, ".plugins"), "Duplicate load of plugin {}", plugin_name);
			}
			try {
				m_plugins.emplace(plugin_name, make_shared<Stored_plugin>(*this, plugin_name, PC_get(conf, ".plugins<%d>", plugin_id)));
			} catch (...) {
				rethrow_with_context(current_exception(), "while pre-loading plugin `{}', ", plugin_name);
			}
		}
	}

	std::vector<std::exception_ptr> errors;
	for (auto&& plugin: m_plugins) {
		try {
			plugin.second->ensure_loaded(*this, ctx);
		} catch (...) {
			try {
				rethrow_with_context(std::current_exception(), "while loading plugin `{}', ", plugin.first);
			} catch (...) {
				errors.emplace_back(std::current_exception());
			}
		}
	}
	rethrow_with_context(errors, "");
}

} // namespace PDI
