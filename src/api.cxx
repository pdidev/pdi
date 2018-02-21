/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

/** Implementation of the PDI public API functions.
 * 
 * \file api.c
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 **/

#include "config.h"

#include <cstddef>
#include <cstring>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>

#include <dlfcn.h>

#include "pdi/plugin.h"
#include "pdi/data_descriptor.h"
#include "pdi/data_reference.h"
#include "pdi/data_type.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/state.h"
#include "pdi/status.h"


using namespace PDI;
using std::cerr;
using std::endl;
using std::make_shared;
using std::move;
using std::stack;
using std::string;
using std::stringstream;
using std::underlying_type;
using std::shared_ptr;


namespace {

static PDI_status_t load_data(PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);
	
	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = PDI_state.desc(to_string(PC_get(node, "{%d}", map_id)));
		dsc.metadata(is_metadata);
		PC_tree_t config = PC_get(node, "<%d>", map_id);
		dsc.creation_template(Data_type::load(config), config);
	}
	
	return PDI_OK;
}

PDI_status_t load_conf(PC_tree_t node)
{
	// no metadata is not an error
	{
		PC_tree_t metadata = PC_get(node, ".metadata");
		if (!PC_status(metadata)) {
			load_data(metadata, true);
		}
	}
	
	// no data is spurious, but not an error
	{
		PC_tree_t data = PC_get(node, ".data");
		if (!PC_status(data)) {
			load_data(data, false);
		}
	}
	
	return PDI_OK;
}

typedef PDI_status_t (*init_f)(PC_tree_t conf, MPI_Comm *world, PDI_plugin_t *plugin);

void load_plugin(const char *plugin_name, PC_tree_t node, MPI_Comm *world, PDI_plugin_t *plugin)
{
	stringstream plugin_symbol;
	plugin_symbol << "PDI_plugin_" << plugin_name << "_ctor";
	void *plugin_ctor_uncast = dlsym(NULL, plugin_symbol.str().c_str());
	
	// case where the library was not prelinked
	if (!plugin_ctor_uncast) {
		stringstream libname;
		libname << "lib" << plugin_name << ".so";
		void *lib_handle = dlopen(libname.str().c_str(), RTLD_NOW);
		if (!lib_handle) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin file: %s", plugin_name, dlerror()};
		}
		plugin_ctor_uncast = dlsym(lib_handle, plugin_symbol.str().c_str());
		if (!plugin_ctor_uncast) {
			throw Error{PDI_ERR_PLUGIN, "Unable to load `%s' plugin from file: %s", plugin_name, dlerror()};
		}
	}
	
	// call the ctor
	(reinterpret_cast<init_f>(plugin_ctor_uncast))(node, world, plugin);
}

void try_load_plugin(PC_tree_t conf, int plugin_id, MPI_Comm *world)
{
	string plugin_name = to_string(PC_get(conf, ".plugins{%d}", plugin_id));
	try {
		PC_tree_t plugin_conf = PC_get(conf, ".plugins<%d>", plugin_id);
		PDI_plugin_t *plugin = new PDI_plugin_t;
		load_plugin(plugin_name.c_str(), plugin_conf, world, plugin);
		PDI_state.plugins.emplace(plugin_name, std::shared_ptr<PDI_plugin_t>(plugin));
	} catch (const std::exception &e) {
		throw Error{PDI_ERR_SYSTEM, "Error while loading plugin `%s': %s", plugin_name.c_str(), e.what()};
	}
}

PDI_inout_t operator&(PDI_inout_t a, PDI_inout_t b)
{
	typedef underlying_type< PDI_inout_t >::type UL;
	return static_cast<PDI_inout_t>(static_cast<UL>(a) & static_cast<UL>(b));
}

}

PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm *world)
try {
	Try_pc fw;
	PDI_state.transaction.clear();
	PDI_state.plugins.clear();
	
	load_conf(conf);
	
	int nb_plugins = len(PC_get(conf, ".plugins"), 0);
	
	for (int ii = 0; ii < nb_plugins; ++ii) {
		//TODO: what to do if a single plugin fails to load?
		try_load_plugin(conf, ii, world);
	}
	return PDI_OK;
	
} catch (const Error &e) {
	for (auto &&plugin : PDI_state.plugins) {
		try { // ignore errors here, try our best to finalize everyone
			plugin.second->finalize();
		} catch (...) {
			cerr << "Error while finalizing " << plugin.first << endl;
		}
	}
	PDI_state.transaction.clear();
	PDI_state.transaction_data.clear();
	PDI_state.plugins.clear();
	return PDI::return_err(e);
}

PDI_status_t PDI_finalize()
{
	Try_pc fw;
	for (auto plugin : PDI_state.plugins) {
		try { // ignore errors here, try our best to finalize everyone
			//TODO: concatenate errors in some way
			plugin.second->finalize();
		} catch (const std::exception &e) {
			cerr << "Error while finalizing " << plugin.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while finalizing " << plugin.first << endl;
		}
	}
	PDI_state.transaction.clear();
	PDI_state.transaction_data.clear();
	PDI_state.plugins.clear();
	//TODO: clear PDI_state.m_descriptors
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}


PDI_status_t PDI_event(const char *event)
{
	Try_pc fw;
	for (auto &elmnt : PDI_state.plugins) {
		try { // ignore errors here, try our best to notify everyone
			//TODO: concatenate errors in some way
			elmnt.second->event(event);
		} catch (const std::exception &e) {
			cerr << "Error while triggering event " << event << " for plugin " << elmnt.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while triggering event " << event << " for plugin " << elmnt.first << endl;
			//TODO: remove the faulty plugin?
		}
	}
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}


PDI_status_t PDI_access(const char *name, void **buffer, PDI_inout_t inout)
try {
	Try_pc fw;
	Data_descriptor &desc = PDI_state.desc(name);
	Data_ref ref = desc.ref();
	switch (inout) {
		case PDI_NONE: *buffer = nullptr; break;
		case PDI_IN: *buffer = const_cast<void*>(Data_r_ref{desc.ref()}.get()); break;
		case PDI_OUT: *buffer = Data_w_ref{desc.ref()}.get(); break;
		case PDI_INOUT: *buffer = Data_rw_ref{desc.ref()}.get(); break;
	}
	desc.share(ref, inout & PDI_IN, inout & PDI_OUT);
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_share(const char *name, void *buffer, PDI_inout_t access)
try {
	Try_pc fw;
	if ( !buffer ) {
		throw Error{PDI_ERR_VALUE, "Sharing null pointers is not allowed"};
	}
	Data_descriptor &desc = PDI_state.desc(name);
	desc.share(buffer, &free, access & PDI_OUT, access & PDI_IN);
	Data_ref ref = desc.ref();
	
	// Provide reference to the plug-ins
	for (auto &&plugin : PDI_state.plugins) {
		try { // ignore errors here, try our best to notify everyone
			//TODO: concatenate errors in some way
			plugin.second->data(name, ref);
		} catch (const std::exception &e) {
			cerr << "Error while sharing " << name << " for plugin " << plugin.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while sharing " << name << " for plugin " << plugin.first << endl;
			//TODO: remove the faulty plugin?
		}
	}

	//TODO we should return concatenated errors here...
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_release(const char *name)
try {
	Try_pc fw;
	PDI_state.desc(name).release();
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_reclaim(const char *name)
try {
	Try_pc fw;
	PDI_state.desc(name).reclaim();
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_expose(const char *name, void *data, PDI_inout_t access)
{
	Try_pc fw;
	if (PDI_status_t status = PDI_share(name, data, access)) return status;
	
	if (! PDI_state.transaction.empty()) {   // defer the reclaim
		PDI_state.transaction_data.insert(name);
	} else { // do the reclaim now
		if (PDI_status_t status = PDI_reclaim(name)) return status;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_transaction_begin(const char *c_name)
try {
	Try_pc fw;
	if (!PDI_state.transaction.empty()) {
		throw Error{PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"};
	}
	PDI_state.transaction = c_name;
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_transaction_end()
try {
	Try_pc fw;
	if (PDI_state.transaction.empty()) {
		throw Error{PDI_ERR_STATE, "No transaction in progress, cannot end one"};
	}
	
	PDI_event(PDI_state.transaction.c_str());
	
	for (const string &data : PDI_state.transaction_data) {
		//TODO we should concatenate errors here...
		PDI_reclaim(data.c_str());
	}
	PDI_state.transaction_data.clear();
	PDI_state.transaction.clear();
	
	return PDI_OK;
} catch (const Error &e) {
	return return_err(e);
}

PDI_status_t PDI_export(const char *name, const void *data)
{
	if (PDI_status_t status = PDI_share(name, (void *)data, PDI_OUT)) return status;
	if (PDI_status_t status = PDI_release(name)) return status;
	return PDI_OK;
}
