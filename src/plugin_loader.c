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
  
//The following is used for doxygen documentation:
 /**
 * \file plugin_loader.c
 * \brief Contains function to load plugins
 * \details Plugins name written in config.yml are read/parse by paraconf: if PDI is build with a plugins having the same name, the plugin is loaded.
 * \author J. Bigot (CEA)
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <dlfcn.h>
#include <errno.h>
#include <stdarg.h>

#include <paraconf.h>

#include "pdi/state.h"
#include "pdi/plugin.h"

#include "status.h"
#include "utils.h"

#include "plugin_loader.h"

typedef PDI_status_t (*init_f)(PC_tree_t conf, MPI_Comm *world, PDI_plugin_t* plugin);

PDI_status_t plugin_loader_load(char *plugin_name, PC_tree_t node, MPI_Comm *world, PDI_plugin_t* plugin)
{
	PDI_status_t status = PDI_OK;
	
	char *plugin_symbol = msprintf("PDI_plugin_%s_ctor", plugin_name);
	void *plugin_ctor_uncast = dlsym(NULL, plugin_symbol);
	
	// case where the library was not prelinked
	if ( !plugin_ctor_uncast ) {
		char *libname = msprintf("lib%s.so", plugin_name);
		void *lib_handle = dlopen(libname, RTLD_NOW);
		free(libname);
		if ( !lib_handle ) {
			handle_error(make_error(PDI_ERR_PLUGIN, "Unable to load plugin file for `%s': %s", plugin_name, dlerror()), err1);
		}
		plugin_ctor_uncast = dlsym(lib_handle, plugin_symbol);
		if ( !plugin_ctor_uncast ) {
			handle_error(make_error(PDI_ERR_PLUGIN, "Unable to load plugin ctor for `%s': %s", plugin_name, dlerror()), err1);
		}
	}
	free(plugin_symbol);
	
	// ugly data to function ptr cast to be standard compatible (though undefined behavior)
	init_f plugin_ctor = *((init_f *)&plugin_ctor_uncast);
	handle_error(plugin_ctor(node, world, plugin), err0);
	
	return status;
	
err1:
	free(plugin_symbol);
	
err0:
	return status;
}

PDI_status_t plugin_loader_tryload( PC_tree_t conf, int plugin_id, MPI_Comm *world )
{
	PDI_status_t status = PDI_OK;
	int msg_done = 0;

	char *plugin_name = NULL;
	handle_PC_err(PC_string(PC_get(conf, ".plugins{%d}", plugin_id), &plugin_name), err0);
	
	PC_tree_t plugin_conf = PC_get(conf, ".plugins<%d>", plugin_id);
	handle_PC_err(PC_status(plugin_conf), err1);
	
	PDI_state.plugins = realloc(PDI_state.plugins, sizeof(PDI_plugin_t)*(PDI_state.nb_plugins+1));
	handle_error(plugin_loader_load(plugin_name, plugin_conf, world, &PDI_state.plugins[PDI_state.nb_plugins]), err1);
	++PDI_state.nb_plugins;
	
	free(plugin_name);
	return status;
	
err1:
	status = make_error(status,
			"Error while loading plugin `%s': %s",
			plugin_name,
			PDI_errmsg()
	);
	msg_done = 1;
	free(plugin_name);
	
err0:
	if (!msg_done) {
		status = make_error(status,
				"Error while loading plugin #%d: %s",
				plugin_id,
				PDI_errmsg()
		);
	}
	return status;
}
