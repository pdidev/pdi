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

#include "paraconf.h"

#include "pdi.h"
#include "pdi/plugin.h"

#include "state.h"
#include "plugin_loader.h"
#include "conf.h"

PDI_state_t PDI_state;

PDI_status_t PDI_init(yaml_document_t *document, yaml_node_t* conf, MPI_Comm* world)
{
	PDI_status_t err = PDI_OK;
	PDI_state.nb_metadata = 0;
	PDI_state.metadata = NULL;
	PDI_state.nb_data = 0;
	PDI_state.data = NULL;
	PDI_state.nb_plugins = 0;
	PDI_state.plugins = NULL;
	
	err = load_conf(document, conf); if (err) goto init_err;
	
	err = PC_get_len(document, conf, ".plugins", &PDI_state.nb_plugins); if (err) goto init_err;
	PDI_state.plugins = malloc(PDI_state.nb_plugins*sizeof(PDI_plugin_t));
	
	int ii;
	for ( ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		yaml_node_t *plugin_conf; err = PC_get(document, conf, ".plugins[%d]", &plugin_conf, ii); if (err) goto init_err;
		err = plugin_loader_load(document, plugin_conf, world, &PDI_state.plugins[ii]); if (err) goto init_err;
	}
	
	
init_err:
	return err;
}

PDI_status_t PDI_finalize()
{
	PDI_status_t err = PDI_OK;
	
	int ii;
	for ( ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		err = PDI_state.plugins[ii].finalize(); if (err) goto finalize_err0;
	}
	
finalize_err0:
	return err;
}

PDI_status_t PDI_event(const char* event)
{
	return PDI_OK;
}

PDI_status_t PDI_share(const char* name, void* data, int access)
{
	if ( access && PDI_IN ) return PDI_UNAVAILABLE;
	return PDI_OK;
}

PDI_status_t PDI_release(const char* name)
{
	return PDI_OK;
}

PDI_status_t PDI_reclaim(const char* name)
{
	return PDI_OK;
}

PDI_status_t PDI_expose(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_export(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_import(const char* name, void* data)
{
	return PDI_UNAVAILABLE;
}
 