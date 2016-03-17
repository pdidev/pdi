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
#include "pdi/state.h"
#include "conf.h"
#include "plugin_loader.h"
#include "status.h"
#include "utils.h"

#define PDI_BUFFER_SIZE 256

PDI_state_t PDI_state;

PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm* world)
{
	PDI_status_t status = PDI_OK;
	
	PDI_state.nb_params = 0;
	PDI_state.params = NULL;
	PDI_state.nb_variables = 0;
	PDI_state.variables = NULL;
	PDI_state.nb_plugins = 0;
	PDI_state.plugins = NULL;
	
	handle_PC_err(conf.status, err0);
	
	handle_err(load_conf(conf), err0);
	
	handle_PC_err(PC_len(PC_get(conf, ".plugins"), &PDI_state.nb_plugins), err0);
	PDI_state.plugins = malloc(PDI_state.nb_plugins*sizeof(PDI_plugin_t));
	
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		char *plugin_name = NULL;
		handle_PC_err(PC_string(PC_get(conf, ".plugins{%d}", ii), &plugin_name), err0);
		
		PC_tree_t plugin_conf = PC_get(conf, ".plugins<%d>", ii);
		handle_PC_err(PC_status(plugin_conf), err1);
		
		handle_err(plugin_loader_load(plugin_name, plugin_conf, world, &PDI_state.plugins[ii]), err1);
		
err1:
		free(plugin_name);
		handle_err(status, err0);
	}
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_finalize()
{
	PDI_status_t status = PDI_OK;
	
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		if ( PDI_state.plugins[ii].finalize() ) status = PDI_ERR_PLUGIN;
	}
	
	return status;
}

PDI_status_t PDI_event(const char* event)
{
	PDI_status_t status = PDI_OK;
	
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		if ( PDI_state.plugins[ii].event(event) ) status = PDI_ERR_PLUGIN;
	}
	PDI_errhandler(errh);
	
	return status;
}

PDI_status_t PDI_share(const char* name, void* data_dat, int access)
{
	PDI_status_t status = PDI_OK;
	
	PDI_variable_t *data = NULL;
	for ( int ii=0; ii<PDI_state.nb_variables; ++ii ) {
		if ( strcmp(PDI_state.variables[ii].name, name) ) continue;
		
		data = PDI_state.variables+ii;
		break;
	}
	if (data) {
		//TODO: handle this case
		if ( access & PDI_IN ) {
			status = PDI_UNAVAILABLE;
		}
		if ( access & PDI_OUT ) {
			data->content.access = access;
			data->content.data = data_dat;
			
			PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
			for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
				if ( PDI_state.plugins[ii].data_start(data) ) status = PDI_ERR_PLUGIN;
			}
			PDI_errhandler(errh);
		}
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
}

PDI_status_t PDI_release(const char* name)
{
	PDI_status_t status = PDI_OK;
	
	PDI_variable_t *data = NULL;
	for ( int ii=0; ii<PDI_state.nb_variables; ++ii ) {
		if ( strcmp(PDI_state.variables[ii].name, name) ) continue;
		
		data = PDI_state.variables+ii;
		break;
	}
	if (data) {
		PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			if ( PDI_state.plugins[ii].data_end(data) ) status = PDI_ERR_PLUGIN;
		}
		PDI_errhandler(errh);
		
		free(data->content.data);
		data->content.data = NULL;
		data->content.access = 0;
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
}

PDI_status_t PDI_reclaim(const char* name)
{
	PDI_status_t status = PDI_OK;
	
	PDI_variable_t *data = NULL;
	for ( int ii=0; ii<PDI_state.nb_variables; ++ii ) {
		if ( strcmp(PDI_state.variables[ii].name, name) ) continue;
		
		data = PDI_state.variables+ii;
		break;
	}
	if (data) {
		PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			if ( PDI_state.plugins[ii].data_end(data) ) status = PDI_ERR_PLUGIN;
		}
		PDI_errhandler(errh);
		
		data->content.access = 0;
		data->content.data = NULL;
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
}

PDI_status_t PDI_expose(const char* name, const void* data_dat)
{
	PDI_status_t status = PDI_OK;
	
	PDI_param_t *param = NULL;
	for ( int ii=0; ii<PDI_state.nb_params; ++ii ) {
		if ( strcmp(name, PDI_state.params[ii].name) != 0 ) continue;
		
		param = PDI_state.params+ii;
		break;
	}
	if (param) {
		handle_err(PDI_copy(&param->type, &param->value, (void*)data_dat), err0);
	} else {
		handle_err(PDI_share(name, (void*)data_dat, PDI_OUT), err0);
		handle_err(PDI_reclaim(name), err0);
	}
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_export(const char* name, const void* data)
{
	PDI_status_t status = PDI_OK;
	
	handle_err(PDI_share(name, (void*)data, PDI_OUT), err0);
	handle_err(PDI_release(name), err0);
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_import(const char* name, void* data)
{
	PDI_status_t status = PDI_OK;
	
	handle_err(PDI_share(name, data, PDI_IN), err0);
	handle_err(PDI_reclaim(name), err0);
	
	return status;
err0:
	return status;
}
 