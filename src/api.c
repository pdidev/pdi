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
	
	throw_error(load_conf(conf), err0);
	
	int nb_plugins; handle_PC_err(PC_len(PC_get(conf, ".plugins"), &nb_plugins), err0);
	PDI_state.plugins = malloc(nb_plugins*sizeof(PDI_plugin_t));
	
	for ( int ii=0; ii<nb_plugins; ++ii ) {
		PDI_state.PDI_comm = *world;
		//TODO we should concatenate errors here...
		throw_error(plugin_loader_tryload(conf, ii, world), err0);
	}
	
	if ( MPI_Comm_dup(*world, &PDI_state.PDI_comm) ) {
		throw_error(make_error(PDI_ERR_SYSTEM, "Unable to clone the main communicator"), err0);
	}

	return status;
	
err0:
	PDI_finalize();
	return status;
}


PDI_status_t PDI_param_destroy(PDI_param_t *param)
{
	PDI_status_t status = PDI_OK;
	
	free(param->name);
	free(param->value);
	throw_error(PDI_datatype_destroy(&param->type), err0);
	
	return status;
	
err0:
	return status;
}

PDI_status_t PDI_variable_destroy(PDI_variable_t *var)
{
	PDI_status_t status = PDI_OK;
	
	free(var->name);
	throw_error(PDI_datatype_destroy(&var->type), err0);
	
	return status;
	
err0:
	return status;
}

PDI_status_t PDI_finalize()
{
	PDI_status_t status = PDI_OK;
	
	// Don't stop on errors in finalize, try to do our best
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		PDI_state.plugins[ii].finalize();
	}
	free(PDI_state.plugins);
	PDI_state.plugins = NULL; // help valgrind
	
	for (int ii=0; ii<PDI_state.nb_params; ++ii) {
		PDI_param_destroy(&PDI_state.params[ii]);
	}
	free(PDI_state.params);
	PDI_state.params = NULL; // help valgrind
	
	for (int ii=0; ii<PDI_state.nb_variables; ++ii) {
		PDI_variable_destroy(&PDI_state.variables[ii]);
	}
	free(PDI_state.variables);
	PDI_state.variables = NULL; // help valgrind
	
	PDI_errhandler(errh);
	
	//TODO we should concatenate errors here...
	return status;
}

PDI_status_t PDI_event(const char* event)
{
	PDI_status_t status = PDI_OK;
	
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		//TODO we should concatenate errors here...
		throw_error(PDI_state.plugins[ii].event(event), err0);
	}
	
	return status;
	
err0:
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
		if ( access & PDI_OUT ) {
			data->content.access = PDI_OUT;
			data->content.data = data_dat;
			for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
				throw_error(PDI_state.plugins[ii].data_start(data), err0);
			}
		}
		if ( access & PDI_IN ) {
			status = PDI_UNAVAILABLE;
			data->content.access = PDI_IN;
			for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
				PDI_status_t instatus = PDI_state.plugins[ii].data_start(data);
				throw_error(instatus, err0);
				if ( !instatus ) { // only one plugin for input
					status = PDI_OK;
					break;
				}
			}
		}
		data->content.access = access;
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
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
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			throw_error(PDI_state.plugins[ii].data_end(data), err0);
		}
		
		free(data->content.data);
		data->content.data = NULL;
		data->content.access = 0;
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
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
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			throw_error(PDI_state.plugins[ii].data_end(data), err0);
		}
		
		data->content.access = 0;
		data->content.data = NULL;
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
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
		int dsize; throw_error(PDI_data_size(&param->type, &dsize), err0);
		param->value = realloc(param->value, dsize);
		throw_error(tcopy(&param->type, param->value, (void*)data_dat), err0);
	} else {
		throw_error(PDI_share(name, (void*)data_dat, PDI_OUT), err0);
		throw_error(PDI_reclaim(name), err0);
	}
	
	return status;
	
err0:
	return status;
}

PDI_status_t PDI_export(const char* name, const void* data)
{
	PDI_status_t status = PDI_OK;
	
	throw_error(PDI_share(name, (void*)data, PDI_OUT), err0);
	throw_error(PDI_release(name), err0);
	
	return status;
	
err0:
	return status;
}

PDI_status_t PDI_import(const char* name, void* data)
{
	PDI_status_t status = PDI_OK;
	
	throw_error(PDI_share(name, data, PDI_IN), err0);
	throw_error(PDI_reclaim(name), err0);
	
	return status;
	
err0:
	return status;
}
