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
 * \file api.c
 * \brief PDI public API functions (init, event, ... finalize).
 * \author J. Bigot (CEA)
 */

#include <string.h>

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


static PDI_metadata_t *find_metadata( const char *name )
{
	PDI_metadata_t *metadata = NULL;
	for ( int ii=0; ii<PDI_state.nb_metadata; ++ii ) {
		if ( strcmp(name, PDI_state.metadata[ii].name) != 0 ) continue;
		
		metadata = PDI_state.metadata+ii;
		break;
	}
	return metadata;
}


static PDI_data_t *find_data( const char *name )
{
	PDI_data_t *data = NULL;
	for ( int ii=0; ii<PDI_state.nb_data; ++ii ) {
		if ( strcmp(PDI_state.data[ii].name, name) ) continue;
		data = PDI_state.data+ii;
		break;
	}
	return data;
}


PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm* world)
{
	PDI_status_t status = PDI_OK;
	PDI_state.nb_metadata = 0;
	PDI_state.metadata = NULL;
	PDI_state.nb_data = 0;
	PDI_state.data = NULL;
	PDI_state.transaction = NULL;
	PDI_state.nb_transaction_data = 0;
	PDI_state.transaction_data = NULL;
	PDI_state.nb_plugins = 0;
	PDI_state.plugins = NULL;
	
	PDI_handle_err(load_conf(conf), err0);
	
	int nb_plugins; handle_PC_err(PC_len(PC_get(conf, ".plugins"), &nb_plugins), err0);
	PDI_state.plugins = malloc(nb_plugins*sizeof(PDI_plugin_t));
	
	for ( int ii=0; ii<nb_plugins; ++ii ) {
		PDI_state.PDI_comm = *world;
		//TODO we should concatenate errors here...
		PDI_handle_err(plugin_loader_tryload(conf, ii, world), err0);
	}
	
	if ( MPI_Comm_dup(*world, &PDI_state.PDI_comm) ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_SYSTEM, "Unable to clone the main communicator"), err0);
	}

	return status;
	
err0:
	PDI_finalize();
	return status;
}


PDI_status_t PDI_metadata_destroy(PDI_metadata_t *metadata)
{
	PDI_status_t status = PDI_OK;
	
	free(metadata->name);
	free(metadata->value);
	PDI_handle_err(PDI_datatype_destroy(&metadata->type), err0);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_data_destroy(PDI_data_t *var)
{
	PDI_status_t status = PDI_OK;
	
	free(var->name);
	PDI_handle_err(PDI_datatype_destroy(&var->type), err0);
	
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
	
	for (int ii=0; ii<PDI_state.nb_metadata; ++ii) {
		PDI_metadata_destroy(&PDI_state.metadata[ii]);
	}
	free(PDI_state.metadata);
	PDI_state.metadata = NULL; // help valgrind
	
	for (int ii=0; ii<PDI_state.nb_data; ++ii) {
		PDI_data_destroy(&PDI_state.data[ii]);
	}
	free(PDI_state.data);
	PDI_state.data = NULL; // help valgrind
	
	PDI_errhandler(errh);
	
	//TODO we should concatenate errors here...
	return status;
}


PDI_status_t PDI_event(const char* event)
{
	PDI_status_t status = PDI_OK;
	
	for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
		//TODO we should concatenate errors here...
		PDI_handle_err(PDI_state.plugins[ii].event(event), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_share( const char* name, void* data_dat, int access )
{
	PDI_status_t status = PDI_OK;
	
	PDI_data_t *data = find_data(name);
	if (data) {
		if ( access & PDI_OUT ) {
			data->content.access = PDI_OUT;
			data->content.data = data_dat;
			for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
				PDI_handle_err(PDI_state.plugins[ii].data_start(data), err0);
			}
		}
		if ( access & PDI_IN ) {
			status = PDI_UNAVAILABLE;
			data->content.access = PDI_IN;
			for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
				PDI_status_t instatus = PDI_state.plugins[ii].data_start(data);
				PDI_handle_err(instatus, err0);
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
	
	PDI_data_t *data = find_data(name);
	if (data) {
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			PDI_handle_err(PDI_state.plugins[ii].data_end(data), err0);
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
	
	PDI_data_t *data = find_data(name);
	if (data) {
		for ( int ii=0; ii<PDI_state.nb_plugins; ++ii ) {
			PDI_handle_err(PDI_state.plugins[ii].data_end(data), err0);
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
	
	PDI_metadata_t *metadata = find_metadata(name);
	if (metadata) {
		int dsize; PDI_handle_err(PDI_data_size(&metadata->type, &dsize), err0);
		metadata->value = realloc(metadata->value, dsize);
		PDI_handle_err(tcopy(&metadata->type, metadata->value, (void*)data_dat), err0);
	} else {
		PDI_handle_err(PDI_share(name, (void*)data_dat, PDI_OUT), err0);
		if ( PDI_state.transaction ) { // defer the reclaim
			PDI_data_t *data = find_data(name);
			if ( data ) {
				++PDI_state.nb_transaction_data;
				PDI_state.transaction_data = realloc(
						PDI_state.transaction_data,
						PDI_state.nb_transaction_data * sizeof(PDI_data_t *) );
				PDI_state.transaction_data[PDI_state.nb_transaction_data-1] = data;
			}
		} else { // do the reclaim now
			PDI_handle_err(PDI_reclaim(name), err0);
		}
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_transaction_begin( const char *name )
{
	PDI_status_t status = PDI_OK;
	
	if ( PDI_state.transaction ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"), err0);
	}
	
	PDI_state.transaction = strdup(name);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_transaction_end()
{
	PDI_status_t status = PDI_OK;
	
	if ( !PDI_state.transaction ) {
		PDI_handle_err(PDI_make_err(PDI_ERR_STATE, "No transaction in progress, cannot end one"), err0);
	}
	
	PDI_event(PDI_state.transaction);
	for( int ii=0; ii<PDI_state.nb_transaction_data; ii++ ) {
		//TODO we should concatenate errors here...
		PDI_reclaim(PDI_state.transaction_data[ii]->name);
	}
	PDI_state.nb_transaction_data = 0;
	free(PDI_state.transaction_data);
	PDI_state.transaction_data = NULL;
	free(PDI_state.transaction);
	PDI_state.transaction = NULL;
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_export(const char* name, const void* data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, (void*)data, PDI_OUT), err0);
	PDI_handle_err(PDI_release(name), err0);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_import(const char* name, void* data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, data, PDI_IN), err0);
	PDI_handle_err(PDI_reclaim(name), err0);
	
	return status;
	
err0:
	return status;
}
