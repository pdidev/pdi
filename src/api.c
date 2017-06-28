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

/**
\file api.c
\brief PDI public API functions (init, event, ... finalize).
\author J. Bigot (CEA)
**/

#include "config.h"

#ifdef STRDUP_WORKS
	#define _POSIX_C_SOURCE 200809L
#endif
#include <string.h>
#include <stddef.h>

#include "paraconf.h"

#include "pdi.h"
#include "pdi/plugin.h"
#include "pdi/state.h"
#include "pdi/data.h"
#include "pdi/datatype.h"
#include "conf.h"
#include "plugin_loader.h"
#include "status.h"
#include "utils.h"

#define PDI_BUFFER_SIZE 256


PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm *world)
{
	PDI_status_t status = PDI_OK;
	PDI_state.nb_data = 0;
	PDI_state.data = NULL;
	PDI_state.transaction = NULL;
	PDI_state.nb_transaction_data = 0;
	PDI_state.transaction_data = NULL;
	PDI_state.nb_plugins = 0;
	PDI_state.plugins = NULL;
	
	PDI_handle_err(load_conf(conf), err0);
	
	int nb_plugins; handle_PC_err(PC_len(PC_get(conf, ".plugins"), &nb_plugins), err0);
	PDI_state.plugins = malloc(nb_plugins * sizeof(PDI_plugin_t));
	
	for (int ii = 0; ii < nb_plugins; ++ii) {
		PDI_state.PDI_comm = *world;
		//TODO we should concatenate errors here...
		PDI_handle_err(plugin_loader_tryload(conf, ii, world), err0);
	}
	
	if (MPI_Comm_dup(*world, &PDI_state.PDI_comm)) {
		PDI_handle_err(PDI_make_err(PDI_ERR_SYSTEM, "Unable to clone the main communicator"), err0);
	}
	
	return status;
	
err0:
	PDI_finalize();
	return status;
}


PDI_status_t PDI_finalize()
{
	PDI_status_t status = PDI_OK;
	
	// Don't stop on errors in finalize, try to do our best
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	
	for (int ii = 0; ii < PDI_state.nb_plugins; ++ii) {
		PDI_state.plugins[ii].finalize();
	}
	free(PDI_state.plugins);
	PDI_state.nb_plugins = 0;
	PDI_state.plugins = NULL; // help valgrind
	
	for (int ii = 0; ii < PDI_state.nb_data; ++ii) {
		PDI_data_destroy(&PDI_state.data[ii]);
	}
	free(PDI_state.data);
	PDI_state.nb_data = 0;
	PDI_state.data = NULL; // help valgrind
	
	PDI_errhandler(errh);
	
	//TODO we should concatenate errors here...
	return status;
}


PDI_status_t PDI_event(const char *event)
{
	PDI_status_t status = PDI_OK;
	
	for (int ii = 0; ii < PDI_state.nb_plugins; ++ii) {
		//TODO we should concatenate errors here...
		PDI_handle_err(PDI_state.plugins[ii].event(event), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_access(const char *name, void **buffer, PDI_inout_t inout)
{
	PDI_data_t *data = PDI_find_data(name);
	*buffer = NULL;
	if (data) {
		if (data->nb_content > 0) {
			PDI_inout_t access = data->content[data->nb_content - 1].access;
			switch (inout) {
			case PDI_OUT:
				if (access & PDI_OUT) {
					*buffer = data->content[data->nb_content - 1].data;
					return PDI_OK;
				} break;
			case PDI_IN:
				if (access & PDI_IN) {
					*buffer = data->content[data->nb_content - 1].data;
					return PDI_OK;
				} break;
			case PDI_INOUT:
				if ((access & PDI_OUT) && (access & PDI_IN)) {
					*buffer = data->content[data->nb_content - 1].data;
					return PDI_OK;
				} break;
			}
		}
	}
	
	return PDI_UNAVAILABLE;
}


PDI_status_t PDI_share(const char *name, void *data_dat, PDI_inout_t access)
{
	PDI_status_t status = PDI_OK;
	
	PDI_data_t *data = PDI_find_data(name);
	if (data) {
		if (data->nb_content > 0 && data->kind & PDI_DK_METADATA) {
			// for metadata, unlink happens on share
			PDI_data_unlink(data, data->nb_content - 1);
		}
		
		// insert the new value
		++data->nb_content;
		data->content = realloc(data->content, data->nb_content * sizeof(PDI_data_value_t));
		data->content[data->nb_content - 1].data = data_dat;
		
		if (access & PDI_OUT) {
			data->content[data->nb_content - 1].access = PDI_OUT;
			for (int ii = 0; ii < PDI_state.nb_plugins; ++ii) {
				PDI_handle_err(PDI_state.plugins[ii].data_start(data), err0);
			}
		}
		if (access & PDI_IN) {
			data->content[data->nb_content - 1].access = PDI_IN;
			status = PDI_UNAVAILABLE;
			for (int ii = 0; ii < PDI_state.nb_plugins && status == PDI_UNAVAILABLE; ++ii) {
				status = PDI_OK;
				PDI_handle_err(PDI_state.plugins[ii].data_start(data), err0);
			}
		}
		data->content[data->nb_content - 1].access = access;
	} else {
		//TODO: create a data with unknown type here
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
	return status;
}



PDI_status_t PDI_release(const char *name)
{
	PDI_status_t status = PDI_OK;
	
	PDI_data_t *data = PDI_find_data(name);
	if (data) {
		if (data->nb_content == 0) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Cannot release a non shared value"), err0);
		}
		data->content[data->nb_content - 1].access |= PDI_MM_FREE;
		if (!(data->kind & PDI_DK_METADATA)) {
			// metadata is unlinked at share, not at release
			PDI_data_unlink(data, data->nb_content - 1);
		}
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_reclaim(const char *name)
{
	PDI_status_t status = PDI_OK;
	
	PDI_data_t *data = PDI_find_data(name);
	if (data) {
		if (data->nb_content == 0) {
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Cannot reclaim a non shared value"), err0);
		}
		if ((data->kind & PDI_DK_METADATA)
		    && (data->content[data->nb_content - 1].access & PDI_OUT)) {
			// keep a copy of the last exposed value of the data
			data->content[data->nb_content - 1].access |= PDI_MM_FREE | PDI_MM_COPY;
			PDI_datatype_t newtype; PDI_handle_err(PDI_datatype_densify(&newtype, &data->type), err0);
			size_t dsize; PDI_handle_err(PDI_datatype_buffersize(&newtype, &dsize), err0);
			void *newval; newval = malloc(dsize);
			PDI_handle_err(PDI_buffer_copy(
			                   newval,
			                   &newtype,
			                   data->content[data->nb_content - 1].data,
			                   &data->type),
			               err1);
			data->content[data->nb_content - 1].data = newval;
			
			PDI_datatype_destroy(&newtype);
			
err1:
			if (status && status != PDI_UNAVAILABLE) {
				PDI_datatype_destroy(&newtype);
				free(newval);
				PDI_handle_err(status, err0);
			}
		} else {
			PDI_data_unlink(data, data->nb_content - 1);
		}
	} else {
		status = PDI_UNAVAILABLE;
	}
	
	return status;
	
err0:
	return status;
}

static void add_to_transaction(const char *name)
{
	PDI_data_t *data = PDI_find_data(name);
	if (data) {
		++PDI_state.nb_transaction_data;
		PDI_state.transaction_data = realloc(
		                                 PDI_state.transaction_data,
		                                 PDI_state.nb_transaction_data * sizeof(PDI_data_t *));
		PDI_state.transaction_data[PDI_state.nb_transaction_data - 1] = data;
	}
	return;
}


PDI_status_t PDI_expose(const char *name, const void *data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, (void *)data, PDI_OUT), err0);
	
	if (PDI_state.transaction) {   // defer the reclaim
		add_to_transaction(name);
	} else { // do the reclaim now
		PDI_handle_err(PDI_reclaim(name), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_exchange(const char *name, void *data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, data, PDI_IN | PDI_OUT), err0);
	
	if (PDI_state.transaction) {   // defer the reclaim
		add_to_transaction(name);
	} else { // do the reclaim now
		PDI_handle_err(PDI_reclaim(name), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_transaction_begin(const char *name)
{
	PDI_status_t status = PDI_OK;
	
	if (PDI_state.transaction) {
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
	
	if (!PDI_state.transaction) {
		PDI_handle_err(PDI_make_err(PDI_ERR_STATE, "No transaction in progress, cannot end one"), err0);
	}
	
	PDI_event(PDI_state.transaction);
	for (int ii = 0; ii < PDI_state.nb_transaction_data; ii++) {
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


PDI_status_t PDI_export(const char *name, const void *data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, (void *)data, PDI_OUT), err0);
	PDI_handle_err(PDI_release(name), err0);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_import(const char *name, void *data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, data, PDI_IN), err0);
	
	if (PDI_state.transaction) {   // defer the reclaim
		add_to_transaction(name);
	} else { // do the reclaim now
		PDI_handle_err(PDI_reclaim(name), err0);
	}
	
	return status;
	
err0:
	return status;
}
