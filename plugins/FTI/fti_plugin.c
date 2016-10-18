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

#include <string.h>
#include <mpi.h>
#include <fti.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/datatype.h>


typedef struct {
	PDI_data_t *data;
	int fti_id;
} protected_data_t;

int nb_protected = 0;
protected_data_t *protected = NULL;

int nb_snapshot_events = 0;
char **snapshot_events = NULL;


PDI_status_t PDI_fti_plugin_init(PC_tree_t conf, MPI_Comm *world)
{
	char *fti_file; PC_string(PC_get(conf, ".config_file"), &fti_file);
	
	nb_protected = 0;
	protected = NULL;
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	for ( int ii=0; ii<PDI_state.nb_data; ++ii ) {
		PDI_data_t *data = &PDI_state.data[ii];
		int fti_id;
		if ( !PC_int(PC_get(data->config, ".fti_id"),&fti_id) ) {
			++nb_protected;
			protected = realloc(protected, nb_protected*sizeof(protected_data_t));
			protected[nb_protected-1].data = data;
			protected[nb_protected-1].fti_id = fti_id;
		}
	}
	
	nb_snapshot_events = 1;
	snapshot_events = malloc(sizeof(char*));
	if ( !PC_string(PC_get(conf, ".snapshot_on"), snapshot_events) ) {
	} else if ( ! PC_len(PC_get(conf, ".snapshot_on"), &nb_snapshot_events) ) {
		snapshot_events = realloc(snapshot_events, nb_snapshot_events * sizeof(char*));
		for ( int ii=0; ii<nb_snapshot_events; ++ii ) {
			PC_string(PC_get(conf, ".snapshot_on[%d]", ii), &snapshot_events[ii]);
		}
	} else {
		fprintf(stderr, "** Warning: [PDI/FTI] missing \"snapshot_on\"\n");
		nb_snapshot_events = 0;
		free(snapshot_events);
	}
	PC_errhandler(pc_handler); // aka PC_end_try
	
	FTI_Init(fti_file, *world);
	free(fti_file);
	*world = FTI_COMM_WORLD;
	return PDI_OK;
}


PDI_status_t PDI_fti_plugin_finalize()
{
	for ( int ii=0; ii<nb_snapshot_events; ++ii ) free(snapshot_events[ii]);
	free(snapshot_events);
	free(protected);
	FTI_Finalize();
	return PDI_OK;
}


PDI_status_t PDI_fti_plugin_event(const char *event)
{
	int do_snapshot = 0;
	for ( int ii=0; ii<nb_snapshot_events; ++ii ) {
		if ( !strcmp(event, snapshot_events[ii]) ) {
			do_snapshot = 1;
			break;
		}
	}
	if( do_snapshot ) {
		for ( int ii = 0; ii<nb_protected; ++ii ) {
			PDI_data_t *data = protected[ii].data;
			if ( data->nb_content && (data->content[data->nb_content-1].access & PDI_OUT) ) {
				int size; PDI_data_size(&protected[ii].data->type, &size);
				//TODO: handle non-contiguous data correctly
				FTI_Protect(protected[ii].fti_id, data->content[data->nb_content-1].data, size, FTI_CHAR);
			} else {
				FTI_Protect(protected[ii].fti_id, "", 0, FTI_CHAR);
				fprintf(stderr,
						"** Warning: [PDI/FTI] Protected variable %s unavailable\n",
						protected[ii].data->name);
			}
		}
		FTI_Snapshot();
	}
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_start(PDI_data_t *data)
{
	data=data; // prevent unused warning
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_end(PDI_data_t *data)
{
	data=data; // prevent unused warning
	return PDI_OK;
}

PDI_PLUGIN(fti_plugin)
