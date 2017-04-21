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

#include <assert.h>
#include <string.h>
#include <mpi.h>
#include <fti.h>
#include <limits.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/datatype.h>


typedef struct {
	PDI_data_t *data;
	int fti_id;
} protected_data_t;


/// The types of events FTI supports
typedef enum { NO_EVENT=0, RECOVER, SNAPSHOT, CHECKPOINT, RESTART_STATUS } PDI_FTI_event_t;

int nb_protected = 0;

protected_data_t *protected = NULL;

int nb_snapshot_events = 0;

char **snapshot_events = NULL;

int nb_checkpoint_events = 0;

char **checkpoint_events = NULL;

int nb_recover_events = 0;

char **recover_events = NULL;

int nb_restart_status_events = 0;

char **restart_status_events = NULL;

int restart_status = 0;



PDI_status_t PDI_fti_plugin_init(PC_tree_t conf, MPI_Comm *world)
{
	char *fti_file; PC_string(PC_get(conf, ".config_file"), &fti_file);
	
	nb_protected = 0;
	protected = NULL;
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	for ( int ii=0; ii<PDI_state.nb_data; ++ii ) {
		PDI_data_t *data = &PDI_state.data[ii];
		long fti_id;
		if ( !PC_int(PC_get(data->config, ".fti_id"),&fti_id) ) {
			++nb_protected;
			protected = realloc(protected, nb_protected*sizeof(protected_data_t));
			protected[nb_protected-1].data = data;
			protected[nb_protected-1].fti_id = fti_id;
		}
	}
	
	nb_snapshot_events = 1; // listing event name that trigger FTI snapshot 
	snapshot_events = malloc(sizeof(char*));
	if ( !PC_string(PC_get(conf, ".snapshot_on"), snapshot_events) ) {
	} else if ( ! PC_len(PC_get(conf, ".snapshot_on"), &nb_snapshot_events) ) {
		snapshot_events = realloc(snapshot_events, nb_snapshot_events * sizeof(char*));
		for ( int ii=0; ii<nb_snapshot_events; ++ii ) {
			PC_string(PC_get(conf, ".snapshot_on[%d]", ii), &snapshot_events[ii]);
		}
	} else {
		nb_snapshot_events = 0;
		free(snapshot_events);
		snapshot_events = NULL;
	}

	nb_recover_events = 1;
	recover_events = malloc(sizeof(char*));
	if ( !PC_string(PC_get(conf, ".recover_on"), recover_events) ) {
	} else if ( ! PC_len(PC_get(conf, ".recover_on"), &nb_recover_events) ) {
		recover_events = realloc(recover_events, nb_recover_events * sizeof(char*));
		for ( int ii=0; ii<nb_recover_events; ++ii ) {
			PC_string(PC_get(conf, ".recover_on[%d]", ii), &recover_events[ii]);
		}
	} else {
		nb_recover_events = 0;
		free(recover_events);
		recover_events = NULL;
	}

	nb_restart_status_events = 1;
	restart_status_events = malloc(sizeof(char*));
	if ( !PC_string(PC_get(conf, ".datastart.restart_status"), restart_status_events) ) {
	} else if ( ! PC_len(PC_get(conf, ".datastart.restart_status"), &nb_restart_status_events) ) {
		restart_status_events = realloc(restart_status_events, nb_restart_status_events * sizeof(char*));
		for ( int ii=0; ii<nb_restart_status_events; ++ii ) {
			PC_string(PC_get(conf, ".datastart.restart_status[%d]", ii), &restart_status_events[ii]);
		}
	} else {
		nb_restart_status_events = 0;
		free(restart_status_events);
		restart_status_events = NULL;
	}

	nb_checkpoint_events = 1; // listing event name that trigger FTI checkpoint 
	checkpoint_events = malloc(sizeof(char*));
	if ( !PC_string(PC_get(conf, ".checkpoint_on"), checkpoint_events) ) {
	} else if ( ! PC_len(PC_get(conf, ".checkpoint_on"), &nb_checkpoint_events) ) {
		checkpoint_events = realloc(checkpoint_events, nb_checkpoint_events * sizeof(char*));
		for ( int ii=0; ii<nb_checkpoint_events; ++ii ) {
			PC_string(PC_get(conf, ".checkpoint_on[%d]", ii), &checkpoint_events[ii]);
		}
	} else {
		nb_checkpoint_events = 0;
		free(checkpoint_events);
		checkpoint_events = NULL;
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
	for ( int ii=0; ii<nb_recover_events; ++ii ) free(recover_events[ii]);
	free(recover_events);
	for ( int ii=0; ii<nb_restart_status_events; ++ii ) free(restart_status_events[ii]);
	free(restart_status_events);
	for ( int ii=0; ii<nb_checkpoint_events; ++ii ) free(checkpoint_events[ii]);
	free(checkpoint_events);
	free(protected);
	FTI_Finalize();
	return PDI_OK;
}


PDI_status_t PDI_fti_plugin_event ( const char *event_name )
{
	PDI_FTI_event_t event = NO_EVENT;
	for ( int ii=0; ii<nb_snapshot_events && !event; ++ii ) {
		if ( !strcmp(event_name, snapshot_events[ii]) ) {
			event = SNAPSHOT; 
		}
	}
	for ( int ii=0; ii<nb_checkpoint_events && !event; ++ii ) {
		if ( !strcmp(event_name, checkpoint_events[ii]) ) {
			event = CHECKPOINT; 
		}
	}
	for ( int ii=0; ii<nb_recover_events && !event; ++ii ) {
		if ( !strcmp(event_name, recover_events[ii]) ) {
			event = RECOVER;
		}
	}
	
	if( event ) {
		/* the direction we need to access the data depending on whether this is a
		   recovery or a checkpoint write */
		PDI_inout_t direction = PDI_IN;
		if ( event == SNAPSHOT && !FTI_Status() ) {
			direction = PDI_OUT;
		}
		if ( event == CHECKPOINT ) {
			direction = PDI_OUT;
		}
		for ( int ii = 0; ii<nb_protected; ++ii ) {
			PDI_data_t *data = protected[ii].data;
			if ( data->nb_content
					&& (data->content[data->nb_content-1].access & direction) ) {
				size_t size; PDI_datatype_datasize(&protected[ii].data->type, &size);
				//TODO: handle non-contiguous data correctly
				FTI_Protect(protected[ii].fti_id,
						data->content[data->nb_content-1].data, size, FTI_CHAR);
			} else {
				FTI_Protect(protected[ii].fti_id, NULL, 0, FTI_CHAR);
				fprintf(stderr,
						"** Warning: [PDI/FTI] Protected variable %s unavailable\n",
						protected[ii].data->name);
			}
		}
		switch ( event ) {
		case SNAPSHOT:
			FTI_Snapshot();
			break;
		case RECOVER:
			FTI_Recover();
			break;
		case CHECKPOINT:
			FTI_Checkpoint(INT_MAX-1, 4);
			break;
		default:
			assert(0 && "Unexpected event type");
		}
	}
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_start(PDI_data_t *data)
{
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		int found_output = 0;
		for ( int ii=0; ii<nb_restart_status_events && !found_output; ++ii ) {
			if ( !strcmp(data->name, restart_status_events[ii]) ) {
				*(int*)data->content[data->nb_content-1].data = FTI_Status();
			}
		}
	}
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_end(PDI_data_t *data)
{
	data=data; // prevent unused warning
	return PDI_OK;
}

PDI_PLUGIN(fti_plugin)
