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

#include <cassert>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>
#include <mpi.h>
#include <fti.h>
#include <limits.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/datatype.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>


using std::cerr;
using std::endl;
using std::string;
using std::unique_ptr;
using std::unordered_set;
using std::vector;

typedef struct {
	PDI::Data_descriptor *desc;
	long fti_id;
} protected_data_t;

/// The types of events FTI supports
typedef enum { NO_EVENT=0, RECOVER, SNAPSHOT, CHECKPOINT, RESTART_STATUS } PDI_FTI_event_t;


vector<protected_data_t> fti_protected;

unordered_set<string> snapshot_events;

unordered_set<string> checkpoint_events;

unordered_set<string> recover_events;

unordered_set<string> restart_status_events;


PDI_status_t PDI_fti_plugin_init(PC_tree_t conf, MPI_Comm *world)
{
	char *fti_file; PC_string(PC_get(conf, ".config_file"), &fti_file);
	
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	for ( auto& iter : PDI_state.descriptors) {
		long fti_id;
		if ( !PC_int(PC_get(iter.second.get_config(), ".fti_id"), &fti_id) ) {
			fti_protected.push_back({&(iter.second), fti_id});
		}
	}
	
	// listing event name that trigger FTI snapshot
	{
		char *event_name;
		if ( !PC_string(PC_get(conf, ".snapshot_on"), &event_name) ) {
			snapshot_events.insert(event_name);
			free(event_name);
		}
	}
	if ( snapshot_events.empty() ) {
		int nb_event;
		if ( ! PC_len(PC_get(conf, ".snapshot_on"), &nb_event) ) {
			for ( int ii=0; ii<nb_event; ++ii ) {
				char *event_name;
				PC_string(PC_get(conf, ".snapshot_on[%d]", ii), &event_name);
				snapshot_events.insert(event_name);
				free(event_name);
			}
		}
	}
	
	{
		char *event_name;
		if ( !PC_string(PC_get(conf, ".recover_on"), &event_name) ) {
			recover_events.insert(event_name);
			free(event_name);
		}
	}
	if ( recover_events.empty() ) {
		int nb_event;
		if ( ! PC_len(PC_get(conf, ".recover_on"), &nb_event) ) {
			for ( int ii=0; ii<nb_event; ++ii ) {
				char *event_name;
				PC_string(PC_get(conf, ".recover_on[%d]", ii), &event_name);
				recover_events.insert(event_name);
				free(event_name);
			}
		}
	}
	
	{
		char *event_name;
		if ( !PC_string(PC_get(conf, ".datastart.restart_status"), &event_name) ) {
			restart_status_events.insert(event_name);
			free(event_name);
		}
	}
	if ( restart_status_events.empty() ) {
		int nb_event;
		if ( ! PC_len(PC_get(conf, ".datastart.restart_status"), &nb_event) ) {
			for ( int ii=0; ii<nb_event; ++ii ) {
				char *event_name;
				PC_string(PC_get(conf, ".datastart.restart_status[%d]", ii), &event_name);
				restart_status_events.insert(event_name);
				free(event_name);
			}
		}
	}
	
	{
		char *event_name;
		if ( !PC_string(PC_get(conf, ".checkpoint_on"), &event_name) ) {
			checkpoint_events.insert(event_name);
			free(event_name);
		}
	}
	if ( checkpoint_events.empty() ) {
		int nb_event;
		if ( ! PC_len(PC_get(conf, ".checkpoint_on"), &nb_event) ) {
			for ( int ii=0; ii<nb_event; ++ii ) {
				char *event_name;
				PC_string(PC_get(conf, ".checkpoint_on[%d]", ii), &event_name);
				checkpoint_events.insert(event_name);
				free(event_name);
			}
		}
	}
	
	PC_errhandler(pc_handler); // aka PC_end_try
	
	FTI_Init(fti_file, *world);
	
	free(fti_file);
	*world = FTI_COMM_WORLD;
	return PDI_OK;
}


PDI_status_t PDI_fti_plugin_finalize()
{
	snapshot_events.clear();
	recover_events.clear();
	restart_status_events.clear();
	checkpoint_events.clear();
	fti_protected.clear();
	FTI_Finalize();
	return PDI_OK;
}


PDI_status_t PDI_fti_plugin_event ( const char *event_name )
{
	PDI_FTI_event_t event = NO_EVENT;
	if ( snapshot_events.find(event_name) != snapshot_events.end() ) {
		event = SNAPSHOT; 
	}
	if ( checkpoint_events.find(event_name) != checkpoint_events.end() ) {
		event = CHECKPOINT; 
	}
	if ( recover_events.find(event_name) != recover_events.end() ) {
		event = RECOVER; 
	}
	
	if( event ) {
		/* the direction we need to access the data depending on whether this is a
		 *			recovery or a checkpoint write */
		PDI_inout_t direction = PDI_IN;
		if ( event == SNAPSHOT && !FTI_Status() ) {
			direction = PDI_OUT;
		}
		if ( event == CHECKPOINT ) {
			direction = PDI_OUT;
		}
		for ( auto& protected_var: fti_protected ) {
			PDI::Data_ref ref = PDI_find_ref(protected_var.desc->get_name());
			if ( ref.grant(direction) ) {
				size_t size; PDI_datatype_datasize(&ref.get_type(), &size);
				//TODO: handle non-contiguous data correctly
				FTI_Protect(protected_var.fti_id,
										ref.get(), size, FTI_CHAR);
			} else {
				FTI_Protect(protected_var.fti_id, NULL, 0, FTI_CHAR);
				fprintf(stderr,
								"** Warning: [PDI/FTI] Protected variable %s unavailable\n",
						protected_var.desc->get_name().c_str());
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

PDI_status_t PDI_fti_plugin_data_start(const std::string& name, PDI::Data_ref ref)
{
	if ( ref.priviledge(PDI_IN) && restart_status_events.find(name) != restart_status_events.end() ) {
		*(int*)ref.get() = FTI_Status();
	}
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_end(const std::string&, PDI::Data_ref)
{
	return PDI_OK;
}

PDI_PLUGIN(fti_plugin)
