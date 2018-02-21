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

#include <mpi.h>

#include <cassert>
#include <climits>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <fti.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/data_type.h>
#include <pdi/data_descriptor.h>
#include <pdi/data_reference.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>


namespace {

using PDI::Context;
using PDI::Data_ref;
using PDI::Data_r_ref;
using PDI::Data_w_ref;
using PDI::Error;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using std::cerr;
using std::endl;
using std::make_pair;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

enum Event_action { NO_EVENT=0, RECOVER, SNAPSHOT, CHECKPOINT, RESTART_STATUS };

unordered_map<string, long> fti_protected;

unordered_map<string, Event_action> events;


void PDI_fti_plugin_init(Context& ctx, PC_tree_t conf, MPI_Comm *world)
{
	for ( auto&& iter : ctx) {
		try {
			fti_protected.emplace(iter.name(), to_long(PC_get(iter.config(), ".fti_id")));
		} catch ( const Error& ) {}
	}
	
	try {
		events.emplace(to_string(PC_get(conf, ".snapshot_on")), SNAPSHOT);
	} catch ( const Error& ) {
		int nb_event;
		try {
			nb_event = len(PC_get(conf, ".snapshot_on"));
		} catch ( const Error& ) {
			nb_event = 0;
		}
		for ( int ii=0; ii<nb_event; ++ii ) {
			events.emplace(to_string(PC_get(conf, ".snapshot_on[%d]", ii)), SNAPSHOT);
		}
	}
	
	try {
		events.emplace(to_string(PC_get(conf, ".recover_on")), RECOVER);
	} catch ( const Error& ) {
		int nb_event;
		try {
			nb_event = len(PC_get(conf, ".recover_on"));
		} catch ( const Error& ) {
			nb_event = 0;
		}
		for ( int ii=0; ii<nb_event; ++ii ) {
			events.emplace(to_string(PC_get(conf, ".recover_on[%d]", ii)), RECOVER);
		}
	}
	
	try {
		events.emplace(to_string(PC_get(conf, ".checkpoint_on")), CHECKPOINT);
	} catch ( const Error& ) {
		int nb_event;
		try {
			nb_event = len(PC_get(conf, ".checkpoint_on"));
		} catch ( const Error& ) {
			nb_event = 0;
		}
		for ( int ii=0; ii<nb_event; ++ii ) {
			events.emplace(to_string(PC_get(conf, ".checkpoint_on[%d]", ii)), CHECKPOINT);
		}
	}
	
	try {
		events.emplace(to_string(PC_get(conf, ".datastart.restart_status")), RESTART_STATUS);
	} catch ( const Error& ) {
		int nb_event;
		try {
			nb_event = len(PC_get(conf, ".datastart.restart_status"));
		} catch ( const Error& ) {
			nb_event = 0;
		}
		for ( int ii=0; ii<nb_event; ++ii ) {
			events.emplace(to_string(PC_get(conf, ".datastart.restart_status[%d]", ii)), RESTART_STATUS);
		}
	}
	
	FTI_Init(const_cast<char*>(to_string(PC_get(conf, ".config_file")).c_str()), *world);
	
	*world = FTI_COMM_WORLD;
}

void PDI_fti_plugin_finalize(Context&)
{
	events.clear();
	fti_protected.clear();
	FTI_Finalize();
}


void PDI_fti_plugin_event ( Context& ctx, const char *event_name )
{
	auto&& evit = events.find(event_name);
	if ( evit == events.end() ) return;
	
	Event_action event = evit->second;
	if ( event == RESTART_STATUS ) return;
	
	/* the direction we need to access the data depending on whether this is a
		*			recovery or a checkpoint write */
	bool output = true;
	if ( ( event == SNAPSHOT && FTI_Status() ) || event == RECOVER ) {
		output = false;
	}
	
	for ( auto&& protected_var: fti_protected ) {
		if ( output ) {
			if ( Data_r_ref ref = ctx.desc(protected_var.first).ref() ) {
				size_t size = ref.type().datasize();
				//TODO: handle non-contiguous data correctly
				FTI_Protect(protected_var.second, const_cast<void*>(ref.get()), size, FTI_CHAR);
			} else {
				FTI_Protect(protected_var.second, NULL, 0, FTI_CHAR);
				fprintf(stderr,
								"** Warning: [PDI/FTI] Protected variable %s unavailable\n",
								protected_var.first.c_str());
			}
		} else {
			if ( Data_w_ref ref = ctx.desc(protected_var.first).ref() ) {
				size_t size = ref.type().datasize();
				//TODO: handle non-contiguous data correctly
				FTI_Protect(protected_var.second, ref.get(), size, FTI_CHAR);
			} else {
				FTI_Protect(protected_var.second, NULL, 0, FTI_CHAR);
				fprintf(stderr,
								"** Warning: [PDI/FTI] Protected variable %s unavailable\n",
								protected_var.first.c_str());
			}
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

void PDI_fti_plugin_data(Context& ctx, const char* name, Data_ref cref)
{
	auto&& evit = events.find(name);
	if ( evit == events.end() ) return;
	if ( evit->second != RESTART_STATUS )  return;
	if ( Data_w_ref ref = cref ) {
		*(int*)ref.get() = FTI_Status();
	}
}

} // namespace <anonymous>

PDI_PLUGIN(fti_plugin)
