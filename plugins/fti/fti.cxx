/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <limits>
#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <fti.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/data_descriptor.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/reference.h>


namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Error;
using PDI::len;
using PDI::Plugin;
using PDI::to_long;
using PDI::to_string;
using std::cerr;
using std::endl;
using std::make_pair;
using std::numeric_limits;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::unordered_set;
using std::vector;

struct fti_plugin:
	Plugin
{
	enum Event_action
	{ NO_EVENT=0, RECOVER, SNAPSHOT, CHECKPOINT, RESTART_STATUS };
	
	unordered_map<string, long> fti_protected;
	
	unordered_map<string, Event_action> events;
	
	fti_plugin(Context& ctx, PC_tree_t conf, MPI_Comm* world):
		Plugin{ctx}
	{
		for ( auto&& iter : ctx) {
			try {
				fti_protected.emplace(iter.name(), to_long(PC_get(iter.config(), ".fti_id")));
			} catch ( const Error& ) {}
		}
		
		PC_tree_t snapshot_on = PC_get(conf, ".snapshot_on");
		if ( !PC_status(snapshot_on) ) {
			try {
				events.emplace(to_string(snapshot_on), SNAPSHOT);
			} catch ( const Error& ) {
				int nb_event = len(snapshot_on, 0);
				for ( int ii=0; ii<nb_event; ++ii ) {
					events.emplace(to_string(PC_get(snapshot_on, "[%d]", ii)), SNAPSHOT);
				}
			}
		}
		
		PC_tree_t recover_on = PC_get(conf, ".recover_on");
		if ( !PC_status(recover_on) ) {
			try {
				events.emplace(to_string(recover_on), RECOVER);
			} catch ( const Error& ) {
				int nb_event = len(recover_on, 0);
				for ( int ii=0; ii<nb_event; ++ii ) {
					events.emplace(to_string(PC_get(recover_on, "[%d]", ii)), RECOVER);
				}
			}
		}
		
		PC_tree_t checkpoint_on = PC_get(conf, ".checkpoint_on");
		if ( !PC_status(checkpoint_on) ) {
			try {
				events.emplace(to_string(checkpoint_on), CHECKPOINT);
			} catch ( const Error& ) {
				int nb_event = len(checkpoint_on, 0);
				for ( int ii=0; ii<nb_event; ++ii ) {
					events.emplace(to_string(PC_get(checkpoint_on, "[%d]", ii)), CHECKPOINT);
				}
			}
		}
		
		PC_tree_t restart_status = PC_get(conf, ".datastart.restart_status");
		if ( !PC_status(restart_status) ) {
			try {
				events.emplace(to_string(restart_status), RESTART_STATUS);
			} catch ( const Error& ) {
				int nb_event = len(restart_status, 0);
				for ( int ii=0; ii<nb_event; ++ii ) {
					events.emplace(to_string(PC_get(restart_status, "[%d]", ii)), RESTART_STATUS);
				}
			}
		}
		
		FTI_Init(const_cast<char*>(to_string(PC_get(conf, ".config_file")).c_str()), *world);
		
		*world = FTI_COMM_WORLD;
	}
	
	~fti_plugin()
	{
		FTI_Finalize();
	}
	
	void event(const char* event_name) override
	{
		auto&& evit = events.find(event_name);
		if ( evit == events.end() ) return;
		
		Event_action event = evit->second;
		if ( event == RESTART_STATUS ) return;
		
		/* the direction we need to access the data depending on whether this is a
		    *           recovery or a checkpoint write */
		bool output = true;
		if ( ( event == SNAPSHOT && FTI_Status() ) || event == RECOVER ) {
			output = false;
		}
		
		for ( auto&& protected_var: fti_protected ) {
			if ( output ) {
				if ( Ref_r ref = context().desc(protected_var.first).ref() ) {
					size_t size = ref.type().datasize();
					//TODO: handle non-contiguous data correctly
					FTI_Protect(static_cast<int>(protected_var.second), const_cast<void*>(ref.get()), static_cast<long>(size), FTI_CHAR);
				} else {
					FTI_Protect(static_cast<int>(protected_var.second), NULL, 0, FTI_CHAR);
					cerr << "** Warning: [PDI/FTI] Protected variable "<<protected_var.first<<" unavailable"<<endl;
				}
			} else {
				if ( Ref_w ref = context().desc(protected_var.first).ref() ) {
					size_t size = ref.type().datasize();
					//TODO: handle non-contiguous data correctly
					FTI_Protect(static_cast<int>(protected_var.second), ref.get(), static_cast<long>(size), FTI_CHAR);
				} else {
					FTI_Protect(static_cast<int>(protected_var.second), NULL, 0, FTI_CHAR);
					cerr << "** Warning: [PDI/FTI] Protected variable "<<protected_var.first<<" unavailable"<<endl;
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
			FTI_Checkpoint(numeric_limits<int>::max(), 4);
			break;
		case RESTART_STATUS: case NO_EVENT:
			assert(0 && "Unexpected event type");
		}
	}
	
	void data(const char* name, Ref cref) override
	{
		auto&& evit = events.find(name);
		if ( evit == events.end() ) return;
		if ( evit->second != RESTART_STATUS )  return;
		if ( Ref_w ref = cref ) {
			*static_cast<int*>(ref.get()) = FTI_Status();
		}
	}
	
}; // struct fti_plugin

} // namespace <anonymous>

PDI_PLUGIN(fti)
