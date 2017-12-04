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

/*** This plug-in provides a set of basic fonctionalities such as:
 * - extracting subarrays for a N dimensionnal array
 * - transforming event into booleen variables
 * - giving the last event call
 *  ... etc.
 ***/

#include "config.h"

#include <cassert>
#include <cstring>
#include <set>
#include <string>
#include <unordered_set>
#include <vector>
#include <inttypes.h>

#include <mpi.h>
#include <sys/stat.h> // to check file existence

#include "pdi.h"
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/data_reference.h>
#include <pdi/datatype.h>
#include <pdi/value.h>

using PDI::Data_ref;
using PDI::Data_r_ref;
using PDI::Data_w_ref;
using PDI::Data_descriptor;
using PDI::Value;
using std::cerr;
using std::endl;
using std::set;
using std::string;
using std::unordered_set;
using std::vector;

/// Supported actions
typedef enum {
	EVENT2DATA=0,
	FILE_EXISTS,
	DIR_EXISTS,
	EXTRACT_SUBARRAY
} utils_action_t;

typedef struct utils_task_s
{
	utils_action_t action; ///<  The action to perform on event
	
	unordered_set<string> events; ///<  Events that trigger the previous action
	
	PDI_value_t in; ///<  An expression: could be a file name, variables, or expressions
	
	PDI_value_t out; ///<  An expression
	
	PDI_value_t select; ///<  select when to perform action (default is always)
	
	int32_t result; ///< the result of the last execution of the action
	
} utils_task_t;


/// Tasks
vector<utils_task_t> tasks;

/// Configuration file obtain from PDI
PC_tree_t my_conf;


/// Initialization: read configuration file and fill data structure
PDI_status_t PDI_utilities_init(PC_tree_t conf, MPI_Comm *world)
{
	(void) world; // prevent unused param warning
	my_conf = conf;
	
	if ( PC_status(my_conf) ) {
		return PDI_ERR_PLUGIN;
	}
	// Get the number of actions
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	int nb_tasks;
	if ( PC_len(my_conf, &nb_tasks) ) { // if our config is absent, simply return
		PC_errhandler(errh);
		return PDI_OK;
	}
	PC_errhandler(errh);
	
	// fill the utils_task_t data structure with corresponding values from conf file
	tasks.resize(nb_tasks);
	for ( int ii=0; ii<nb_tasks; ++ii){
		// Get the next node
		PC_tree_t treetmp = PC_get(my_conf, "<%d>", ii);
		
		
		// List of events (or single event)
		errh = PC_errhandler(PC_NULL_HANDLER);
		PC_tree_t all_events = PC_get(treetmp,".events");
		int nb_events;
		if( !PC_len(all_events, &nb_events) ){
			for( int nn=0; nn < nb_events; ++nn ){
				char *event;
				PC_string(PC_get(all_events, "{%d}", ii) , &event);
				tasks[ii].events.insert(event);
				free(event);
			}
		} else { // testing with one event
			char *event;
			PC_string(PC_get(treetmp,".event"), &event);
			tasks[ii].events.insert(event);
			free(event);
		}
		PC_errhandler(errh);
		assert(!tasks[ii].events.empty() && "[PDI/Utilities] Error: no value or invalid value for 'event' or 'events'.\n");
		
		// input and ouput
		// data 'in'
		char *in = NULL;
		if( !PC_string(PC_get(treetmp, ".in"), &in) ){
			tasks[ii].in = Value{in};
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'in'.\n");
			return PDI_ERR_CONFIG;
		}
		free(in);
		// data 'out'
		char* out = NULL;
		if( !PC_string(PC_get(treetmp, ".out"), &out) ){
			tasks[ii].out = Value{out};
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'out'.\n");
			return PDI_ERR_CONFIG;
		}
		free(out);
		
		// optional 'select'
		PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
		char* select;
		if ( !PC_string(PC_get(treetmp, ".select"), &select) ) {
			tasks[ii].select = Value{select};
			free(select);
		} else {
			// else apply default value of 1 (always true)
			tasks[ii].select = Value{"1"};
		}
		PC_errhandler(errh);
		
		// Select an existing task
		char* task = NULL;
		PC_string(PC_get(my_conf, "{%d}", ii) , &task);
		if ( !strcmp(task, "event2data") ){
			tasks[ii].action = EVENT2DATA;
		} else if ( !strcmp(task, "file_exists") ){
			tasks[ii].action = FILE_EXISTS;
		} else if ( !strcmp(task, "dir_exists") ){
			tasks[ii].action = DIR_EXISTS;
		} else if ( !strcmp(task, "extract_subarray") ){
			tasks[ii].action = EXTRACT_SUBARRAY;
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: Invalid task name, '%s'.\n", task);
			return PDI_ERR_CONFIG;
		}
		free(task);
	}
	
	return PDI_OK;
}


/// Finalize and free allocated data
PDI_status_t PDI_utilities_finalize()
{
	tasks.clear();
	return PDI_OK;
}


/// Traps and converts events into tasks
PDI_status_t PDI_utilities_event(const char *event_name) {
	for ( auto&& one_task: tasks ) {
		if ( one_task.events.find(event_name) != one_task.events.end() ) {
			long select = one_task.select.to_long();
			if ( !select ) continue;
			switch ( one_task.action ) {
			case EVENT2DATA:{
				// Should evaluate expression define in 'in' and set value in 'out'
				one_task.result = one_task.in.to_long();
			} break;
			case FILE_EXISTS: {
				// get filename from input into fname
				string fname = one_task.in;
				// check for file
				struct stat sb;
				if (stat(fname.c_str(), &sb) == 0 && S_ISREG(sb.st_mode)){
					one_task.result = 1;
				} else {
					one_task.result = 0;
				}
			} break;
			case DIR_EXISTS: {
				// get filename from input into str
				string fname = one_task.in;
				// check for file
				struct stat sb;
				if (stat(fname.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode)){
					one_task.result = 1;
				} else {
					one_task.result = 0;
				}
			} break;
			case EXTRACT_SUBARRAY:
				break;
			}
		}
	}
	return PDI_OK;
}

/// Convert into corresponding data type
PDI_status_t cast_data_int(Data_ref ref, int32_t plugin_data) {
	PDI_status_t status = PDI_OK;
	const PDI_datatype_t& type = ref.type();
	if ( type.kind == PDI_K_SCALAR ) {
		switch ( type.c.scalar ) {
			case PDI_T_INT32:
				*((int32_t*)ref.get()) = plugin_data;
				break;
			case PDI_T_INT8:
				*((int8_t*)ref.get()) = plugin_data;
				break;
			case PDI_T_INT16: 
				*((int16_t*)ref.get()) = plugin_data;
				break;
			case PDI_T_INT64: 
				*((int64_t*)ref.get()) = plugin_data;
				break;
			case PDI_T_FLOAT: 
				*((float*)ref.get()) = plugin_data;
				break;
			case PDI_T_DOUBLE:
				*((double*)ref.get()) = plugin_data;
				break;
			case PDI_T_LONG_DOUBLE:
				*((long double*)ref.get()) = plugin_data;
				break;
			default: status = PDI_ERR_VALUE;
		}
	} else {
		status = PDI_ERR_VALUE;
	}
	return status;
}



PDI_status_t PDI_utilities_data( const std::string& name, PDI::Data_ref cref )
{
	PDI_status_t status = PDI_OK;
	
	if ( Data_w_ref ref = cref ) {
		status = PDI_UNAVAILABLE;
		// for each utils_task look if the output is the same as the PDI_data_t
		for ( auto&& one_task: tasks ) {
			switch(one_task.action){ // check datatype compatibiliy
			case EVENT2DATA:
			case FILE_EXISTS:
			case DIR_EXISTS: {
				string str_out = one_task.out; // output string
				if ( status && name == str_out ) { // output and data name matches
					status = cast_data_int(ref, one_task.result);
					goto task_w_found;
				}
			} break;
			default: // do nothing
				break;
			}
		}
	}
task_w_found:
	if ( Data_r_ref ref = cref ) {
		for ( auto&& one_task: tasks ) {
			if ( one_task.action == EXTRACT_SUBARRAY) {
				string str_in = one_task.in; // input string
				if ( name == str_in ) { // input and data name match
					string str_out = one_task.out; // output string
					if( str_in == str_out ){
						fprintf(stderr,"[PDI/Utilities] Cannot extract subarray. Array %s and subarray %s identify the same data\n", str_in.c_str(), str_out.c_str());
						return PDI_ERR_CONFIG;
					}
					Data_descriptor& outdesc = PDI_state.desc(str_out);
					size_t oldsize; PDI_datatype_buffersize(&ref.type(), &oldsize);
					size_t subsize; PDI_datatype_datasize(&outdesc.get_type(), &subsize);
					void *subdata = malloc(subsize);
					PDI_buffer_copy(subdata, &outdesc.get_type(), ref.get(), &ref.type());
					PDI_expose(str_out.c_str(), subdata, PDI_OUT);
					free(subdata);
				}
			}
		}
	}
	return status;
}

PDI_PLUGIN(utilities)

