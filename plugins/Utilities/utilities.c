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
 * - extracting subarrays for a N dimensionnal array (N<=7)
 * - transforming event into booleen variables
 * - giving the last event call
 *  ... etc.
 ***/

#define _DBG_MACRO_ fprintf(stderr,"line %6d ||  Utilities\n", __LINE__); fflush(stderr);

#include "config.h"

#include <inttypes.h>
#include <assert.h>
#include <string.h>
#ifdef STRDUP_WORKS
#define _POSIX_C_SOURCE 200809L
#endif

#include <mpi.h>

// Check file exist 
#include <sys/stat.h>

#include "pdi.h"
#include <pdi/plugin.h>
#include <pdi/state.h>




PC_tree_t my_conf;

/// Supported actions 
typedef enum { EVENT2DATA=0,
	FILE_EXISTS,
	EXTRACT_SUBARRAY}
	utils_action_t;

typedef struct utils_task_s
{
	utils_action_t action; // action to perform on event

	int nb_events;

	char **events; // events that trigger processing the task

	PDI_value_t in; // file name, variables, or expressions 

	PDI_value_t out; // currently, only a variable name

	void *data; // a compatible PDI data

	size_t size; // size of allocated data

} utils_task_t;


int nb_tasks = 0;

utils_task_t *tasks = NULL;


#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = malloc(strlen(s)+1);
	if ( p ) strcpy(p, s);
	return p;
}
#endif


PDI_status_t PDI_utilities_init(PC_tree_t conf, MPI_Comm *world)
{
	world = world; // prevent unused param warning
	my_conf = conf;
	
	if ( PC_status(my_conf) ) {
		return PDI_ERR_PLUGIN;
	}
	/// Get the number of actions 
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	if(PC_len(my_conf,&nb_tasks)){
		nb_tasks=0;
		tasks=NULL;
		PC_errhandler(errh);
		return PDI_OK;
	} else {
		tasks=malloc(nb_tasks*sizeof(utils_task_t));
	}
	PC_errhandler(errh);

	/// fill the utils_task_t data structure with corresponding values from conf file
	char *str_in;
	char *str_out;
	char *str_action;
	const long zero = 0;
	for ( int ii=0; ii<=nb_tasks-1; ++ii){ 
		// Get the next node
		PC_tree_t treetmp = PC_get(my_conf, "<%d>", ii);
		
		tasks[ii].nb_events = 0;
		// List of events (or single event)
		errh = PC_errhandler(PC_NULL_HANDLER);
		PC_tree_t all_events = PC_get(treetmp,".events");
		if( !PC_len(all_events, &tasks[ii].nb_events) ){
			tasks[ii].events=malloc(tasks[ii].nb_events*sizeof(char*));
			for( int nn=0; nn < tasks[ii].nb_events; ++nn ){
				PC_string(PC_get(all_events, "{%d}", ii) , &tasks[ii].events[nn]); 
			}
		} else { // testing with one event
			tasks[ii].nb_events=1;
			tasks[ii].events=malloc(tasks[ii].nb_events*sizeof(char *));
			PC_string(PC_get(treetmp,".event"), &tasks[ii].events[0]);
		}
		PC_errhandler(errh);
		assert(tasks[ii].nb_events > 0 && "[PDI/Utilities] Error: no value or invalid value for 'event' or 'events'.\n");

		// input and ouput
		// data 'in'
		str_in =NULL; 
		if( !PC_string(PC_get(treetmp, ".in"), &str_in) ){
			PDI_value_parse(str_in, &tasks[ii].in);
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'in'.\n");
			return PDI_ERR_CONFIG;
		}
		// data 'out'
		str_out=NULL;
		if( !PC_string(PC_get(treetmp, ".out"), &str_out) ){
			PDI_value_parse(str_out, &tasks[ii].out);
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'out'.\n");
			return PDI_ERR_CONFIG;
		}

		// Select an existing task 
		str_action=NULL;
		PC_string(PC_get(my_conf, "{%d}", ii) , &str_action); 
		if ( !strcmp(str_action, "event2data") ){
			tasks[ii].action = EVENT2DATA;
			tasks[ii].size = sizeof(int64_t);
			tasks[ii].data = malloc(tasks[ii].size);
			memcpy(tasks[ii].data,(const void *)(&zero), tasks[ii].size);
			if (tasks[ii].size) fprintf(stderr,"is allocated, %ld",*(int64_t *)tasks[ii].data);

		} else if ( !strcmp(str_action, "file_exists") ){
			tasks[ii].action = FILE_EXISTS;
			tasks[ii].size = sizeof(int8_t);
			tasks[ii].data = malloc(tasks[ii].size);
			memcpy(tasks[ii].data,(const void *)(&zero), tasks[ii].size);
			if (tasks[ii].size) fprintf(stderr,"is allocated, %ld",*(int8_t *)tasks[ii].data);

		} else if ( !strcmp(str_action, "extract_subarray") ){
			tasks[ii].action = EXTRACT_SUBARRAY;
			tasks[ii].data = NULL; 
			tasks[ii].size = 0;

		} else {
			tasks[ii].data = NULL; 
			tasks[ii].size = 0;
			fprintf(stderr, "[PDI/Utilities] Error: Invalid node name, '%s'.\n",str_action);
			return PDI_ERR_CONFIG;
		}
		
	}
	
	return PDI_OK;
}


PDI_status_t PDI_utilities_finalize()
{
	_DBG_MACRO_
	for ( int ii=0; ii<nb_tasks; ++ii ) {
		free(tasks[ii].events);
		if(tasks[ii].size) free(tasks[ii].data);
	}
	free(tasks);
	return PDI_OK;
}


PDI_status_t PDI_utilities_event(const char *event_name)
{
	char *str=NULL;
	char *str2=NULL;
	int same_event = 0;
	struct stat sb;
	for(int ii=0; ii<nb_tasks; ++ii ){
		for(int nn=0; nn<tasks[ii].nb_events; ++nn){
			same_event=0;
			if(!strcmp(tasks[ii].events[nn],event_name)) same_event=1;
			if( same_event || tasks[ii].action == EVENT2DATA ){
				switch(tasks[ii].action){
					case EVENT2DATA:{
						int64_t tmp=0;
						// Evaluate expression define in 'in' and set value in 'out'
						if (same_event){
							PDI_value_int(&tasks[ii].in,&tmp); // TODO[CR]: error check
						} else { 
							tasks[ii].data = 0;
						}
						fprintf(stderr,"event %s \n", tasks[ii].events[0]);
	//_DBG_MACRO_
						if (!tasks[ii].size) fprintf(stderr,"not allocated"); 
	//					fflush(stderr);
	//_DBG_MACRO_
						if (tasks[ii].size) fprintf(stderr,"is allocated:");fflush(stderr);
	//_DBG_MACRO_
	//					if (tasks[ii].size) fprintf(stderr,"is allocated, %ld",*(long *)tasks[ii].data);
	//					fflush(stderr);
	_DBG_MACRO_
						memcpy(tasks[ii].data, &tmp, tasks[ii].size);
	_DBG_MACRO_
						} break;
					case EXTRACT_SUBARRAY:
						// Find dimensions of input array
						
						// Find dimensions of output array
						
						// Copy one into the other
						
						break;

					case FILE_EXISTS:{
						int8_t tmp=0;
						// get filename into str
						PDI_value_str(&tasks[ii].in,&str); // TODO[CR]: error check
						// if (stat(pathname, &sb) == 0 && S_ISDIR(sb.st_mode))
						tmp = 0;
						// check for file
						if (stat(str, &sb) == 0 && S_ISREG(sb.st_mode)){
							tmp = 1;
						}  
						memcpy(tasks[ii].data, &tmp, tasks[ii].size);

						// expose in ouput variable
						// PDI_value_str(&tasks[ii].out,&str); // TODO[CR]: error check
						free(str);
						} break;

					default:
						return PDI_ERR_CONFIG;
				}
			}
		}
	}
	return PDI_OK;
}

int cast_data_int(PDI_data_t *data, int64_t plugin_data) {
	int status = PDI_OK; 
	_DBG_MACRO_
	if (data->type.kind == PDI_K_SCALAR){
		switch(data->type.c.scalar){
			// single cast 
			case PDI_T_INT32:  
				*(int32_t *) data->content[data->nb_content-1].data = (int32_t)plugin_data;
				break;
			// double cast 
			case PDI_T_INT8: 
				*(int8_t *)  data->content[data->nb_content-1].data = (int8_t)plugin_data;
				break; 
			case PDI_T_INT16:  
				*(int16_t*)  data->content[data->nb_content-1].data = (int16_t)plugin_data;
				break; 
			case PDI_T_INT64:  
				*(int64_t *) data->content[data->nb_content-1].data = (int64_t)plugin_data;
				break; 
			case PDI_T_FLOAT:  
				*(float *)   data->content[data->nb_content-1].data = (float)plugin_data;
				break; 
			case PDI_T_DOUBLE: 
				*(double *)  data->content[data->nb_content-1].data = (double)plugin_data;
				break; 
			case PDI_T_LONG_DOUBLE:
				*(long double *)data->content[data->nb_content-1].data = (long double)plugin_data;
				break; 
			case PDI_T_UNDEF: status = PDI_ERR_VALUE; 
		}
	} else {
		status = PDI_ERR_VALUE;
	}
	_DBG_MACRO_
	return status;
}


PDI_status_t PDI_utilities_data_start( PDI_data_t *data )
{
	char *str=NULL;
	int status=PDI_OK;
	// Only import is possible
	int64_t copy;
	_DBG_MACRO_
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		// for each utils_task look if the output is the same than the PDI_data_t
		for ( int ii=0; ii<nb_tasks; ++ii ) {  
			str=NULL;
			PDI_value_str(&tasks[ii].out,&str); // output string
			if ( !strcmp(str, data->name) ){   // output and data name matches
				switch(tasks[ii].action){  // check datatype compatibiliy
					case EVENT2DATA:
						copy = *(int64_t *)tasks[ii].data;
						status = cast_data_int(data, copy);
						break; 
				//	case EXTRACT_SUBARRAY:
				//		return PDI_UNAVAILABLE;
					case FILE_EXISTS:
						copy = (int64_t)(*(int8_t *)tasks[ii].data);
						status = cast_data_int(data, copy);
						break; 
					default:
						status = PDI_ERR_VALUE;
				}
				
			}
		}
	}
	_DBG_MACRO_
	return status;
}

PDI_status_t PDI_utilities_data_end(PDI_data_t *data)
{
	int status = PDI_OK;
	char *str=NULL;
	// Only import is possible
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		// for each utils_task look if the output is the same than the PDI_data_t
	_DBG_MACRO_
		for ( int ii=0; ii<nb_tasks; ++ii ) {  
			str=NULL;
			PDI_value_str(&tasks[ii].out,&str); // output string
			if ( tasks[ii].action == EXTRACT_SUBARRAY){
				if (!strcmp(str, data->name) ){   // output and data name matches
					if(tasks[ii].size){
	_DBG_MACRO_
						free(tasks[ii].data); 
						tasks[ii].size = 0;
					}
	_DBG_MACRO_
				}
			}
		}
	}
	return status;
}

PDI_PLUGIN(utilities)
