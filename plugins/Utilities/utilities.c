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

#include "config.h"


#ifdef STRDUP_WORKS
#define _POSIX_C_SOURCE 200809L
#include <string.h>
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

	PDI_value_t *in; 

	PDI_value_t *out; 

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


/** Reads the plugin config for all tasks  
 * \param conf the configuration node to read
 * \param hdf5data the array where to store the resulting variables
 * \param nb_hdf5data somewhere where to store the number of variables found
 * \param def_file the default file for HDF5 inputs/outputs
 * \param def_select the default select for HDF5 inputs/outputs
 */


PDI_status_t PDI_utilities_init(PC_tree_t conf, MPI_Comm *world)
{
	world = world; // prevent unused param warning
	my_conf = conf;
	
	if ( PC_status(my_conf) ) {
		fprintf(stderr, "[PDI/Utilities] Invalid configuration.");
		return PDI_ERR_PLUGIN;
	}
	
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

	char *tmp_str;
	for ( int ii=0; ii<=nb_tasks-1; ++ii){

		//------ Mandatory nodes and informations
		
		// Select task 
		tmp_str=NULL;
		PC_string(PC_get(my_conf, "{%d}", ii) , &tmp_str); 
		if ( !strcmp(tmp_str, "event2data") ){
			tasks[ii].action = EVENT2DATA;
		} else if ( !strcmp(tmp_str, "file_exists") ){
			tasks[ii].action = FILE_EXISTS;
		} else if ( !strcmp(tmp_str, "extract_subarray") ){
			tasks[ii].action = EXTRACT_SUBARRAY;
		} else {
			fprintf(stderr, "[PDI/Utilities] Unknown action: %s.\nInvalid configuration.",tmp_str);
			return PDI_ERR_CONFIG;
		}
		
		// Define parameters to perform the task 
		PC_tree_t treetmp = PC_get(my_conf, "<%d>", ii); // get the node
		
		// List of events (or single event)
		tmp_str=NULL;
		PC_tree_t all_events = PC_get(treetmp,".events");
		PC_len(all_events, &tasks[ii].nb_events);
		tasks[ii].events=malloc(tasks[ii].nb_events*sizeof(char*));
		for( int nn=0; nn < tasks[ii].nb_events; ++nn ){
			PC_string(PC_get(all_events, "{%d}", ii) , &tasks[ii].events[nn]); 
		}


		//------ input and ouput
		// data 'in'
		PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
		tmp_str=NULL;
		tasks[ii].in=NULL;
		if( !PC_string(PC_get(treetmp, ".in"), &tmp_str)){
			PDI_value_parse(tmp_str,tasks[ii].in);
		} 
		
		// data 'out'
		tmp_str=NULL;
		tasks[ii].out=NULL;
		if( !PC_string(PC_get(treetmp, ".out"), &tmp_str)){
			PDI_value_parse(tmp_str,tasks[ii].out);
		} 
		PC_errhandler(errh);
		
	}
	
	return PDI_OK;
}


PDI_status_t PDI_utilities_finalize()
{
	for ( int ii=0; ii<nb_tasks; ++ii ) {
		free(tasks[ii].events);
	}
	free(tasks);
	return PDI_OK;
}


PDI_status_t PDI_utilities_event(const char *event_name)
{
	char *str=NULL;
	int same_event = 0;
	long tmp_value = 0;
	struct stat sb;
	for(int ii=0; ii<nb_tasks; ++ii ){
		for(int nn=0; nn<tasks[ii].nb_events; ++nn){
			same_event = !strcmp(tasks[ii].events[nn],event_name);
			if( same_event || tasks[ii].action == EVENT2DATA ){
				switch(tasks[ii].action){
					case EVENT2DATA:
						tmp_value=0;
						PDI_value_str(tasks[ii].out,&str); // TODO[CR]: error check
						// Evaluate expression define in 'in' and set value in 'out'
						if (same_event){
							PDI_value_int(tasks[ii].in,&tmp_value); // TODO[CR]: error check
							PDI_expose(str,&tmp_value);
						}else{ // set 0 in tmp_value
							PDI_expose(str,&tmp_value);
						}
						free(str); // check  

						break;
					case EXTRACT_SUBARRAY:
						// Find dimensions of input array
						
						// Find dimensions of output array
						
						// Copy one into the other
						
						// PDI_expose
						fprintf(stderr, "[PDI/Utilities] Extracting subarray not implemented yet.");
						break;

					case FILE_EXISTS:
						// get filename into str
						PDI_value_str(tasks[ii].in,&str); // TODO[CR]: error check
						// if (stat(pathname, &sb) == 0 && S_ISDIR(sb.st_mode))
						// check for file
						if (stat(str, &sb) == 0 && S_ISREG(sb.st_mode) ){
							tmp_value=1;
						} else {
							tmp_value=0;
						}

						// expose in ouput variable
						PDI_value_str(tasks[ii].out,&str); // TODO[CR]: error check
						PDI_expose(str,&tmp_value);
						free(str);
						break;

					default:
						return PDI_ERR_CONFIG;
				}
			}
		}
	}
	return PDI_OK;
}


PDI_status_t PDI_utilities_data_start( PDI_data_t *data )
{
	data = data; // prevent unused warning
	return PDI_OK;
}

PDI_status_t PDI_utilities_data_end(PDI_data_t *data)
{
	data = data; // prevent unused warning
	return PDI_OK;
}

PDI_PLUGIN(utilities)
