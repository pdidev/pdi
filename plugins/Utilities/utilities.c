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

#include <inttypes.h>
#include <assert.h>
#include <mpi.h>
#include <sys/stat.h> // to check file existence 

#include "pdi.h"
#include <pdi/plugin.h>
#include <pdi/state.h>

#ifdef STRDUP_WORKS
#define _POSIX_C_SOURCE 200809L
#endif
#include <string.h>



/// Supported actions 
typedef enum { EVENT2DATA=0,
	FILE_EXISTS,
	DIR_EXISTS,
	EXTRACT_SUBARRAY}
	utils_action_t;

typedef struct utils_task_s
{
	utils_action_t action; ///<  The action to perform on event

	int nb_events; ///<  Number of events

	char **events; ///<  Events that trigger the previous action

	PDI_value_t in; ///<  An expression: could be a file name, variables, or expressions

	PDI_value_t out; ///<  An expression 

	PDI_value_t select; ///<  select when to perform action (default is always)

	void *data; ///<  data to store the result   

	size_t size; ///<  data size

} utils_task_t;


/// Number of tasks performed by the plug-in
int nb_tasks = 0;
/// Tasks 
utils_task_t *tasks = NULL;

/// Configuration file obtain from PDI
PC_tree_t my_conf;


#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = malloc(strlen(s)+1);
	if ( p ) strcpy(p, s);
	return p;
}
#endif


/// Initialization: read configuration file and fill data structure
PDI_status_t PDI_utilities_init(PC_tree_t conf, MPI_Comm *world)
{
	world = world; // prevent unused param warning
	my_conf = conf; 
	
	if ( PC_status(my_conf) ) {
		return PDI_ERR_PLUGIN;
	}
	// Get the number of actions 
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

	// fill the utils_task_t data structure with corresponding values from conf file
	char *str, *one="1";
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
		str =NULL; 
		if( !PC_string(PC_get(treetmp, ".in"), &str) ){
			PDI_value_parse(str, &tasks[ii].in); 
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'in'.\n");
			return PDI_ERR_CONFIG;
		}
		// data 'out'
		str=NULL;
		if( !PC_string(PC_get(treetmp, ".out"), &str) ){
			PDI_value_parse(str, &tasks[ii].out); 
		} else {
			fprintf(stderr, "[PDI/Utilities] Error: no value or invalid value for 'out'.\n");
			return PDI_ERR_CONFIG;
		}

		// optional 'select'
		PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
		str=one;
		PC_string(PC_get(treetmp, ".select"), &str);
		// else apply default value of 1 (always true)
		PDI_value_parse(str, &tasks[ii].select); 
		PC_errhandler(errh);

		// Select an existing task 
		str="[empty]";
		PC_string(PC_get(my_conf, "{%d}", ii) , &str); 
		if ( !strcmp(str, "event2data") ){
			tasks[ii].action = EVENT2DATA;
			tasks[ii].size = sizeof(int32_t);
			tasks[ii].data = malloc(tasks[ii].size);
			memcpy(tasks[ii].data,(const void *)(&zero), tasks[ii].size);

		} else if ( !strcmp(str, "file_exists") ){
			tasks[ii].action = FILE_EXISTS;
			tasks[ii].size = sizeof(int32_t);
			tasks[ii].data = malloc(tasks[ii].size);
			memcpy(tasks[ii].data,(const void *)(&zero), tasks[ii].size);

		} else if ( !strcmp(str, "dir_exists") ){
			tasks[ii].action = DIR_EXISTS;
			tasks[ii].size = sizeof(int32_t);
			tasks[ii].data = malloc(tasks[ii].size);
			memcpy(tasks[ii].data,(const void *)(&zero), tasks[ii].size);

		} else if ( !strcmp(str, "extract_subarray") ){
			tasks[ii].action = EXTRACT_SUBARRAY;
			tasks[ii].data = NULL; 
			tasks[ii].size = 0;

		} else {
			tasks[ii].data = NULL; 
			tasks[ii].size = 0;
			fprintf(stderr, "[PDI/Utilities] Error: Invalid node name, '%s'.\n",str);
			return PDI_ERR_CONFIG;
		}
		
	}
	
	return PDI_OK;
}


/// Finalize and free allocated data
PDI_status_t PDI_utilities_finalize()
{
	for ( int ii=0; ii<nb_tasks; ++ii ) {
		free(tasks[ii].events);
		if(tasks[ii].size) free(tasks[ii].data);
		tasks[ii].size = 0;
	}
	free(tasks);
	return PDI_OK;
}


/// Traps and converts events into tasks
PDI_status_t PDI_utilities_event(const char *event_name)
{
	char *str=NULL;
	int32_t tmp;
	long  ltmp;
	struct stat sb;
	for(int ii=0; ii<nb_tasks; ++ii ){
		for(int nn=0; nn<tasks[ii].nb_events; ++nn){
			PDI_value_int(&tasks[ii].select,&ltmp);
			if(ltmp){
				if(tasks[ii].action == EVENT2DATA){// TODO[CR]: error check
					// Should evaluate expression define in 'in' and set value in 'out'
					if (!strcmp(tasks[ii].events[nn],event_name)){
						PDI_value_int(&tasks[ii].in,&ltmp);
						tmp=ltmp; // Nasty workaround to remove warning

					} else {
						tmp=0;
					}
					// Save value 
					memcpy(tasks[ii].data, &tmp, tasks[ii].size);

					// expose value in current out (can be received by other plug-ins).
					PDI_value_str(&tasks[ii].out,&str);
					PDI_expose(str, &tmp);
				}
				else if(!strcmp(tasks[ii].events[nn],event_name)){
					switch(tasks[ii].action){
							
						case EXTRACT_SUBARRAY:
							break;

						case FILE_EXISTS:
							// get filename from input into str
							PDI_value_str(&tasks[ii].in,&str); 
							// check for file
							if (stat(str, &sb) == 0 && S_ISREG(sb.st_mode)){
								tmp = 1;
							} else {
								tmp = 0;
							} 
							free(str); 

							memcpy(tasks[ii].data, &tmp, tasks[ii].size);
							// get name to expose the return value
							PDI_value_str(&tasks[ii].out,&str);
							PDI_expose(str, &tmp);

							free(str);
							break;

						case DIR_EXISTS:
							// get filename from input into str
							PDI_value_str(&tasks[ii].in,&str); 
							// check for file
							if (stat(str, &sb) == 0 && S_ISDIR(sb.st_mode)){
								tmp = 1;
							} else {
								tmp = 0;
							} 
							free(str); 

							memcpy(tasks[ii].data, &tmp, tasks[ii].size);
							// get name to expose the return value
							PDI_value_str(&tasks[ii].out,&str);
							PDI_expose(str, &tmp);

							free(str);
							break;

						default:
							return PDI_ERR_CONFIG;
					}
				}
			}
		}
	}
	return PDI_OK;
}

/// Convert into corresponding data type
int cast_data_int(PDI_data_t *data, int32_t plugin_data) {
	int status = PDI_OK; 
	if (data->type.kind == PDI_K_SCALAR){
		switch(data->type.c.scalar){
			case PDI_T_INT32:  
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(int32_t));
				break;
			case PDI_T_INT8: 
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(int8_t));
				break; 
			case PDI_T_INT16:  
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(int16_t));
				break; 
			case PDI_T_INT64:  
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(int64_t));
				break; 
			case PDI_T_FLOAT:  
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(float));
				break; 
			case PDI_T_DOUBLE: 
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(double));
				break; 
			case PDI_T_LONG_DOUBLE:
				memcpy(data->content[data->nb_content-1].data, &plugin_data, sizeof(long double));
				break; 
			case PDI_T_UNDEF: status = PDI_ERR_VALUE; 
		}
	} else {
		status = PDI_ERR_VALUE;
	}
	return status;
}


// Arrays should have the same rank
PDI_status_t extract_subarray( PDI_data_t *data, PDI_data_t *sub, void **buffer, size_t *size ){
	void *p1, *p2;
	int order = PDI_ORDER_C;
	int sub_order = PDI_ORDER_C;
	// Array sizes and offset
	int sizes   [7]={0,0,0,0,0,0,0};
	int subsizes[7]={0,0,0,0,0,0,0};
	int offsets [7]={0,0,0,0,0,0,0};
	// Sizes of buffer == subsizes
	int buff_sizes[7]={0,0,0,0,0,0,0};

	// Allocate and Initialialize
	order = data->type.c.array->order;
	int rank = data->type.c.array->ndims;
	// buffer 
	int rank_sub = sub->type.c.array->ndims;
	if (rank != rank_sub){
		fprintf(stderr, "[PDI/Utilities] While extracting subarray, rank mismatched.");
		return PDI_ERR_VALUE;
	}

	// Parse size
	int ii,si;
	long intdim = 0;
	for ( int i=0; i<rank; ++i ) {
		// dimensions arrays
		switch (order){
		case PDI_ORDER_C:
			ii = i; break; // ORDER_C
		case PDI_ORDER_FORTRAN:
			ii = rank-i-1; break; // ORDER_FORTRAN
		}
		
		PDI_value_int(&data->type.c.array->sizes[ii], &intdim);
		sizes[ii] = intdim;
		
		PDI_value_int(&data->type.c.array->subsizes[ii], &intdim);
		subsizes[ii] = intdim;
		
		PDI_value_int(&data->type.c.array->starts[ii], &intdim);
		offsets[ii] = intdim;
		
		// dimensions sub-arrays
		switch (sub_order){
		case PDI_ORDER_C:
			si = i; break; 
		case PDI_ORDER_FORTRAN:
			si = rank-i-1; break;
		}
		PDI_value_int(&sub->type.c.array->sizes[si], &intdim);
		buff_sizes[si] = intdim;
	}
	
	// allocate the buffer
	size_t nbytes=0;
	if ( PDI_data_size( &sub->type.c.array->type, &nbytes)) return PDI_ERR_VALUE;
	
	size_t positive_nb;
	*size = nbytes;
	for(int i=0; i<rank; ++i){
		positive_nb = buff_sizes[i];
		if( buff_sizes[i] < 1) positive_nb=1;
		*size *= positive_nb;
	}
	*buffer=malloc(*size);

	// Copy one into the other
	// max rank=7
	for ( int n6=offsets[6], i6=0; n6<=subsizes[6]; ++n6 ) {
	  for ( int n5=offsets[5], i5=0; n5<=subsizes[5]; ++n5 ) {
	    for ( int n4=offsets[4], i4=0; n4<=subsizes[4]; ++n4 ) {
	      for ( int n3=offsets[3], i3=0; n3<=subsizes[3]; ++n3 ) {
	        for ( int n2=offsets[2], i2=0; n2<=subsizes[2]; ++n2 ) {
	          for ( int n1=offsets[1], i1=0; n1<=subsizes[1]; ++n1 ) {
	            p1=data->content[data->nb_content-1].data + nbytes*( offsets[0]
	              + n1*sizes[0] 
	              + n2*sizes[0]*sizes[1] 
	              + n3*sizes[0]*sizes[1]*sizes[2]
	              + n4*sizes[0]*sizes[1]*sizes[2]*sizes[3]
	              + n5*sizes[0]*sizes[1]*sizes[2]*sizes[3]*sizes[4]
	              + n6*sizes[0]*sizes[1]*sizes[2]*sizes[3]*sizes[4]*sizes[5]
	              );
	
	            p2=*buffer + nbytes*(
	              + i1*buff_sizes[0] 
	              + i2*buff_sizes[0]*buff_sizes[1] 
	              + i3*buff_sizes[0]*buff_sizes[1]*buff_sizes[2]
	              + i4*buff_sizes[0]*buff_sizes[1]*buff_sizes[2]*buff_sizes[3]
	              + i5*buff_sizes[0]*buff_sizes[1]*buff_sizes[2]*buff_sizes[3]*buff_sizes[4]
	              + i6*buff_sizes[0]*buff_sizes[1]*buff_sizes[2]*buff_sizes[3]*buff_sizes[4]*buff_sizes[5]
	              );
	
	            memcpy(p2, p1, nbytes*subsizes[0]);
	            ++i1;
	          }
	          ++i2;
	        }
	        ++i3; 
	      }
	      ++i4;
	    }
	    ++i5;
	  }
	  ++i6;
	}
return PDI_OK;
}



PDI_status_t PDI_utilities_data_start( PDI_data_t *data )
{
	char *str_out=NULL,*str_in=NULL;
	int status=PDI_OK,tmp_status=PDI_OK;
	// Only import is possible
	int32_t copy;
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		// for each utils_task look if the output is the same than the PDI_data_t
		for ( int ii=0; ii<nb_tasks; ++ii ) {
			if(tmp_status!= PDI_OK) status=tmp_status;

			PDI_value_str(&tasks[ii].out, &str_out); // output string
			if ( !strcmp(str_out, data->name) ){   // output and data name matches
				switch(tasks[ii].action){  // check datatype compatibiliy
					case EVENT2DATA:
						copy = *(int32_t *)tasks[ii].data;
						tmp_status = cast_data_int(data, copy);
						break; 
					case FILE_EXISTS:
						copy = *(int32_t *)tasks[ii].data;
						tmp_status = cast_data_int(data, copy);
						break; 
					case DIR_EXISTS:
						copy = *(int32_t *)tasks[ii].data;
						tmp_status = cast_data_int(data, copy);
						break; 
					default:
						break;
				}
				
			}
		}
	} 
	if (data->content[data->nb_content-1].access & PDI_OUT){
		for ( int ii=0; ii<nb_tasks; ++ii ) {
			if(tmp_status!= PDI_OK) status=tmp_status;
				if( tasks[ii].action == EXTRACT_SUBARRAY ){
				// check select
				PDI_value_str(&tasks[ii].in, &str_in); // input string
				if( (!strcmp(str_in, data->name)) ){ // && ( data->type.kind == PDI_K_ARRAY) ){
					PDI_value_str(&tasks[ii].out, &str_out); // output string
					if( !strcmp(str_in,str_out) ){
						fprintf(stderr,"[PDI/Utilities] Cannot extract subarray. Array %s and subarray %s identified the same data\n", str_in, str_out);
						tmp_status = PDI_ERR_CONFIG;
						continue;
					}

					PDI_data_t *data_out=PDI_find_data(str_out);
					if( data_out ){ // checking that output data exists
						//TODO[CR] Check that both datasets have the correct type
						//  if( data->type.c.array->type.kind != PDI_K_SCALAR ){
						//  	fprintf(stderr," Array : %d %d %d\n", PDI_K_SCALAR, PDI_K_ARRAY, data->type.c.array->type.kind);
						//  	fprintf(stderr,"[PDI/Utilities] Cannot extract subarray (from in). Array %s is not a N-dim array\n", str_in);
						//  	tmp_status = PDI_ERR_CONFIG;
						//  	continue;
						//  }
						//  if( data_out->type.c.array->type.kind != PDI_K_SCALAR ){
						//  	fprintf(stderr,"[PDI/Utilities] Cannot extract subarray (out). Array %s is not a N-dim array\n", str_out);
						//  	tmp_status = PDI_ERR_CONFIG;
						//  	continue;
						//  }

						fprintf(stderr, "Matching, in: %s   ||  var: %s\n", str_in, data->name);
						if( tasks[ii].size != 0) free(tasks[ii].data);
						tmp_status=extract_subarray(data, data_out, &tasks[ii].data, &tasks[ii].size);
						PDI_expose(str_out, tasks[ii].data);
					}
				} else {
					fprintf(stderr, "Not matching, in: %s   ||  var: %s\n", str_in, data->name);
				}
			}
		}
	}
	return status;
}

PDI_status_t PDI_utilities_data_end(PDI_data_t *data)
{
	data=data; // remove warning "unused var..."
	return PDI_OK;
}

PDI_PLUGIN(utilities)
