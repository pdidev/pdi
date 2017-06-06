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


#include "config.h"

#ifdef STRDUP_WORKS 	// used while reading configuration (strdup )
	#define _POSIX_C_SOURCE 200809L
	#include <string.h>
#endif

#include <mpi.h> 	// MPI library

#include <pdi.h> 	// PDI library 
#include <pdi/plugin.h>
#include <pdi/state.h>

#include <dlfcn.h> 	// dynamic loading of function 

/// if strdup is not provided
#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = malloc(strlen(s) + 1);
	if (p) strcpy(p, s);
	return p;
}
#endif
#define PRINTF_BUFFER_SIZE 256

char *vmsprintf(const char *fmt, va_list ap)
{
	int index_size = PRINTF_BUFFER_SIZE;
	char *index = malloc(index_size);
	while ( vsnprintf(index, index_size, fmt, ap) > index_size ) {
		index_size *= 2;
		index = realloc(index, index_size);
	}
	return index;
}

char *msprintf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char *res = vmsprintf(fmt, ap);
	va_end(ap);
	return res;
}

// ================  CONSTANT AND MACRO =====
#define UC_stderr stderr

/// Verbose level : 0 Error, 1 Warning, 2 Debug
#define UC_verbose 2

#if UC_verbose > 1
#define UC_dbg(...) {fprintf(UC_stderr, "[PDI/user_code] Debug: ");\
		fprintf(UC_stderr, __VA_ARGS__);\
		fflush(UC_stderr);}
#else  // Does nothing
#define UC_dbg(...) if(0) printf( __VA_ARGS__);
#endif

#if UC_verbose > 0
#define UC_warn(...) {fprintf(UC_stderr, "[PDI/user_code] Warning: ");\
		fprintf(UC_stderr, __VA_ARGS__);\
		fflush(UC_stderr);}
#else  // Does nothing
#define UC_warn(...) if(0) printf( __VA_ARGS__);
#endif

#define UC_err(...) {fprintf(UC_stderr, "[PDI/user_code] Error: " );\
		fprintf(UC_stderr, __VA_ARGS__);\
		fflush(UC_stderr);}



typedef void (*ptr_fct_t)(void);

/// Structure to define a function
typedef struct func_s {
	ptr_fct_t call; //< body of the function
	char *name;   //< function name
} func_t;

/// Structure to organize the interaction between functions and PDI
typedef struct UC_s {
	func_t fct; 		//< function to call on event

	char **events; 		//< event names that trigger the function call
	int nb_events; 		//< nb of events

	char **inputs; 		//< inputs for the function
	int nb_inputs; 		//< nb of inputs

	char **outputs; 	//< ouputs of the functions
	int nb_outputs; 	//< nb of outputs
} UC_t ;



UC_t  **all_uc; // array of user code (functions...)
int nb_uc;

int myrank;       // MPI rank 
MPI_Comm myworld; // MPI comm world 
PC_tree_t myconf;

// ================  STRING and NODES  =====


char *str2nodename(const char *s)
{
	char *p = malloc(strlen(s) + 1 + 1);
	if (p) {
		p[0] = '.';
		strcpy(&p[1], s);
	}
	return p;
}


PDI_status_t set_str_from_node(PC_tree_t cur_conf, const char *node_name, char ***list_str, int *nb_str)
{
	PDI_status_t status = PDI_UNAVAILABLE;
	if (!node_name) return status;

	*nb_str = 1;
	char **list_tmp = malloc(sizeof(char *));

	if (!PC_string(PC_get(cur_conf, node_name), list_tmp)) { //< if one node
		status = PDI_OK;
	} else if (!PC_len(PC_get(cur_conf, node_name), nb_str)) { //< if multiple nodes
		list_tmp = realloc(list_tmp, (*nb_str) * sizeof(char *));

		char *all_node_name = malloc(strlen(node_name) + 4 + 1); //< len(node_name)+len([%d])+'\0'
		strcpy(all_node_name, node_name);
		strcat(&all_node_name[strlen(node_name)], "[%d]"); // going through the list

		for (int ii = 0; ii < (*nb_str); ++ii) {
			PC_string(PC_get(cur_conf, all_node_name, ii), &list_tmp[ii]);
		}
		free(all_node_name);
		status = PDI_OK;
	} else {
		*nb_str = 0;
		free(list_tmp);
		list_tmp = NULL;
	}

	*list_str = list_tmp;
	return status;
}


// ============ Loading Functions Dynamicaly =====
PDI_status_t find_fct(char *fct_name, char *libname, ptr_fct_t  *fct)
{
	PDI_status_t status = PDI_OK;

	char *fct_symbol = msprintf("%s", fct_name);

	dlerror(); /// Empty dlerror in case of previous failure

	void *fct_uncast = dlsym(NULL, fct_symbol);

	// case where the library was not prelinked
	if (!fct_uncast) {
		void *lib_handle = dlopen(libname, RTLD_NOW);
		if (!lib_handle) {
			UC_err("Unable to load lib %s: %s\n", libname, dlerror())
				status = PDI_ERR_CONFIG;
		}
		fct_uncast = dlsym(lib_handle, fct_symbol);
		if (!fct_uncast) {
			UC_err("Unable to load fct `%s' from lib %s: %s\n", fct_name, libname, dlerror());
			status = PDI_ERR_CONFIG;
		}

		if(status){
			free(fct_symbol);
			return status;
		}
	}

	// ugly data to function ptr cast to be standard compatible (though undefined behavior)
	*fct = *((ptr_fct_t *)&fct_uncast);
	
	free(fct_symbol);
	return status;
}




/* Read next node
 * User_Code:
 * 	fct_1: // default = function name
 * 		name :    // override default
 * 		events:   // name that trigger function
 *
 * // Assume one inputs after another
 * 		inputs:   // inputs required for the function
 * 		outputs:  // outputs required for the function
 */


PDI_status_t read_one_elemnt(UC_t *this, PC_tree_t conf, char *name)
{
	char *node = str2nodename(name);
	PC_tree_t tmptree = PC_get(conf, node);
	free(node);

	char *str = name;
	if (PC_string(PC_get(tmptree, ".function"), &str)) {
		UC_warn("'function' node not found. Default value '%s' \n", str);
	}
	this->fct.name = str;

	node = ".events";
	if (set_str_from_node(tmptree, node, &this->events, &this->nb_events)) {
		UC_err("node '%s' not found\n", node);
		return PDI_ERR_CONFIG;
	}

	node = ".inputs";
	if (set_str_from_node(tmptree, node, &this->inputs, &this->nb_inputs))
		UC_warn("node '%s' not found\n", node);

	node = ".outputs";
	if (set_str_from_node(tmptree, node, &this->outputs, &this->nb_outputs))
		UC_warn("node '%s' not found\n", node);

	return PDI_OK;
}


// ================  INIT AND FINALIZE  =====
PDI_status_t new_UC(UC_t **new_uc)
{
	UC_t *this = malloc(sizeof(UC_t));
	if (!this) {
		UC_err("Malloc failed during init...\n");
		return PDI_ERR_PLUGIN;
	}
	// init function
	this->fct.call = NULL;
	this->fct.name = NULL;

	this->events = NULL;
	this->inputs = NULL;
	this->outputs = NULL;

	*new_uc = this;

	return PDI_OK;
}


PDI_status_t PDI_user_code_init(PC_tree_t conf, MPI_Comm *world)
{
	PDI_status_t status = PDI_OK;

	// Copy world and save rank at initialization
	MPI_Comm_rank(*world, &myrank);
	myworld = *world;

	// Copy Yaml configuration
	myconf = conf;

	if (PC_status(conf)) {
		UC_err("Invalid configuration, MPI_rank %d\n", myrank);
		return PDI_ERR_CONFIG;
	}

	// Loading configuration for events
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);

	int nb_node = 0;
	PC_len(conf, &nb_node);
	if (nb_node <= 0) {
		UC_warn("Plugin 'User Code' is loaded but configuration is empty (or invalid)\n");
	} else {
		all_uc = NULL;
		nb_uc = 0;
		for (int map_id = 0; map_id < nb_node; map_id++) {
			char *name;
			UC_t *next;
			/// Get next node
			if (PC_string(PC_get(myconf, "{%d}", map_id), &name)) continue;

			/// Allocate and initialize
			if (new_UC(&next)){
				UC_dbg("Error when creating next UserCode %s\n", name);
				continue;
			}

			/// Fill the element
			if (read_one_elemnt(next, conf, name)) {
				free(next);
				UC_warn("Error when reading element %s\n", name);
				continue;
			}

			/// Find the corresponding function
			if (find_fct(name, NULL, &(next->fct.call) )) {
				free(next);
				UC_warn("Error when reading element %s\n", name);
				continue;
			}

			/// Append to list
			all_uc = realloc(all_uc, sizeof(UC_t **)*nb_uc);
			all_uc[nb_uc] = next;
			free(name);
			nb_uc++;

			UC_dbg("Number of function loaded %d\n", nb_uc);
		}
	}

	return status;
}

PDI_status_t PDI_user_code_finalize()
{
	return PDI_OK;
}
PDI_status_t PDI_user_code_event(const char *event)
{
	for ( int ii=0; ii<nb_uc ; ++ii ) {
		for ( int n=0; n<all_uc[ii]->nb_events ; ++n ) {
			if ( !strcmp(event, all_uc[ii]->events[n]) ) {
				(*all_uc[ii]->fct.call)();
			}
		}
	}

	return PDI_OK;
}
PDI_status_t PDI_user_code_data_start(PDI_data_t *data)
{
	return PDI_OK;
}
PDI_status_t PDI_user_code_data_end(PDI_data_t *data)
{
	return PDI_OK;
}


PDI_PLUGIN(user_code)
