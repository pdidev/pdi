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

#ifdef STRDUP_WORKS
#define _POSIX_C_SOURCE 200809L
#include <string.h>
#endif
#include <mpi.h>
#include <hdf5.h>
#include <hdf5_hl.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>

PC_tree_t my_conf;

typedef struct hdf5pp_var_s
{
	char *name;
	
	PDI_value_t h5file;
	
	PDI_value_t h5var;
	
	PDI_value_t select;
	
} hdf5pp_var_t;

int nb_outputs = 0;

hdf5pp_var_t *outputs = NULL;

int nb_inputs = 0;

hdf5pp_var_t *inputs = NULL;


#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = malloc(strlen(s)+1);
	if ( p ) strcpy(p, s);
	return p;
}
#endif


/** Reads the plugin config for either inputs or outputs
 * \param conf the configuration node to read
 * \param hdf5data the array where to store the resulting variables
 * \param nb_hdf5data somewhere where to store the number of variables found
 * \param def_file the default file for HDF5 inputs/outputs
 * \param def_select the default select for HDF5 inputs/outputs
 */
PDI_status_t read_config_file( PC_tree_t conf, hdf5pp_var_t *hdf5data[],
		int *nb_hdf5data, char *def_file, char *def_select )
{
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	
	if ( PC_len(conf, nb_hdf5data) ) { // if no subtree found
		*nb_hdf5data = 0;
		*hdf5data = NULL;
		PC_errhandler(errh);
		return PDI_OK;
	}
	*hdf5data = malloc((*nb_hdf5data)*sizeof(hdf5pp_var_t));
	
	for ( int ii=0; ii<(*nb_hdf5data); ++ii ) {
		PC_string(PC_get(conf, "{%d}", ii) , &(*hdf5data)[ii].name);
		PC_tree_t treetmp = PC_get(conf, "<%d>", ii); // get the node

		// set the corresponding HDF5 variable name
		char *var_strv = NULL;
		if( PC_string(PC_get(treetmp, ".var"), &var_strv)){ // no variable name or not readable
			var_strv = strdup((*hdf5data)[ii].name); // the node label is used (as a default value)
		}
		PDI_value_parse(var_strv, &(*hdf5data)[ii].h5var);
		
		// set the HDF5 filename (i.e. where do we write the data)
		char *file_strv = NULL;
		if( PC_string(PC_get(treetmp, ".file"), &file_strv) ){ // No file name
			if ( def_file ) { // using default
				file_strv = strdup(def_file);
			} else {
				fprintf(stderr, "[PDI/HDF5] 'file' not found for variable %s\n", var_strv);
				free(var_strv);
				free(file_strv);
				PC_errhandler(errh);
				return PDI_ERR_CONFIG;
			}
		}
		PDI_value_parse(file_strv, &(*hdf5data)[ii].h5file);
		free(file_strv);
		
		// sampling or frequency (i.e. when do we expose)
		char *select_strv = NULL;
		if ( PC_string(PC_get(treetmp, ".select"), &select_strv) ) {
			if ( def_select ) { // using default
				select_strv = strdup(def_select);
			} else {
				fprintf(stderr, "[PDI/HDF5] 'select' not found for variable %s\n", var_strv);
				free(var_strv);
				free(select_strv);
				PC_errhandler(errh);
				return PDI_ERR_CONFIG;
			}
		}
		PDI_value_parse(select_strv, &(*hdf5data)[ii].select);
		free(select_strv);
		
		free(var_strv);
	}
	PC_errhandler(errh);

	return PDI_OK;
}


PDI_status_t PDI_hdf5_per_process_init(PC_tree_t conf, MPI_Comm *world)
{
	world = world; // prevent unused param warning
	my_conf = conf;
	
	if ( H5open() < 0 ) { // Failure initializing HDF5
		fprintf(stderr, "[PDI/HDF5] Cannot load HDF5 library");
		return PDI_ERR_PLUGIN;
	}
	
	if ( PC_status(my_conf) ) {
		fprintf(stderr, "[PDI/HDF5] Invalid configuration.");
		return PDI_ERR_PLUGIN;
	}
	
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	char *def_out_file = NULL; // default output file if none specified
	PC_string(PC_get(my_conf, ".defaults.outputs.file"), &def_out_file);
	char *def_out_select = NULL; // default output select if none specified
	PC_string(PC_get(my_conf, ".defaults.outputs.select"), &def_out_select);
	PC_tree_t outputs_cfg = PC_get(conf, ".outputs");
	PC_errhandler(errh);
	
	PDI_status_t status = read_config_file(outputs_cfg, &outputs, &nb_outputs,
			def_out_file, def_out_select);
	free(def_out_file);
	free(def_out_select);
	
	if (status) return status;
	
	errh = PC_errhandler(PC_NULL_HANDLER);
	char* def_in_file = NULL; // default input file if none specified
	PC_string(PC_get(my_conf, ".defaults.inputs.file"), &def_in_file);
	char* def_in_select = NULL; // default input select if none specified
	PC_string(PC_get(my_conf, ".defaults.inputs.select"), &def_in_select);
	PC_tree_t inputs_cfg = PC_get(conf, ".inputs");
	PC_errhandler(errh);
	
	status = read_config_file(inputs_cfg, &inputs, &nb_inputs, def_in_file,
			def_in_select);
	free(def_in_file);
	free(def_in_select);
	
	return status;
}

PDI_status_t PDI_hdf5_per_process_finalize()
{
	for ( int ii=0; ii<nb_outputs; ++ii ) {
		PDI_value_destroy(&outputs[ii].h5file);
		PDI_value_destroy(&outputs[ii].h5var);
		PDI_value_destroy(&outputs[ii].select);
		free(outputs[ii].name);
	}
	free(outputs);
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_event(const char *event)
{
	event = event; // prevent unused warning
	return PDI_OK;
}

hid_t h5type(PDI_scalar_type_t ptype) {
	switch (ptype) {
	case PDI_T_INT8: return  H5T_NATIVE_INT8;
	case PDI_T_INT16: return  H5T_NATIVE_INT16;
	case PDI_T_INT32: return  H5T_NATIVE_INT32;
	case PDI_T_INT64: return  H5T_NATIVE_INT64;
	case PDI_T_FLOAT: return  H5T_NATIVE_FLOAT;
	case PDI_T_DOUBLE: return  H5T_NATIVE_DOUBLE;
	case PDI_T_LONG_DOUBLE: return  H5T_NATIVE_LDOUBLE;
	case PDI_T_UNDEF: return H5T_NO_CLASS;
	}
	return H5T_NO_CLASS; //TODO: better handle the error
}

int is_h5_file(char *filename)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	int fexists = (H5Fis_hdf5(filename)>0);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
	return fexists;
}

void rm_if_exist(hid_t h5file, char *dset_name)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	H5Ldelete(h5file, dset_name, H5P_DEFAULT);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
}

void write_to_file(PDI_data_t *data, char *filename, char *pathname)
{
	int rank = 0;
	int order = PDI_ORDER_C;
	hsize_t *h5sizes = NULL;
	hsize_t *h5subsizes = NULL;
	hsize_t *h5starts = NULL;
	PDI_type_t *scalart = &data->type;
	if ( data->type.kind == PDI_K_ARRAY ) {
		rank = data->type.c.array->ndims;
		h5sizes = malloc(rank*sizeof(hsize_t));
		h5subsizes = malloc(rank*sizeof(hsize_t));
		h5starts = malloc(rank*sizeof(hsize_t));
		order = data->type.c.array->order;
		int h5ii = 0;
		long intdim = 0;
		for ( int ii=0; ii<rank; ++ii ) {
			switch (order){
			case PDI_ORDER_C:
				h5ii = ii; break; // ORDER_C
			case PDI_ORDER_FORTRAN:
				h5ii = rank-ii-1; break; // ORDER_FORTRAN
			}
			
			PDI_value_int(&data->type.c.array->sizes[ii], &intdim);
			h5sizes[h5ii] = intdim;
			
			PDI_value_int(&data->type.c.array->subsizes[ii], &intdim);
			h5subsizes[h5ii] = intdim;
			
			PDI_value_int(&data->type.c.array->starts[ii], &intdim);
			h5starts[h5ii] = intdim;
		}
		scalart = &data->type.c.array->type;
	}
	if ( scalart->kind != PDI_K_SCALAR ) return;
	
	hid_t h5file;
	if ( is_h5_file(filename) ) {
		h5file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
	} else {
		h5file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	}
	
	rm_if_exist(h5file, pathname);
	
	hid_t h5fspace = H5Screate_simple(rank, h5subsizes, NULL);
	hid_t h5mspace = H5Screate_simple(rank, h5sizes, NULL);
	H5Sselect_hyperslab(h5mspace, H5S_SELECT_SET, h5starts, NULL, h5subsizes, NULL );
	hid_t h5lcp = H5Pcreate(H5P_LINK_CREATE);
	H5Pset_create_intermediate_group( h5lcp, 1 );
	hid_t h5set = H5Dcreate( h5file, pathname, h5type(scalart->c.scalar),
			h5fspace, h5lcp, H5P_DEFAULT, H5P_DEFAULT);
	H5Dwrite(h5set, h5type(scalart->c.scalar), h5mspace, H5S_ALL, H5P_DEFAULT,
			data->content[data->nb_content-1].data);

	H5Dclose(h5set);
	H5PTclose(h5lcp);
	H5Sclose(h5mspace);
	H5Sclose(h5fspace);
	H5Fclose(h5file);
	
	free(h5sizes);
	free(h5subsizes);
	free(h5starts);
}

int read_from_file(PDI_data_t *data, char *filename, char *pathname)
{
	int status=0;
	int rank = 0;
	int order = PDI_ORDER_C;
	hsize_t *sizes = NULL;
	hsize_t *subsizes = NULL;
	hsize_t *starts = NULL;
	PDI_type_t *scalart = &data->type;
	if ( data->type.kind == PDI_K_ARRAY ) {
		rank = data->type.c.array->ndims;
		sizes = malloc(rank*sizeof(hsize_t));
		subsizes = malloc(rank*sizeof(hsize_t));
		starts = malloc(rank*sizeof(hsize_t));
		order = data->type.c.array->order;
		int h5ii ;
		for ( int ii=0; ii<rank; ++ii ) {
			switch (order){
			case PDI_ORDER_C:
				h5ii = ii; break; // ORDER_C
			case PDI_ORDER_FORTRAN:
				h5ii = rank-ii-1; break; // ORDER_FORTRAN
			}
			long intdim;
			
			PDI_value_int(&data->type.c.array->sizes[ii], &intdim);
			sizes[h5ii] = intdim;
			
			PDI_value_int(&data->type.c.array->subsizes[ii], &intdim);
			subsizes[h5ii] = intdim;
			
			PDI_value_int(&data->type.c.array->starts[ii], &intdim);
			starts[h5ii] = intdim;
		}
		scalart = &data->type.c.array->type;
	}
	if ( scalart->kind != PDI_K_SCALAR ) return PDI_ERR_CONFIG;

	/// Open file for read/write
	hid_t file_id = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);

	if(file_id >= 0){ // File exists

		/// Open dataset "pathname" and gets its memspace
		hid_t dataset_id = H5Dopen(file_id, pathname, H5P_DEFAULT);
		if( dataset_id >= 0 ) { // Successfull

			hid_t dataspace_id = H5Dget_space (dataset_id);

			/// The dataspace from the file is left unmodified (ie get all data from file).

			/// Create data representation 
			hid_t memspace = H5Screate_simple(rank, sizes, NULL);
			
			/// Extract subspace of data 
			if ( data->type.kind == PDI_K_ARRAY ) {
			H5Sselect_hyperslab (memspace, H5S_SELECT_SET, starts, NULL, 
        		                          subsizes, NULL);
			}

			/// Read content 
			status = H5Dread( dataset_id, h5type(scalart->c.scalar), memspace, dataspace_id, H5P_DEFAULT,
					data->content[data->nb_content-1].data);
			
			H5Sclose(memspace);
			H5Sclose(dataspace_id);
			H5Dclose(dataset_id);

			if (status<0) status= PDI_UNAVAILABLE;
		} else {
			status = PDI_UNAVAILABLE;
		}
		H5Fclose(file_id);
	} else {
		status = PDI_UNAVAILABLE;
	}
	free(sizes);
	free(subsizes);
	free(starts);
	
	return status;
}

PDI_status_t PDI_hdf5_per_process_data_start( PDI_data_t *data )
{
	int status = PDI_OK;
	if ( data->content[data->nb_content-1].access & PDI_OUT ) {
		int found_output = 0;
		for ( int ii=0; ii<nb_outputs && !found_output; ++ii ) {
			if ( !strcmp(outputs[ii].name, data->name) ) {
				found_output = 1;
				
				char *h5file; PDI_value_str(&outputs[ii].h5file, &h5file);
				char *h5var;  PDI_value_str(&outputs[ii].h5var,  &h5var);
				long select;   PDI_value_int(&outputs[ii].select, &select);
				
				if ( select ) write_to_file(data, h5file, h5var);
				
				free(h5var);
				free(h5file);
			}
		}
	}
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		int found_input = 0;
		for ( int ii=0; ii<nb_inputs && !found_input; ++ii ) {
			if ( !strcmp(inputs[ii].name, data->name) ) {
				found_input = 1;
				
				char *h5file; PDI_value_str(&inputs[ii].h5file, &h5file);
				char *h5var;  PDI_value_str(&inputs[ii].h5var,  &h5var);
				long select;   PDI_value_int(&inputs[ii].select, &select);
				
				if ( select ) status = read_from_file(data, h5file, h5var);
				
				free(h5var);
				free(h5file);
			}
		}
	}
	return status;
}

PDI_status_t PDI_hdf5_per_process_data_end(PDI_data_t *data)
{
	data = data; // prevent unused warning
	return PDI_OK;
}

PDI_PLUGIN(hdf5_per_process)
