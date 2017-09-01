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
#include <stdbool.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/data.h>
#include <pdi/value.h>

PC_tree_t my_conf;

typedef struct hdf5pp_var_s
{
	char *name;
	
	PDI_value_t h5file;
	
	PDI_value_t h5var; // changed to char * for the sake of simplicity 
	
	PDI_value_t select;
	
	// Global offet and size of the dataset
	PDI_value_t *gstarts;
	PDI_value_t *gsizes;
	
} hdf5pp_var_t;

int nb_outputs = 0;

hdf5pp_var_t *outputs = NULL;

int nb_inputs = 0;

hdf5pp_var_t *inputs = NULL;


#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = (char *) malloc(strlen(s)+1);
	if ( p ) strcpy(p, s);
	return p;
}
#endif


PDI_status_t set_parallel_extent(hdf5pp_var_t *var, const char *scalar_start, const char* scalar_size) // return PDI_OK if succeed
{
	PDI_status_t status = PDI_OK;

	PDI_data_t *data = PDI_find_data(var->name);
	if(!data) return PDI_ERR_SYSTEM;
	

	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	if ( data->type.kind == PDI_K_SCALAR ){
		char *tmp = NULL ;
		var->gstarts = (PDI_value_t*) malloc(sizeof(PDI_value_t)); 
		if( PC_string(PC_get(data->config, ".global_start"), &tmp)){
			if ( scalar_start ){
				tmp = strdup(scalar_start);
			} else {
				fprintf(stderr, "Var name %s\n",var->name); 
				fprintf(stderr, "[PDI/Parallel_DeclH5] %s not found in data (no default value for scalar data)\n", "global_start");
				return PDI_ERR_CONFIG;
			}
		}
		status = PDI_value_parse(tmp, &(var->gstarts[0]));
		free(tmp);

		var->gsizes = (PDI_value_t*) malloc(sizeof(PDI_value_t));
		if( PC_string(PC_get(data->config, ".global_size"), &tmp)){
			if ( scalar_size ){
				tmp = strdup(scalar_size);
			} else {
				fprintf(stderr, "Var name %s\n",var->name); 
				fprintf(stderr, "[PDI/Parallel_DeclH5] %s not found in data (no default value for scalar data)\n", "global_size");
				return PDI_ERR_CONFIG;
			}
		}
		status = PDI_value_parse(tmp, &(var->gsizes[0]));
		free(tmp);

	} else {

		// Starts (offset where the local array begins on the distributed array)
		
		PC_tree_t treetmp = PC_get(data->config, ".global_starts");
		if(PC_status(treetmp)) {
			fprintf(stderr, "For var named %s \n",var->name); 
			fprintf(stderr, "[PDI/Parallel_DeclH5] %s not found in data \n", "global_starts");
			return PDI_ERR_CONFIG;
		}

		int len; PC_len(treetmp, &len);
		var->gstarts = (PDI_value_t*) malloc(len*sizeof(PDI_value_t)); 
		for ( int ii=0; ii<len; ++ii ) {
			char *expr = NULL;
			PC_string(PC_get(treetmp, "[%d]", ii), &expr) ; 
			status = PDI_value_parse(expr, &(var->gstarts[ii]));
			if(status) return status;
		}

		// Sizes (Size of the distributed array)

		treetmp = PC_get(data->config, ".global_sizes");
		if(PC_status(treetmp)) {
			fprintf(stderr, "[PDI/Parallel_DeclH5] %s not found in data \n", "global_sizes");
			return PDI_ERR_CONFIG;
		}

		PC_len(treetmp, &len);
		var->gsizes  = (PDI_value_t*) malloc(len*sizeof(PDI_value_t));
		for ( int ii=0; ii<len; ++ii ) {
			char *expr = NULL;
			PC_string(PC_get(treetmp, "[%d]", ii), &expr) ; 
			status = PDI_value_parse(expr, &(var->gsizes[ii]));
			if(status) return status;
		}
	}

	PC_errhandler(errh);
	return PDI_OK;
}


/** Reads the plugin config for either inputs or outputs
 * \param conf the configuration node to read
 * \param hdf5data the array where to store the resulting variables
 * \param nb_hdf5data somewhere where to store the number of variables found
 * \param def_file the default file for HDF5 inputs/outputs
 * \param def_select the default select for HDF5 inputs/outputs
 */
PDI_status_t read_config_file( PC_tree_t conf, hdf5pp_var_t *hdf5data[],
		int *nb_hdf5data, char *def_file, char *def_select,
		const char* scal_start, const char* scal_size)
{
	PDI_status_t status = PDI_OK;
	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	
	if ( PC_len(conf, nb_hdf5data) ) { // if no subtree found
		*nb_hdf5data = 0;
		*hdf5data = NULL;
		PC_errhandler(errh);
		return PDI_OK;
	}
	*hdf5data = (hdf5pp_var_t*) malloc((*nb_hdf5data)*sizeof(hdf5pp_var_t));
	
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
		
		status = set_parallel_extent( &(*hdf5data)[ii], scal_start, scal_size);
		if(status){
			fprintf(stderr, "[PDI/Parallel_DeclH5] cannot run in parallel \n");
			return status;
		}

	}
	PC_errhandler(errh);

	return PDI_OK;
}


PDI_status_t PDI_parallel_declh5_init(PC_tree_t conf, MPI_Comm *world)
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
	char *scal_start = NULL, *scal_size = NULL;
	PC_string(PC_get(my_conf, ".defaults.scalars.global_start"), &scal_start);
	PC_string(PC_get(my_conf, ".defaults.scalars.global_size"), &scal_size);

	char *def_out_file = NULL; // default output file if none specified
	PC_string(PC_get(my_conf, ".defaults.outputs.file"), &def_out_file);
	char *def_out_select = NULL; // default output select if none specified
	PC_string(PC_get(my_conf, ".defaults.outputs.select"), &def_out_select);
	PC_tree_t outputs_cfg = PC_get(conf, ".outputs");
	PC_errhandler(errh);
	
	
	PDI_status_t status = read_config_file(outputs_cfg, &outputs, &nb_outputs,
			def_out_file, def_out_select, scal_start, scal_size);
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
			def_in_select, scal_start, scal_size);
	free(def_in_file);
	free(def_in_select);

	free(scal_start);
	free(scal_size);
	
	return status;
}


PDI_status_t PDI_parallel_declh5_finalize()
{
	for ( int ii=0; ii<nb_outputs; ++ii ) {
		PDI_value_destroy(&outputs[ii].h5file);
		PDI_value_destroy(&outputs[ii].h5var);
		PDI_value_destroy(&outputs[ii].select);
		free(outputs[ii].name);
	}
	free(outputs);

	for ( int ii=0; ii<nb_inputs; ++ii ) {
		PDI_value_destroy(&inputs[ii].h5file);
		PDI_value_destroy(&inputs[ii].h5var);
		PDI_value_destroy(&inputs[ii].select);
		free(inputs[ii].name);
	}
	free(inputs);
	return PDI_OK;
}


PDI_status_t PDI_parallel_declh5_event(const char *event)
{
	event = event; // prevent unused warning
	return PDI_OK;
}

hid_t h5type(PDI_scalar_type_t ptype) {
	switch (ptype) {
	case PDI_T_INT8: return  H5T_NATIVE_CHAR;
	case PDI_T_INT16: return  H5T_NATIVE_SHORT;
	case PDI_T_INT32: return  H5T_NATIVE_INT;
	case PDI_T_INT64: return  H5T_NATIVE_LONG;
	case PDI_T_FLOAT: return  H5T_NATIVE_FLOAT;
	case PDI_T_DOUBLE: return  H5T_NATIVE_DOUBLE;
	case PDI_T_LONG_DOUBLE: return  H5T_NATIVE_LDOUBLE;
	case PDI_T_UNDEF: break;
	}
	fprintf(stderr, "[PDI/HDF5] Type is invalid (line %d) \n",__LINE__);
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


PDI_datatype_t* init_sizes(hsize_t **sizes, hsize_t** subsizes, hsize_t **starts, hsize_t *rank, PDI_data_t *data)
{
	PDI_datatype_t *scalart = &data->type;
	if ( data->type.kind == PDI_K_ARRAY ) {
		*rank = data->type.c.array->ndims;
		*sizes = (hsize_t*) malloc(*rank*sizeof(hsize_t));
		*subsizes = (hsize_t*) malloc(*rank*sizeof(hsize_t));
		*starts = (hsize_t*) malloc(*rank*sizeof(hsize_t));
		for ( unsigned int ii=0; ii<*rank; ++ii ) {
			long intdim;
			
			PDI_value_int(&data->type.c.array->sizes[ii], &intdim);
			(*sizes)[ii] = intdim;
			
			PDI_value_int(&data->type.c.array->subsizes[ii], &intdim);
			(*subsizes)[ii] = intdim;
			
			PDI_value_int(&data->type.c.array->starts[ii], &intdim);
			(*starts)[ii] = intdim;
		}
		scalart = &data->type.c.array->type;
	} else { // assuming scalar type 
		*rank = 1;
		*sizes = (hsize_t*) malloc(sizeof(hsize_t));
		*subsizes = (hsize_t*) malloc(sizeof(hsize_t));
		*starts = (hsize_t*) malloc(sizeof(hsize_t));
		*sizes[0] = 1;  
		*subsizes[0] = 1; 
		*starts[0] =  0;
	}
	if ( scalart->kind != PDI_K_SCALAR ) return NULL;

	return scalart;
}


PDI_status_t pwrite_to_file(PDI_data_t *data, char *filename, char *pathname, hsize_t *gstarts, hsize_t *gsizes)
{
	hsize_t rank = 0;
	hsize_t *sizes = NULL;
	hsize_t *subsizes = NULL;
	hsize_t *starts = NULL;
	
	/// Setting sizes, subsizes, offset
	PDI_datatype_t *scalart = init_sizes(&sizes, &subsizes, &starts, &rank, data);
	

	/// Setting files properties
	hid_t file;
	hid_t plist_id = H5Pcreate(H5P_FILE_ACCESS);
	H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)

	if ( is_h5_file(filename) ) {
		file = H5Fopen(filename, H5F_ACC_RDWR, plist_id);  
		rm_if_exist(file, pathname);
	} else {
		file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, plist_id);
	}

	// The total size (including ghost..) = data is spread along processor
	hid_t dataspace = H5Screate_simple(rank, gsizes, NULL);   // space on disk
	hid_t dataset = H5Dcreate( file, pathname, h5type(scalart->c.scalar), dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	// File space: part of the global file that we use to store our piece of array
	hid_t filespace = H5Screate_simple(rank, gsizes , NULL);
	H5Sselect_hyperslab(filespace, H5S_SELECT_SET, gstarts, NULL, subsizes, NULL ); 
	
	// The size in memory (with ghost, data is sparse here)
	hid_t memspace = H5Screate_simple(rank, sizes, NULL);
	H5Sselect_hyperslab(memspace, H5S_SELECT_SET, starts, NULL, subsizes, NULL ); // todo: read one more data (offset in file and file size)

	hid_t plist_id2 = H5Pcreate(H5P_DATASET_XFER);
	H5Pset_dxpl_mpio(plist_id2, H5FD_MPIO_COLLECTIVE);
	//       dataset, datatype                 , selection in memory , selection within the file dataset dataspace , properties , buffer );
	H5Dwrite(dataset, h5type(scalart->c.scalar), memspace, filespace, plist_id2, data->content[data->nb_content-1].data);

	// closing 
	H5Sclose(memspace);
	H5Sclose(filespace);
	H5Sclose(dataspace);
	
	H5Dclose(dataset);
	
	H5Fclose(file);
	
	free(sizes);
	free(subsizes);
	free(starts);

	return PDI_OK;
}


PDI_status_t pread_from_file(PDI_data_t *data, char *filename, char *pathname, hsize_t *gstarts)
{
	PDI_status_t status= PDI_OK;
	hsize_t rank = 0;
	hsize_t *sizes = NULL;
	hsize_t *subsizes = NULL;
	hsize_t *starts = NULL;

	/// Setting sizes, subsizes, offset
	PDI_datatype_t *scalart = init_sizes(&sizes, &subsizes, &starts, &rank, data);

	hid_t plist_id = H5Pcreate(H5P_FILE_ACCESS);
	H5Pset_fapl_mpio(plist_id, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)
	/// Open file for read/write
	hid_t file_id = H5Fopen(filename, H5F_ACC_RDWR, plist_id);

	if(file_id >= 0){ // File exists


		/// Open dataset "pathname" and gets its memspace
		hid_t dataset_id = H5Dopen(file_id, pathname, H5P_DEFAULT);

		if( dataset_id >= 0 ) { // Successfull

			hid_t dataspace_id = H5Dget_space (dataset_id);
			// create a view on the data space
			H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, gstarts, NULL, subsizes, NULL);

			/// Create data representation 
			hid_t memspace = H5Screate_simple(rank, sizes, NULL);
			
			/// Extract subspace of data 
			if ( data->type.kind == PDI_K_ARRAY ) {
			H5Sselect_hyperslab (memspace, H5S_SELECT_SET, starts, NULL, 
        		                          subsizes, NULL);
			}

			/// Read content 
			hid_t plist_id2 = H5Pcreate(H5P_DATASET_XFER);
			H5Pset_dxpl_mpio(plist_id2, H5FD_MPIO_COLLECTIVE);
			if( 0 > H5Dread( dataset_id, h5type(scalart->c.scalar), memspace, dataspace_id, plist_id2,
					data->content[data->nb_content-1].data)) status = PDI_ERR_SYSTEM;
			
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


typedef enum PDI_order_e {
	PDI_ORDER_INVALID = -1,
	PDI_ORDER_C,
	PDI_ORDER_FORTRAN
} PDI_order_t;


PDI_order_t array_order(PC_tree_t node)
{
	PDI_order_t order = PDI_ORDER_C;
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		char *order_str = strdup("c");
		PC_string(PC_get(node, ".order"), &order_str);
		if ((!strcmp(order_str, "c")) || (!strcmp(order_str, "C"))) {
			order = PDI_ORDER_C;
		} else if ((!strcmp(order_str, "fortran")) || (!strcmp(order_str, "Fortran"))) {
			order = PDI_ORDER_FORTRAN;
		} else {
			order = PDI_ORDER_INVALID;
		}
		free(order_str);
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	return order;
}

PDI_status_t PDI_parallel_declh5_data_start( PDI_data_t *data )
{
	PDI_status_t status = PDI_OK;
	if ( data->content[data->nb_content-1].access & PDI_OUT ) {
		int found_output = 0;
		for ( int ii=0; ii<nb_outputs && !found_output; ++ii ) {
			if ( !strcmp(outputs[ii].name, data->name) ) {
				found_output = 1;
				
				char *h5file; PDI_value_str(&outputs[ii].h5file, &h5file);
				char *h5var ; PDI_value_str(&outputs[ii].h5var, &h5var);
				long select;  PDI_value_int(&outputs[ii].select, &select);

				// TODO: warn user, assuming size is unchanged (unknow consequence when size is changed...)
				hsize_t *gstarts = NULL; 
				hsize_t *gsizes = NULL;  
				if ( data->type.kind == PDI_K_ARRAY ) {
					PDI_order_t order; 
					if( (order = array_order(data->config)) < 0 )
						return PDI_ERR_CONFIG;
					int rank = data->type.c.array->ndims;
					gstarts = (hsize_t*) malloc(rank*sizeof(hsize_t));
					gsizes = (hsize_t*) malloc(rank*sizeof(hsize_t));
					for ( int jj = 0 ; jj < rank; ++jj){
						int val = jj;
						if(order == PDI_ORDER_FORTRAN) val = rank-1-jj ;
						long n=0; PDI_value_int(&outputs[ii].gstarts[val], &n);
						gstarts[jj] = n;
						
						PDI_value_int(&outputs[ii].gsizes[val], &n);
						gsizes[jj] = n;
					}
				} else { // supposing scalar case 
					gstarts = (hsize_t*) malloc(sizeof(hsize_t));
					gsizes = (hsize_t*) malloc(sizeof(hsize_t));
					long n=0; PDI_value_int(&outputs[ii].gstarts[0], &n);
					gstarts[0] = n;
					
					n = 0;
					PDI_value_int(&outputs[ii].gsizes[0], &n);
					gsizes[0] = n;
				}

				if ( select ) pwrite_to_file(data, h5file, h5var, gstarts, gsizes);
				
				free(h5var);
				free(h5file);
				
				free(gstarts);
				free(gsizes);
			}
		}
	}
	if ( data->content[data->nb_content-1].access & PDI_IN ) {
		status = PDI_UNAVAILABLE;
		int found_input = 0;
		for ( int ii=0; ii<nb_inputs && !found_input; ++ii ) {
			if ( !strcmp(inputs[ii].name, data->name) ) {
				found_input = 1;
				
				char *h5file; PDI_value_str(&inputs[ii].h5file, &h5file);
				char *h5var; PDI_value_str(&inputs[ii].h5var, &h5var);
				long select; PDI_value_int(&inputs[ii].select, &select);
				
				hsize_t *gstarts = NULL; 
				hsize_t *gsizes = NULL; 
				if ( data->type.kind == PDI_K_ARRAY ) {
					PDI_order_t order;
					if( (order = array_order(data->config)) < 0 )
						return PDI_ERR_CONFIG;
					int rank = data->type.c.array->ndims;
					gstarts = (hsize_t*) malloc(rank*sizeof(hsize_t));
					gsizes = (hsize_t*) malloc(rank*sizeof(hsize_t));
					for ( int jj = 0 ; jj < rank; ++jj){
						int val = jj;
						if(order == PDI_ORDER_FORTRAN) val = rank-1-jj ;
						long n=0; PDI_value_int(&outputs[ii].gstarts[val], &n);
						gstarts[jj] = n;
						
						PDI_value_int(&outputs[ii].gsizes[val], &n);
						gsizes[jj] = n;
					}
				} else { // supposing scalar case 
					gstarts = (hsize_t*) malloc(sizeof(hsize_t));
					gsizes = (hsize_t*) malloc(sizeof(hsize_t));
					long n=0; PDI_value_int(&outputs[ii].gstarts[0], &n);
					gstarts[0] = n;
					
					n = 0;
					PDI_value_int(&outputs[ii].gsizes[0], &n);
					gsizes[0] = n;
				}

				if ( select ) status = pread_from_file(data, h5file, h5var, gstarts);
				
				free(h5var);
				free(h5file);
				
				free(gstarts);
				free(gsizes);
			}
		}
	}
	return status;
}

PDI_status_t PDI_parallel_declh5_data_end(PDI_data_t *data)
{
	data = data; // prevent unused warning
	return PDI_OK;
}

PDI_PLUGIN(parallel_declh5)
