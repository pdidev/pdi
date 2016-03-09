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

#include <string.h>
#include <mpi.h>
#include <hdf5/serial/hdf5.h>
#include <hdf5/serial/hdf5_hl.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>

MPI_Comm my_world;
PC_tree_t my_conf;

PDI_status_t PDI_hdf5_per_process_init(PC_tree_t conf, MPI_Comm *world)
{
	my_world = *world;
	my_conf = conf;
	
	H5open();
	
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_finalize()
{
	H5close();
	
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_event(const char *event)
{
	return PDI_OK;
}

char *topdir(char **path)
{
	while ( **path && **path == '/' ) { ++*path; }
	char* separator = *path;
	while ( *separator && *separator != '/' ) { ++separator; }
	if ( !*separator ) return NULL;
	char *result = malloc(separator-*path+1);
	memcpy(result, *path, separator-*path);
	result[separator-*path] = 0;
	*path = separator;
	return result;
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
	}
}

void write_to_file(PDI_variable_t *data, char *filename, char *pathname)
{
	int rank = 0;
	hsize_t *h5sizes = NULL;
	hsize_t *h5subsizes = NULL;
	hsize_t *h5starts = NULL;
	PDI_type_t *scalart = &data->type;
	if ( data->type.kind = PDI_K_ARRAY ) {
		rank = data->type.c.array->ndims;
		h5sizes = malloc(rank*sizeof(hsize_t));
		h5subsizes = malloc(rank*sizeof(hsize_t));
		h5starts = malloc(rank*sizeof(hsize_t));
		for ( int ii=0; ii<rank; ++ii ) {
			int intdim;
			
			PDI_value_int(&data->type.c.array->sizes[ii], &intdim);
			h5sizes[ii] = intdim;
			
			PDI_value_int(&data->type.c.array->subsizes[ii], &intdim);
			h5subsizes[ii] = intdim;
			
			PDI_value_int(&data->type.c.array->starts[ii], &intdim);
			h5starts[ii] = intdim;
		}
		scalart = &data->type.c.array->type;
	}
	if ( scalart->kind != PDI_K_SCALAR ) return;
	
	hid_t h5file;
	herr_t status = H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	if ( H5Fis_hdf5(filename)>0 ) {
		h5file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
	} else {
		h5file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	}
	hid_t h5fspace = H5Screate_simple(rank, h5subsizes, NULL);
	hid_t h5mspace = H5Screate_simple(rank, h5sizes, NULL);
	H5Sselect_hyperslab(h5mspace, H5S_SELECT_SET, h5starts, NULL, h5subsizes, NULL );
	free(h5sizes);
	hid_t h5lcp = H5Pcreate(H5P_LINK_CREATE);
	H5Pset_create_intermediate_group( h5lcp, 1 );
	hid_t h5set = H5Dcreate( h5file, pathname, h5type(scalart->c.scalar), h5fspace, h5lcp, H5P_DEFAULT, H5P_DEFAULT);
    H5Dwrite(h5set, h5type(scalart->c.scalar), h5mspace, H5S_ALL, H5P_DEFAULT, data->content.data);
	H5Dclose(h5set);
	H5PTclose(h5lcp);
	H5Sclose(h5fspace);
	
	H5Fclose(h5file);
}

PDI_status_t PDI_hdf5_per_process_data_start(PDI_variable_t *data)
{
	int nb_outputs; PC_len(PC_get(my_conf, ".outputs"), &nb_outputs);
	PC_tree_t output;
	int found_output = 0;
	for ( int ii=0; ii<nb_outputs && !found_output; ++ii ) {
		char *output_name; 
		PC_string(PC_get(my_conf, ".outputs{%d}", ii), &output_name);
		if ( !strcmp(output_name, data->name) ) {
			output = PC_get(my_conf, ".outputs<%d>", ii);
			found_output = 1;
		}
		free(output_name);
	}
	if ( found_output ) {
		char *file_strv; PC_string(PC_get(output, ".file"), &file_strv);
		PDI_value_t file_val; PDI_value_parse(file_strv, &file_val);
		free(file_strv);
		char *filename; PDI_value_str(&file_val, &filename);
		PDI_value_destroy(&file_val);
		
		char *var_strv; PC_string(PC_get(output, ".var"), &var_strv);
		PDI_value_t var_val; PDI_value_parse(var_strv, &var_val);
		free(var_strv);
		char *varname; PDI_value_str(&var_val, &varname);
		PDI_value_destroy(&var_val);
		
		char *select_strv; PC_string(PC_get(output, ".select"), &select_strv);
		PDI_value_t select_val; PDI_value_parse(select_strv, &select_val);
		free(select_strv);
		int select; PDI_value_int(&select_val, &select);
		PDI_value_destroy(&select_val);
		
		if ( select ) write_to_file(data, filename, varname);
		
		free(varname);
		free(filename);
	}
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_data_end(PDI_variable_t *data)
{
	return PDI_OK;
}

PDI_PLUGIN(hdf5_per_process)
