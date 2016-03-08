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

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>

MPI_Comm my_world;
PC_tree_t my_conf;

PDI_status_t PDI_hdf5_per_process_init(PC_tree_t conf, MPI_Comm *world)
{
	my_world = *world;
	my_conf = conf;
	
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_finalize()
{
	return PDI_OK;
}

PDI_status_t PDI_hdf5_per_process_event(const char *event)
{
	return PDI_OK;
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
		
		int rank; MPI_Comm_rank(MPI_COMM_WORLD, &rank);
		
		if ( select ) {
			fprintf(stderr, "HDF5[%d]: TODO output `%s' as `%s' in `%s'\n", rank, data->name, varname, filename);
		}
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
