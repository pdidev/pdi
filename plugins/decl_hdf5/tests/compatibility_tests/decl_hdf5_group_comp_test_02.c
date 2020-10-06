/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <assert.h>
#include <hdf5.h>
#include <pdi.h>
#include <unistd.h>

#define FILE "hdf5_group_comp_test_02.h5"

int PDI_write()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                       \n"
	    "data:                                                                \n"
	    "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
	    "  array_data_second: { size: [5, 10], type: array, subtype: int }    \n"
	    "plugins:                                                             \n"
	    "  decl_hdf5:                                                         \n"
	    "    file: hdf5_group_comp_test_02.h5                                 \n"
	    "    on_event: write_event                                            \n"
	    "    datasets:                                                        \n"
	    "      group/array_data:                                              \n"
	    "        size: [5, 10]                                                \n"
	    "        type: array                                                  \n"
	    "        subtype: int                                                 \n"
	    "      group_second/array_data:                                       \n"
	    "        size: [5, 10]                                                \n"
	    "        type: array                                                  \n"
	    "        subtype: int                                                 \n"
	    "    write:                                                           \n"
	    "      array_data:                                                    \n"
	    "        - dataset: group/array_data                                  \n"
	    "      array_data_second:                                             \n"
	    "        - dataset: group_second/array_data                           \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int test_array[5][10];
	int test_array_second[5][10];
	
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
			test_array_second[i][j] = (i * 10 + j) * 10;
		}
	}
	
	PDI_multi_expose( "write_event",
	    "array_data", test_array, PDI_OUT,
	    "array_data_second", test_array_second, PDI_OUT,
	    NULL);
	    
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int HDF5_read()
{
	hid_t file_id = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}
	
	hid_t dataset_id = H5Dopen2(file_id, "group/array_data", H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}
	hid_t dataset_id_second = H5Dopen2(file_id, "group_second/array_data", H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}
	
	int dset_data[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			dset_data[i][j] = 0;
		}
	}
	int dset_data_second[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			dset_data_second[i][j] = 0;
		}
	}
	
	herr_t status = H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data);
	if (status < 0) {
		return 1;
	}
	status = H5Dread(dataset_id_second, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data_second);
	if (status < 0) {
		return 1;
	}
	
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (dset_data[i][j] != i * 10 + j) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, dset_data[i][j], i * 10 + j);
				return 1;
			} else if (dset_data_second[i][j] != (i * 10 + j) * 10) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, dset_data_second[i][j], i * 10 + j);
				return 1;
			}
		}
	}
	
	status = H5Dclose(dataset_id);
	if (status < 0) {
		return 1;
	}
	status = H5Dclose(dataset_id_second);
	if (status < 0) {
		return 1;
	}
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}

int main()
{
	int status = PDI_write();
	if (status !=0) {
		return status;
	}
	status = HDF5_read();
	if (status !=0) {
		return status;
	}
	
	return 0;
}
