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

#define FILE "hdf5_dataset_comp_test_01.h5"


int HDF5_write()
{
	hid_t file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}
	
	hsize_t coords[2] = {5, 10};
	hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
	if (dataspace_id < 0) {
		return 1;
	}
	
	int test_array[5][10];
	for (int i = 0; i<5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
		}
	}
	hid_t dataset_id = H5Dcreate2(file_id, "/array_data", H5T_STD_I32BE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}
	herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, test_array);
	
	status = H5Sclose(dataspace_id);
	if (status < 0) {
		return 1;
	}
	status = H5Dclose(dataset_id);
	if (status < 0) {
		return 1;
	}
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}

int PDI_read()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                 \n"
	    "data:                                                          \n"
	    "  array_data: { size: [5, 10], type: array, subtype: int }     \n"
	    "plugins:                                                       \n"
	    "  decl_hdf5:                                                   \n"
	    "    file: hdf5_dataset_comp_test_01.h5                         \n"
	    "    datasets:                                                  \n"
	    "      array_data: {type: array, subtype: int, size: [5, 10]}   \n"
	    "    read:                                                      \n"
	    "      array_data:                                              \n"
	    "        - memory_selection:                                    \n"
	    "            size: [5, 5]                                       \n"
	    "          dataset_selection:                                   \n"
	    "            size: [5, 5]                                       \n"
	    "            start: [0, 5]                                      \n"
	    "        - memory_selection:                                    \n"
	    "            size: [5, 5]                                       \n"
	    "            start: [0, 5]                                      \n"
	    "          dataset_selection:                                   \n"
	    "            size: [5, 5]                                       \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int test_array[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = 0;
		}
	}
	PDI_expose("array_data", test_array, PDI_IN);
	
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 5; j++) {
			if (test_array[i][j] != i * 10 + j + 5) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array[i][j], i * 10 + j + 5);
				return 1;
			}
		}
	}
	for (int i = 0; i < 5; i++) {
		for (int j = 5; j < 10; j++) {
			if (test_array[i][j] != i * 10 + j - 5) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array[i][j], i * 10 + j - 5);
				return 1;
			}
		}
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int main()
{
	int status = HDF5_write();
	if (status !=0) {
		return status;
	}
	status = PDI_read();
	if (status !=0) {
		return status;
	}
	
	return 0;
}
