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
#include <unistd.h>

#define FILE "dataset_test.h5"

int main()
{
	printf("HDF5 dataset_read_test started\n");
	hid_t file_id = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}
	
	int dset_data[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			dset_data[i][j] = 0;
		}
	}
	
	hid_t dataset_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}

	hsize_t count[2] = {5, 5};
	hsize_t stride[2] = {1, 1};
	hsize_t dataset_offset[2] = {0, 0};
	hsize_t memory_offset[2] = {0, 5};
	hsize_t block[2] = {1, 1};

	hid_t dataspace_id = H5Dget_space(dataset_id);
	herr_t status = H5Sselect_hyperslab(dataspace_id, H5S_SELECT_SET, dataset_offset, stride, count, block);
	if (status < 0) {
		return 1;
	}
	hid_t memory_dataspace_id = H5Scopy(dataspace_id);
	if (memory_dataspace_id < 0) {
		return 1;
	}
	status = H5Sselect_hyperslab(memory_dataspace_id, H5S_SELECT_SET, memory_offset, stride, count, block);
	if (status < 0) {
		return 1;
	}

	status = H5Dread(dataset_id, H5T_NATIVE_INT, memory_dataspace_id, dataspace_id, H5P_DEFAULT, dset_data);
	if (status < 0) {
		return 1;
	}
	// swap memory_dataspace selection with dataset dataspace
	status = H5Dread(dataset_id, H5T_NATIVE_INT, dataspace_id, memory_dataspace_id, H5P_DEFAULT, dset_data);
	if (status < 0) {
		return 1;
	}

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (dset_data[i][j] != i * 10 + j) {
				fprintf(stderr, "%d != %d\n ", dset_data[i][j], i * 10 + j);
				return 1;
			}
		}
	}
	
	status = H5Sclose(memory_dataspace_id);
	if (status < 0) {
		return 1;
	}
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
	
	printf("HDF5_C dataset_read_test finalized\n");
	return 0;
}
