/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#define FILE "d2s_test.h5"

int main()
{
	printf("HDF5 d2s_precision_write_test started\n");
	hid_t file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}

	hsize_t coords[2] = {100, 10};
	hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
	if (dataspace_id < 0) {
		return 1;
	}

	double dset_data[1000];
	for (int i = 0; i < 1000; i++) {
		dset_data[i] = i * 10.0 + 3.141592653589793;
	}

	hid_t dataset_id = H5Dcreate2(file_id, "/float_data", H5T_NATIVE_FLOAT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}

	herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data);
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

	printf("HDF5_C d2s_precision_write_test finalized\n");
	return 0;
}
