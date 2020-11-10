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
#include <stdlib.h>
#include <unistd.h>

#define FILE "variables_test.h5"

void read_dataset(hid_t file_id, const char* name, hid_t type, void* data)
{
	hid_t dataspace_id = H5Screate(H5S_SCALAR);
	if (dataspace_id < 0) {
		exit(1);
	}
	hid_t dataset_id = H5Dopen2(file_id, name, H5P_DEFAULT);
	if (dataset_id < 0) {
		exit(1);
	}
	herr_t status = H5Dread(dataset_id, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	if (status < 0) {
		exit(1);
	}
	
	status = H5Sclose(dataspace_id);
	if (status < 0) {
		exit(1);
	}
	status = H5Dclose(dataset_id);
	if (status < 0) {
		exit(1);
	}
}

int main()
{
	printf("HDF5 variables_read_test started\n");
	hid_t file_id = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		exit(1);
	}
	int int_data = 0;
	read_dataset(file_id, "/int_data", H5T_NATIVE_INT, &int_data);
	double double_data = 0.0;
	read_dataset(file_id, "/double_data", H5T_NATIVE_DOUBLE, &double_data);
	float float_data = 0.0f;
	read_dataset(file_id, "/float_data", H5T_NATIVE_FLOAT, &float_data);
	char char_data = 0;
	read_dataset(file_id, "/char_data", H5T_NATIVE_CHAR, &char_data);
	
	if (int_data != 15) {
		fprintf(stderr, "%d != %d\n ", int_data, 15);
		exit(1);
	}
	if (double_data != 55.26) {
		fprintf(stderr, "%f != %f\n ", double_data, 55.26);
		exit(1);
	}
	if (float_data != 3.5) {
		fprintf(stderr, "%f != %f\n ", float_data, 3.5);
		exit(1);
	}
	if (char_data != 'z') {
		fprintf(stderr, "%c != %c\n ", char_data, 'z');
		exit(1);
	}
	
	herr_t status = H5Fclose(file_id);
	if (status < 0) {
		exit(1);
	}
	
	printf("HDF5_C variables_read_test finalized\n");
	return 0;
}
