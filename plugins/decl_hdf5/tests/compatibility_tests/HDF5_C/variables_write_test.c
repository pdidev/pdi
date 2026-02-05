/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <hdf5.h>
#include <stdlib.h>
#include <unistd.h>

#define FILE "variables_test.h5"

void write_dataset(hid_t file_id, const char* name, hid_t type, const void* data)
{
	hid_t dataspace_id = H5Screate(H5S_SCALAR);
	if (dataspace_id < 0) {
		exit(1);
	}
	hid_t dataset_id = H5Dcreate2(file_id, name, type, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) {
		exit(1);
	}
	herr_t status = H5Dwrite(dataset_id, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
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
	printf("HDF5 variables_write_test started\n");
	hid_t file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		exit(1);
	}
	int int_data = 15;
	write_dataset(file_id, "/int_data", H5T_NATIVE_INT, &int_data);
	double double_data = 55.26;
	write_dataset(file_id, "/double_data", H5T_NATIVE_DOUBLE, &double_data);
	float float_data = 3.5;
	write_dataset(file_id, "/float_data", H5T_NATIVE_FLOAT, &float_data);
	char char_data = 'z';
	write_dataset(file_id, "/char_data", H5T_NATIVE_CHAR, &char_data);

	herr_t status = H5Fclose(file_id);
	if (status < 0) {
		exit(1);
	}

	printf("HDF5_C variables_write_test finalized\n");
	return 0;
}
