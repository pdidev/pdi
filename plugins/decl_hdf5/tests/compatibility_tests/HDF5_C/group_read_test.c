/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <hdf5.h>
#include <unistd.h>

#define FILE "group_test.h5"

int main()
{
	printf("HDF5 group_read_test started\n");
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

	printf("HDF5_C group_read_test finalized\n");
	return 0;
}
