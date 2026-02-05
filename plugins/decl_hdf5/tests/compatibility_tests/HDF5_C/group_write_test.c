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
	printf("HDF5 group_write_test started\n");
	hid_t file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}

	hsize_t coords[2] = {5, 10};
	hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
	if (dataspace_id < 0) {
		return 1;
	}

	int dset_data[5][10];
	int dset_data_second[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			dset_data[i][j] = i * 10 + j;
			dset_data_second[i][j] = (i * 10 + j) * 10;
		}
	}

	hid_t group_id = H5Gcreate2(file_id, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	hid_t group_second_id = H5Gcreate2(file_id, "group_second", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	hid_t dataset_id = H5Dcreate2(group_id, "array_data", H5T_STD_I32BE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}
	hid_t dataset_id_second = H5Dcreate2(group_second_id, "array_data", H5T_STD_I32BE, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id_second < 0) {
		return 1;
	}
	herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data);
	if (status < 0) {
		return 1;
	}
	status = H5Dwrite(dataset_id_second, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data_second);
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
	status = H5Dclose(dataset_id_second);
	if (status < 0) {
		return 1;
	}
	status = H5Gclose(group_id);
	if (status < 0) {
		return 1;
	}
	status = H5Gclose(group_second_id);
	if (status < 0) {
		return 1;
	}
	status = H5Fclose(file_id);
	if (status < 0) {
		return 1;
	}

	printf("HDF5_C group_write_test finalized\n");
	return 0;
}
