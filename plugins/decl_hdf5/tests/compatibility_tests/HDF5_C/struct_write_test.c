/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <hdf5.h>
#include <unistd.h>

#define FILE "struct_test.h5"

typedef struct test_struct {
	char a;
	int b[5][10];
} test_struct;

int main()
{
	printf("HDF5 struct_write_test started\n");
	hid_t file_id = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) {
		return 1;
	}

	hid_t dataspace_id = H5Screate(H5S_SCALAR);
	if (dataspace_id < 0) {
		return 1;
	}

	test_struct record_data;

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			record_data.b[i][j] = i * 10 + j;
		}
	}
	record_data.a = 'a';

	hsize_t coords[2] = {5, 10};
	hid_t array_type_id = H5Tarray_create(H5T_STD_I32LE, 2, coords);

	hid_t test_struct_id = H5Tcreate(H5T_COMPOUND, sizeof(test_struct));
	H5Tinsert(test_struct_id, "a_name", HOFFSET(test_struct, a), H5T_STD_I8LE);
	H5Tinsert(test_struct_id, "b_name", HOFFSET(test_struct, b), array_type_id);

	hid_t dataset_id = H5Dcreate(file_id, "test_struct", test_struct_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) {
		return 1;
	}

	herr_t status = H5Dwrite(dataset_id, test_struct_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
	if (status < 0) {
		return 1;
	}

	status = H5Tclose(array_type_id);
	if (status < 0) {
		return 1;
	}
	status = H5Tclose(test_struct_id);
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

	printf("HDF5_C struct_write_test finalized\n");
	return 0;
}
