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

#define FILE "struct_test.h5"

typedef struct test_struct {
	char a;
	int b[5][10];
} test_struct;

int main()
{
	printf("HDF5 struct_read_test started\n");
	hid_t file_id = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		exit(1);
	}
	
	hid_t dataset_id = H5Dopen2(file_id, "/test_struct", H5P_DEFAULT);
	if (dataset_id < 0) {
		exit(1);
	}
	
	test_struct record_data;
	record_data.a = 0;
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			record_data.b[i][j] = 0;
		}
	}
	
	hsize_t coords[2] = {5, 10};
	hid_t array_type_id = H5Tarray_create(H5T_NATIVE_INT, 2, coords);
	
	hid_t test_struct_id = H5Tcreate (H5T_COMPOUND, sizeof(test_struct));
	H5Tinsert(test_struct_id, "a_name", HOFFSET(test_struct, a), H5T_NATIVE_CHAR);
	H5Tinsert(test_struct_id, "b_name", HOFFSET(test_struct, b), array_type_id);
	
	herr_t status = H5Dread(dataset_id, test_struct_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
	if (status < 0) {
		exit(1);
	}
	
	if (record_data.a != 2) {
		fprintf(stderr, "%c != 'a' \n", record_data.a);
	}
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (record_data.b[i][j] != i * 10 + j) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, record_data.b[i][j], i * 10 + j);
				for (int k = 0; k < 5; k++) {
					for ( int l = 0; l < 10; l++) {
						fprintf(stderr, "%d ", record_data.b[k][l]);
					}
					fprintf(stderr, "\n");
				}
				exit(1);
			}
		}
	}
	
	status = H5Tclose(test_struct_id);
	if (status < 0) {
		exit(1);
	}
	
	status = H5Tclose(array_type_id);
	if (status < 0) {
		exit(1);
	}
	
	status = H5Dclose(dataset_id);
	if (status < 0) {
		exit(1);
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		exit(1);
	}
	
	printf("HDF5_C struct_read_test finalized\n");
	return 0;
}
