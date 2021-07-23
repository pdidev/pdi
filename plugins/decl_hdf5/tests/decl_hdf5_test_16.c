/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <hdf5.h>
#include <pdi.h>

int PDI_write_pure()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                          \n"
	    "data:                                                                   \n"
	    "  array_data: { size: 100, type: array, subtype: int}                   \n"
	    "  matrix_data: { size: [100, 100], type: array, subtype: float }        \n"
	    "plugins:                                                                \n"
	    "  decl_hdf5:                                                            \n"
	    "    file: decl_hdf5_test_16_pure.h5                                     \n"
	    "    deflate: -1                                                         \n"
	    "    write:                                                              \n"
	    "      - array_data                                                      \n"
	    "      - matrix_data                                                     \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 100*i + j * 0.95f;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read_pure()
{
	hid_t file_id = H5Fopen("decl_hdf5_test_16_pure.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		printf("Cannot open pure file to read\n");
		return 1;
	}
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = 0;
	}
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 0.0f;
		}
	}
	
	hid_t array_data_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (array_data_id < 0) {
		printf("Cannot open array dataset\n");
		return 1;
	}
	
	herr_t status = H5Dread(array_data_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, array_data);
	if (status < 0) {
		printf("Cannot read array dataset\n");
		return 1;
	}
	
	// check chunking
	hid_t array_plist = H5Dget_create_plist(array_data_id);
	hsize_t array_dims[2];
	int chunk_dim = H5Pget_chunk(array_plist, 2, array_dims);
	if (chunk_dim >= 0) {
		printf("Array dataset has chunking in pure file\n");
		return 1;
	}
	
	// check deflate and fletcher
	int filters_count = H5Pget_nfilters(array_plist);
	if (filters_count > 0) {
		printf("Array dataset has filters in pure file\n");
		return 1;
	}
	
	status = H5Dclose(array_data_id);
	if (status < 0) {
		printf("Cannot close array dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		if (array_data[i] != i) {
			printf("array_size[%d] invalid value: %d (should be: %d)\n", i, array_data[i], i);
			return 1;
		}
	}
	
	
	
	hid_t matrix_data_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	if (matrix_data_id < 0) {
		printf("Cannot open matrix dataset\n");
		return 1;
	}
	
	status = H5Dread(matrix_data_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix_data);
	if (status < 0) {
		printf("Cannot read matrix dataset\n");
		return 1;
	}
	
	// check chunking
	hid_t matrix_plist = H5Dget_create_plist(matrix_data_id);
	hsize_t matrix_dims[2];
	chunk_dim = H5Pget_chunk(matrix_plist, 2, matrix_dims);
	if (chunk_dim >= 0) {
		printf("Matrix dataset has chunking in pure file\n");
		return 1;
	}
	printf("Above errors are expected and correct\n");
	
	// check deflate and fletcher
	filters_count = H5Pget_nfilters(matrix_plist);
	if (filters_count > 0) {
		printf("Matrix dataset has filters in pure file\n");
		return 1;
	}
	
	status = H5Dclose(matrix_data_id);
	if (status < 0) {
		printf("Cannot close matrix dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			if (matrix_data[i][j] != i*100 + j*0.95f) {
				printf("matrix_size[%d, %d] invalid value: %f (should be: %f)\n", i, j, matrix_data[i][j], i*100 + j*0.95f);
				return 1;
			}
		}
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}

int PDI_write_chunked()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                                \n"
	    "data:                                                                         \n"
	    "  array_data: { size: 100, type: array, subtype: int, +decl_hdf5.chunking: 10}\n"
	    "  matrix_data: { size: [100, 100], type: array, subtype: float }              \n"
	    "plugins:                                                                      \n"
	    "  decl_hdf5:                                                                  \n"
	    "    file: decl_hdf5_test_16_chunked.h5                                        \n"
	    "    write:                                                                    \n"
	    "      array_data:                                                             \n"
	    "      matrix_data: {chunking: [10, 10]}                                       \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 100*i + j * 0.95f;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read_chunked()
{
	hid_t file_id = H5Fopen("decl_hdf5_test_16_chunked.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		printf("Cannot open chunked file to read\n");
		return 1;
	}
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = 0;
	}
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 0.0f;
		}
	}
	
	hid_t array_data_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (array_data_id < 0) {
		printf("Cannot open array dataset\n");
		return 1;
	}
	
	herr_t status = H5Dread(array_data_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, array_data);
	if (status < 0) {
		printf("Cannot read array dataset\n");
		return 1;
	}
	
	// check chunking
	hid_t array_plist = H5Dget_create_plist(array_data_id);
	hsize_t array_dims[1];
	int chunk_dim = H5Pget_chunk(array_plist, 1, array_dims);
	if (chunk_dim < 0) {
		printf("Array dataset is not chunked in chunked file\n");
		return 1;
	}
	if (array_dims[0] != 10) {
		printf("Array dataset invalid chunking: %d (should be: 10)\n", array_dims[0]);
		return 1;
	}
	
	// check deflate and fletcher
	int filters_count = H5Pget_nfilters(array_plist);
	if (filters_count > 0) {
		printf("Array dataset has filters in chunked file\n");
		return 1;
	}
	
	status = H5Dclose(array_data_id);
	if (status < 0) {
		printf("Cannot close array dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		if (array_data[i] != i) {
			printf("array_size[%d] invalid value: %d (should be: %d)\n", i, array_data[i], i);
			return 1;
		}
	}
	
	hid_t matrix_data_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	if (matrix_data_id < 0) {
		printf("Cannot open matrix dataset\n");
		return 1;
	}
	
	status = H5Dread(matrix_data_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix_data);
	if (status < 0) {
		printf("Cannot read matrix dataset\n");
		return 1;
	}
	
	// check chunking
	hid_t matrix_plist = H5Dget_create_plist(matrix_data_id);
	hsize_t matrix_dims[2];
	chunk_dim = H5Pget_chunk(matrix_plist, 2, matrix_dims);
	if (chunk_dim < 0) {
		printf("Matrix dataset is not chunked in chunked file\n");
		return 1;
	}
	if (matrix_dims[0] != 10 || matrix_dims[1] != 10) {
		printf("Matrix dataset invalid chunking: [%d, %d] (should be: [10, 10])\n", matrix_dims[0], matrix_dims[1]);
		return 1;
	}
	
	// check deflate and fletcher
	filters_count = H5Pget_nfilters(matrix_plist);
	if (filters_count > 0) {
		printf("Matrix dataset has filters in pure file\n");
		return 1;
	}
	
	status = H5Dclose(matrix_data_id);
	if (status < 0) {
		printf("Cannot close matrix dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			if (matrix_data[i][j] != i*100 + j*0.95f) {
				printf("matrix_size[%d, %d] invalid value: %f (should be: %f)\n", i, j, matrix_data[i][j], i*100 + j*0.95f);
				return 1;
			}
		}
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}

int PDI_write_deflated()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                                                 \n"
	    "data:                                                                                          \n"
	    "  array_data: { size: 100, type: array, subtype: int, +decl_hdf5.chunking: 10}                 \n"
	    "  matrix_data: { size: [100, 100], type: array, subtype: float, +decl_hdf5.chunking: [10, 10]} \n"
	    "plugins:                                                                                       \n"
	    "  decl_hdf5:                                                                                   \n"
	    "    file: decl_hdf5_test_16_deflated.h5                                                        \n"
	    "    deflate: 5                                                                                 \n"
	    "    write:                                                                                     \n"
	    "      - array_data                                                                             \n"
	    "      - matrix_data                                                                            \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 100*i + j * 0.95f;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read_deflated()
{
	hid_t file_id = H5Fopen("decl_hdf5_test_16_deflated.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		printf("Cannot open deflated file to read\n");
		return 1;
	}
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = 0;
	}
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 0.0f;
		}
	}
	
	hid_t array_data_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (array_data_id < 0) {
		printf("Cannot open array dataset\n");
		return 1;
	}
	
	herr_t status = H5Dread(array_data_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, array_data);
	if (status < 0) {
		printf("Cannot read array dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t array_plist = H5Dget_create_plist(array_data_id);
	int filters_count = H5Pget_nfilters(array_plist);
	if (filters_count != 1) {
		printf("Array dataset filters == %d (should be 1) in deflated file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(array_data_id);
	if (status < 0) {
		printf("Cannot close array dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		if (array_data[i] != i) {
			printf("array_size[%d] invalid value: %d (should be: %d)\n", i, array_data[i], i);
			return 1;
		}
	}
	
	hid_t matrix_data_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	if (matrix_data_id < 0) {
		printf("Cannot open matrix dataset\n");
		return 1;
	}
	
	status = H5Dread(matrix_data_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix_data);
	if (status < 0) {
		printf("Cannot read matrix dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t matrix_plist = H5Dget_create_plist(matrix_data_id);
	filters_count = H5Pget_nfilters(matrix_plist);
	if (filters_count != 1) {
		printf("Matrix dataset filters == %d (should be: 1) in deflated file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(matrix_data_id);
	if (status < 0) {
		printf("Cannot close matrix dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			if (matrix_data[i][j] != i*100 + j*0.95f) {
				printf("matrix_size[%d, %d] invalid value: %f (should be: %f)\n", i, j, matrix_data[i][j], i*100 + j*0.95f);
				return 1;
			}
		}
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}

int PDI_write_fletcher()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                                                 \n"
	    "data:                                                                                          \n"
	    "  array_data: { size: 100, type: array, subtype: int, +decl_hdf5.chunking: 10}                 \n"
	    "  matrix_data: { size: [100, 100], type: array, subtype: float, +decl_hdf5.chunking: [10, 10]} \n"
	    "plugins:                                                                                       \n"
	    "  decl_hdf5:                                                                                   \n"
	    "    file: decl_hdf5_test_16_fletcher.h5                                                        \n"
	    "    fletcher: 1                                                                                \n"
	    "    write:                                                                                     \n"
	    "      - array_data                                                                             \n"
	    "      - matrix_data                                                                            \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 100*i + j * 0.95f;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read_fletcher()
{
	hid_t file_id = H5Fopen("decl_hdf5_test_16_fletcher.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		printf("Cannot open fletcher file to read\n");
		return 1;
	}
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = 0;
	}
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 0.0f;
		}
	}
	
	hid_t array_data_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (array_data_id < 0) {
		printf("Cannot open array dataset\n");
		return 1;
	}
	
	herr_t status = H5Dread(array_data_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, array_data);
	if (status < 0) {
		printf("Cannot read array dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t array_plist = H5Dget_create_plist(array_data_id);
	int filters_count = H5Pget_nfilters(array_plist);
	if (filters_count != 1) {
		printf("Array dataset filters == %d (should be 1) in fletcher file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(array_data_id);
	if (status < 0) {
		printf("Cannot close array dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		if (array_data[i] != i) {
			printf("array_size[%d] invalid value: %d (should be: %d)\n", i, array_data[i], i);
			return 1;
		}
	}
	
	hid_t matrix_data_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	if (matrix_data_id < 0) {
		printf("Cannot open matrix dataset\n");
		return 1;
	}
	
	status = H5Dread(matrix_data_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix_data);
	if (status < 0) {
		printf("Cannot read matrix dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t matrix_plist = H5Dget_create_plist(matrix_data_id);
	filters_count = H5Pget_nfilters(matrix_plist);
	if (filters_count != 1) {
		printf("Matrix dataset filters == %d (should be: 1) in fletcher file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(matrix_data_id);
	if (status < 0) {
		printf("Cannot close matrix dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			if (matrix_data[i][j] != i*100 + j*0.95f) {
				printf("matrix_size[%d, %d] invalid value: %f (should be: %f)\n", i, j, matrix_data[i][j], i*100 + j*0.95f);
				return 1;
			}
		}
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}


int PDI_write_defletcher()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                                                 \n"
	    "data:                                                                                          \n"
	    "  array_data: { size: 100, type: array, subtype: int, +decl_hdf5.chunking: 10}                 \n"
	    "  matrix_data: { size: [100, 100], type: array, subtype: float, +decl_hdf5.chunking: [10, 10]} \n"
	    "plugins:                                                                                       \n"
	    "  decl_hdf5:                                                                                   \n"
	    "    file: decl_hdf5_test_16_defletcher.h5                                                      \n"
	    "    fletcher: 1                                                                                \n"
	    "    deflate: 7                                                                                 \n"
	    "    write:                                                                                     \n"
	    "      - array_data                                                                             \n"
	    "      - matrix_data                                                                            \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 100*i + j * 0.95f;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read_defletcher()
{
	hid_t file_id = H5Fopen("decl_hdf5_test_16_defletcher.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	if (file_id < 0) {
		printf("Cannot open defletcher file to read\n");
		return 1;
	}
	
	int array_data[100];
	for (int i = 0; i < 100; i++) {
		array_data[i] = 0;
	}
	
	float matrix_data[100][100];
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			matrix_data[i][j] = 0.0f;
		}
	}
	
	hid_t array_data_id = H5Dopen2(file_id, "array_data", H5P_DEFAULT);
	if (array_data_id < 0) {
		printf("Cannot open array dataset\n");
		return 1;
	}
	
	herr_t status = H5Dread(array_data_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, array_data);
	if (status < 0) {
		printf("Cannot read array dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t array_plist = H5Dget_create_plist(array_data_id);
	int filters_count = H5Pget_nfilters(array_plist);
	if (filters_count != 2) {
		printf("Array dataset filters == %d (should be 2) in defletcher file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(array_data_id);
	if (status < 0) {
		printf("Cannot close array dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		if (array_data[i] != i) {
			printf("array_size[%d] invalid value: %d (should be: %d)\n", i, array_data[i], i);
			return 1;
		}
	}
	
	hid_t matrix_data_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	if (matrix_data_id < 0) {
		printf("Cannot open matrix dataset\n");
		return 1;
	}
	
	status = H5Dread(matrix_data_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix_data);
	if (status < 0) {
		printf("Cannot read matrix dataset\n");
		return 1;
	}
	
	// check deflate
	hid_t matrix_plist = H5Dget_create_plist(matrix_data_id);
	filters_count = H5Pget_nfilters(matrix_plist);
	if (filters_count != 2) {
		printf("Matrix dataset filters == %d (should be: 2) in defletcher file\n", filters_count);
		return 1;
	}
	
	status = H5Dclose(matrix_data_id);
	if (status < 0) {
		printf("Cannot close matrix dataset\n");
		return 1;
	}
	
	for (int i = 0; i < 100; i++) {
		for (int j = 0; j < 100; j++) {
			if (matrix_data[i][j] != i*100 + j*0.95f) {
				printf("matrix_size[%d, %d] invalid value: %f (should be: %f)\n", i, j, matrix_data[i][j], i*100 + j*0.95f);
				return 1;
			}
		}
	}
	
	status = H5Fclose(file_id);
	if (file_id < 0) {
		return 1;
	}
	
	return 0;
}


int main()
{
	// pure
	int status = PDI_write_pure();
	if (status !=0) {
		return status;
	}
	status = PDI_read_pure();
	if (status !=0) {
		return status;
	}
	
	// chunked
	status = PDI_write_chunked();
	if (status !=0) {
		return status;
	}
	status = PDI_read_chunked();
	if (status !=0) {
		return status;
	}
	
	// deflated
	status = PDI_write_deflated();
	if (status !=0) {
		return status;
	}
	status = PDI_read_deflated();
	if (status !=0) {
		return status;
	}
	
	// fletcher
	status = PDI_write_fletcher();
	if (status !=0) {
		return status;
	}
	status = PDI_read_fletcher();
	if (status !=0) {
		return status;
	}
	
	// deflate and fletcher
	status = PDI_write_defletcher();
	if (status !=0) {
		return status;
	}
	status = PDI_read_defletcher();
	if (status !=0) {
		return status;
	}
	
	return 0;
}
