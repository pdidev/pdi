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

#define FILE "hdf5_comp_test_03.h5"

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

void HDF5_write()
{
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
}

void PDI_read()
{
	const char* CONFIG_YAML =
	    "logging: trace                                               \n"
	    "data:                                                        \n"
	    "  int_data: int                                              \n"
	    "  double_data: double                                        \n"
	    "  float_data: float                                          \n"
	    "  char_data: char                                            \n"
	    "plugins:                                                     \n"
	    "  decl_hdf5:                                                 \n"
	    "    file: hdf5_comp_test_03.h5                               \n"
	    "    read: [ int_data, double_data, float_data, char_data ]   \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int test_int = 0;
	double test_double = 0.0;
	float test_float = 0.0f;
	char test_char = 0;
	
	PDI_expose("int_data", &test_int, PDI_IN);
	PDI_expose("double_data", &test_double, PDI_IN);
	PDI_expose("float_data", &test_float, PDI_IN);
	PDI_expose("char_data", &test_char, PDI_IN);
	
	if (test_int != 15) {
		fprintf(stderr, "%d != %d\n ", test_int, 15);
		exit(1);
	}
	if (test_double != 55.26) {
		fprintf(stderr, "%f != %f\n ", test_double, 55.26);
		exit(1);
	}
	if (test_float != 3.5) {
		fprintf(stderr, "%f != %f\n ", test_float, 3.5);
		exit(1);
	}
	if (test_char != 'z') {
		fprintf(stderr, "%c != %c\n ", test_char, 'z');
		exit(1);
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
}

int main()
{
	HDF5_write();
	PDI_read();
	
	return 0;
}
