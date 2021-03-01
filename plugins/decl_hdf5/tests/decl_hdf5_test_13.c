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

#include <pdi.h>

int PDI_write()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                          \n"
	    "data:                                                                   \n"
	    "  array_data: { size: 10, type: array, subtype: int }                   \n"
	    "  matrix_data: { size: [10, 10], type: array, subtype: float }          \n"
	    "plugins:                                                                \n"
	    "  decl_hdf5:                                                            \n"
	    "    file: decl_hdf5_test_13.h5                                          \n"
	    "    write: [array_data, matrix_data]                                    \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int array_data[10];
	for (int i = 0; i < 10; i++) {
		array_data[i] = i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);
	
	int matrix_data[10][10];
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			matrix_data[i][j] = 10*i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                          \n"
	    "metadata:                                                               \n"
	    "  input: int                                                            \n"
	    "data:                                                                   \n"
	    "  array_data_size: int64                                                \n"
	    "  matrix_data_size: { size: 2, type: array, subtype: int64 }            \n"
	    "plugins:                                                                \n"
	    "  decl_hdf5:                                                            \n"
	    "    - file: decl_hdf5_test_13.h5                                        \n"
	    "      when: $input                                                      \n"
	    "      read:                                                             \n"
	    "        array_data_size:                                                \n"
	    "          size_of: array_data                                           \n"
	    "        matrix_data_size:                                               \n"
	    "          size_of: matrix_data                                          \n"
	    "    - file: decl_hdf5_test_13.h5                                        \n"
	    "      on_event: \"read_size\"                                           \n"
	    "      read:                                                             \n"
	    "        array_data_size:                                                \n"
	    "          size_of: array_data                                           \n"
	    "        matrix_data_size:                                               \n"
	    "          size_of: matrix_data                                          \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int input = 1;
	PDI_expose("input", &input, PDI_OUT);
	
	long array_size = 0;
	long matrix_size[2] = {0, 0};
	PDI_expose("array_data_size", &array_size, PDI_IN);
	PDI_expose("matrix_data_size", matrix_size, PDI_IN);
	
	if (array_size != 10) {
		printf("array_size invalid value: %d (should be: 10)\n", array_size);
		return 1;
	}
	if (matrix_size[0] != 10 || matrix_size[1] != 10) {
		printf("matrix_size invalid value: [%d, %d] (should be: [10, 10])\n", matrix_size[0], matrix_size[1]);
		return 1;
	}
	
	// now with event
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	
	array_size = 0;
	matrix_size[0] = 0;
	matrix_size[1] = 0;
	PDI_multi_expose("read_size",
	    "array_data_size", &array_size, PDI_IN,
	    "matrix_data_size", matrix_size, PDI_IN,
	    NULL);
	    
	if (array_size != 10) {
		printf("array_size invalid value: %d (should be: 10)\n", array_size);
		return 1;
	}
	if (matrix_size[0] != 10 || matrix_size[1] != 10) {
		printf("matrix_size invalid value: [%d, %d] (should be: [10, 10])\n", matrix_size[0], matrix_size[1]);
		return 1;
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int main()
{
	int status = PDI_write();
	if (status !=0) {
		return status;
	}
	status = PDI_read();
	if (status !=0) {
		return status;
	}
	
	return 0;
}
