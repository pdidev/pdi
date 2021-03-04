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

#include <assert.h>
#include <pdi.h>
#include <unistd.h>

int PDI_write()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                 \n"
	    "data:                                                          \n"
	    "  scalar_data: double                                          \n"
	    "  array_data: { size: [8, 8], type: array, subtype: int }      \n"
	    "plugins:                                                       \n"
	    "  decl_hdf5:                                                   \n"
	    "    file: decl_hdf5_test_11.h5                                 \n"
	    "    datasets:                                                  \n"
	    "      scalar_data: {type: array, subtype: double, size: 1}     \n"
	    "      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
	    "    write: [scalar_data, array_data]                           \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	double scalar_data = 1.2345;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);
	
	int array_data[8][8];
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			array_data[i][j] = i * 8 + j;
		}
	}
	
	PDI_expose("array_data", array_data, PDI_OUT);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}

int PDI_read()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                 \n"
	    "data:                                                          \n"
	    "  scalar_data: double                                          \n"
	    "  array_data: { size: [8, 8], type: array, subtype: int }      \n"
	    "plugins:                                                       \n"
	    "  decl_hdf5:                                                   \n"
	    "    file: decl_hdf5_test_11.h5                                 \n"
	    "    datasets:                                                  \n"
	    "      scalar_data: {type: array, subtype: double, size: 1}     \n"
	    "      array_data: {type: array, subtype: int, size: [4, 4, 4]} \n"
	    "    read: [scalar_data, array_data]                            \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	double scalar_data = 0;
	PDI_expose("scalar_data", &scalar_data, PDI_IN);
	if (scalar_data != 1.2345) {
		printf("Wrong value of scalar_data: %f != 1.12345\n", scalar_data);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return 1;
	}
	
	int array_data[8][8];
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			array_data[i][j] = 0;
		}
	}
	
	PDI_expose("array_data", array_data, PDI_IN);
	
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 8; j++) {
			if (array_data[i][j] != i * 8 + j) {
				printf("Wrong value of array_data[%d][%d]: %d != %d\n", i, j, array_data[i][j], i * 8 + j);
				PDI_finalize();
				PC_tree_destroy(&conf);
				return 1;
			}
		}
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
