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

int main()
{
	const char* CONFIG_YAML =
	    "logging: trace                                                 \n"
	    "data:                                                          \n"
	    "  scalar_data: int                                             \n"
	    "  array_data: { size: [4, 4], type: array, subtype: int }      \n"
	    "plugins:                                                       \n"
	    "  decl_hdf5:                                                   \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      on_event: init                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      collision_policy: skip                                   \n"
	    "      on_event: skip                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      collision_policy: write_into                             \n"
	    "      on_event: write_into                                     \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [scalar_data, array_data]                         \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      collision_policy: replace                                \n"
	    "      on_event: replace                                        \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      collision_policy: write_into                             \n"
	    "      on_event: append                                         \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "      write:                                                   \n"
	    "        scalar_data:                                           \n"
	    "          collision_policy: error                              \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      collision_policy: error                                  \n"
	    "      on_event: error                                          \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      write: [array_data]                                      \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      on_event: read                                           \n"
	    "      datasets:                                                \n"
	    "        array_data: {type: array, subtype: int, size: [4, 4]}  \n"
	    "      read: [array_data]                                       \n"
	    "    - file: decl_hdf5_test_15.h5                               \n"
	    "      on_event: read_scalar                                    \n"
	    "      datasets:                                                \n"
	    "        scalar_data: int                                       \n"
	    "      read: [scalar_data]                                      \n"
	    ;
	    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	// INIT
	int array_data[4][4];
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 0;
		}
	}
	
	PDI_multi_expose("init", "array_data", array_data, PDI_OUT, NULL);
	
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 1;
		}
	}
	
	// SKIP
	PDI_multi_expose("skip", "array_data", array_data, PDI_OUT, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 0) {
				printf("array_data[%d][%d] = %d, should be: 0", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				return -1;
			}
		}
	}
	
	// WRITE_INTO
	int scalar_data = 42;
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 2;
		}
	}
	PDI_multi_expose("write_into",
	    "scalar_data", &scalar_data, PDI_OUT,
	    "array_data", array_data, PDI_OUT,
	    NULL);
	scalar_data = -1;
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}
	PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (scalar_data != 42) {
		printf("scalar_data = %d, should be: 42", scalar_data);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return -1;
	}
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 2) {
				printf("array_data[%d][%d] = %d, should be: 2", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				return -1;
			}
		}
	}
	
	// REPLACE
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 3;
		}
	}
	PDI_multi_expose("replace", "array_data", array_data, PDI_OUT, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = -1;
		}
	}
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 3) {
				printf("array_data[%d][%d] = %d, should be: 3", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				return -1;
			}
		}
	}
	PDI_errhandler(PDI_NULL_HANDLER);
	PDI_status_t status = PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (status == PDI_OK) {
		printf("replace: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return -1;
	}
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
	// APPEND
	scalar_data = 42;
	PDI_multi_expose("append", "scalar_data", &scalar_data, PDI_OUT, NULL);
	scalar_data = -1;
	PDI_multi_expose("read_scalar", "scalar_data", &scalar_data, PDI_IN, NULL);
	if (scalar_data != 42) {
		printf("scalar_data = %d, should be: 42", scalar_data);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return -1;
	}
	PDI_errhandler(PDI_NULL_HANDLER);
	status = PDI_multi_expose("append", "scalar_data", &scalar_data, PDI_OUT, NULL);
	if (status == PDI_OK) {
		printf("append: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return -1;
	}
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
	// ERROR
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			array_data[i][j] = 4;
		}
	}
	PDI_errhandler(PDI_NULL_HANDLER);
	status = PDI_multi_expose("error", "array_data", array_data, PDI_OUT, NULL);
	if (status == PDI_OK) {
		printf("error: status = %d, should not be: 0 (PDI_OK)", status);
		PDI_finalize();
		PC_tree_destroy(&conf);
		return -1;
	}
	
	PDI_multi_expose("read", "array_data", array_data, PDI_IN, NULL);
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			if (array_data[i][j] != 3) {
				printf("array_data[%d][%d] = %d, should be: 4", i, j, array_data[i][j]);
				PDI_finalize();
				PC_tree_destroy(&conf);
				return -1;
			}
		}
	}
	PDI_errhandler(PDI_ASSERT_HANDLER);
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
