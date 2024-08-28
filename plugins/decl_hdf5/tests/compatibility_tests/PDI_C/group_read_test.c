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
#include <unistd.h>
#include <pdi.h>

#define FILE "group_test.h5"

int main()
{
	printf("PDI group_read_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "  array_data_second: { size: [5, 10], type: array, subtype: int }    \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: group_test.h5                                              \n"
		  "    on_event: read_event                                             \n"
		  "    datasets:                                                        \n"
		  "      group/array_data:                                              \n"
		  "        size: [5, 10]                                                \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group_second/array_data:                                       \n"
		  "        size: [5, 10]                                                \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    read:                                                            \n"
		  "      array_data:                                                    \n"
		  "        - dataset: group/array_data                                  \n"
		  "      array_data_second:                                             \n"
		  "        - dataset: group_second/array_data                           \n";


	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int test_array[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = 0;
		}
	}
	int test_array_second[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array_second[i][j] = 0;
		}
	}

	PDI_multi_expose("read_event", "array_data", test_array, PDI_IN, "array_data_second", test_array_second, PDI_IN, NULL);

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (test_array[i][j] != i * 10 + j) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array[i][j], i * 10 + j);
				return 1;
			} else if (test_array_second[i][j] != (i * 10 + j) * 10) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array_second[i][j], i * 10 + j);
				return 1;
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI group_read_test finalized\n");
	return 0;
}
