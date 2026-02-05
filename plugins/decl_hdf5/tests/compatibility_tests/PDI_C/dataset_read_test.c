/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "dataset_test.h5"

int main()
{
	printf("PDI dataset_read_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                                 \n"
		  "data:                                                          \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }     \n"
		  "plugins:                                                       \n"
		  "  decl_hdf5:                                                   \n"
		  "    file: dataset_test.h5                                      \n"
		  "    datasets:                                                  \n"
		  "      array_data: {type: array, subtype: int, size: [5, 10]}   \n"
		  "    read:                                                      \n"
		  "      array_data:                                              \n"
		  "        - memory_selection:                                    \n"
		  "            size: [5, 5]                                       \n"
		  "          dataset_selection:                                   \n"
		  "            size: [5, 5]                                       \n"
		  "            start: [0, 5]                                      \n"
		  "        - memory_selection:                                    \n"
		  "            size: [5, 5]                                       \n"
		  "            start: [0, 5]                                      \n"
		  "          dataset_selection:                                   \n"
		  "            size: [5, 5]                                       \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int test_array[5][10];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = 0;
		}
	}
	PDI_expose("array_data", test_array, PDI_IN);

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (test_array[i][j] != i * 10 + j) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array[i][j], i * 10 + j);
				return 1;
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI dataset_read_test finalized\n");
	return 0;
}
