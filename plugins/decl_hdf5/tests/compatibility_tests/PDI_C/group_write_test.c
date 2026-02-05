/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "group_test.h5"

int main()
{
	printf("PDI group_write_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                                       \n"
		  "data:                                                                \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }           \n"
		  "  array_data_second: { size: [5, 10], type: array, subtype: int }    \n"
		  "plugins:                                                             \n"
		  "  decl_hdf5:                                                         \n"
		  "    file: group_test.h5                                              \n"
		  "    on_event: write_event                                            \n"
		  "    datasets:                                                        \n"
		  "      group/array_data:                                              \n"
		  "        size: [5, 10]                                                \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "      group_second/array_data:                                       \n"
		  "        size: [5, 10]                                                \n"
		  "        type: array                                                  \n"
		  "        subtype: int                                                 \n"
		  "    write:                                                           \n"
		  "      array_data:                                                    \n"
		  "        - dataset: group/array_data                                  \n"
		  "      array_data_second:                                             \n"
		  "        - dataset: group_second/array_data                           \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int test_array[5][10];
	int test_array_second[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
			test_array_second[i][j] = (i * 10 + j) * 10;
		}
	}

	PDI_multi_expose("write_event", "array_data", test_array, PDI_OUT, "array_data_second", test_array_second, PDI_OUT, NULL);

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI group_write_test finalized\n");
	return 0;
}
