/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "array_test.h5"

int main()
{
	printf("PDI array_write_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                               \n"
		  "data:                                                        \n"
		  "  array_data: { size: [5, 10], type: array, subtype: int }   \n"
		  "plugins:                                                     \n"
		  "  decl_hdf5:                                                 \n"
		  "    file: array_test.h5                                      \n"
		  "    write: [ array_data ]                                    \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	int test_array[5][10];

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			test_array[i][j] = i * 10 + j;
		}
	}

	PDI_expose("array_data", test_array, PDI_OUT);
	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI array_write_test finalized\n");
	return 0;
}
