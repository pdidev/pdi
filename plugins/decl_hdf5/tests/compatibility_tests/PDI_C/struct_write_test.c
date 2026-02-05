/*
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "struct_test.h5"

typedef struct test_struct {
	char a;
	int b[5][10];
} test_struct;

int main()
{
	printf("PDI struct_write_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                 \n"
		  "data:                                          \n"
		  "  test_struct:                                 \n"
		  "    type: struct                               \n"
		  "    members:                                   \n"
		  "      - a_name: char                           \n"
		  "      - b_name:                                \n"
		  "          type: array                          \n"
		  "          size: [5, 10]                        \n"
		  "          subtype: int                         \n"
		  "plugins:                                       \n"
		  "  decl_hdf5:                                   \n"
		  "  - file: struct_test.h5                       \n"
		  "    write: [test_struct]                       \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	test_struct record_data;

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			record_data.b[i][j] = i * 10 + j;
		}
	}
	record_data.a = 'a';

	PDI_expose("test_struct", &record_data, PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI struct_write_test finalized\n");
	return 0;
}
