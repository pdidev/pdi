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
	printf("PDI struct_read_test started\n");
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
		  "    read: [test_struct]                        \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	test_struct read_struct;
	read_struct.a = 0;
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			read_struct.b[i][j] = 0;
		}
	}
	PDI_expose("test_struct", &read_struct, PDI_IN);

	if (read_struct.a != 'a') {
		fprintf(stderr, "%c != 'a' \n", read_struct.a);
	}

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 10; j++) {
			if (read_struct.b[i][j] != i * 10 + j) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, read_struct.b[i][j], i * 10 + j);
				for (int k = 0; k < 5; k++) {
					for (int l = 0; l < 10; l++) {
						fprintf(stderr, "%d ", read_struct.b[k][l]);
					}
					fprintf(stderr, "\n");
				}
				return 1;
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI struct_read_test finalized\n");
	return 0;
}
