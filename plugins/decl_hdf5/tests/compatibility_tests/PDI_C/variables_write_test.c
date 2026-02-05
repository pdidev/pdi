/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "variables_test.h5"

int main()
{
	printf("PDI variables_write_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                               \n"
		  "data:                                                        \n"
		  "  int_data: int                                              \n"
		  "  double_data: double                                        \n"
		  "  float_data: float                                          \n"
		  "  char_data: char                                            \n"
		  "plugins:                                                     \n"
		  "  decl_hdf5:                                                 \n"
		  "    file: variables_test.h5                                  \n"
		  "    write: [ int_data, double_data, float_data, char_data ]  \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int int_data = 15;
	double double_data = 55.26;
	float float_data = 3.5;
	char char_data = 'z';

	PDI_multi_expose(
		"testing",
		"int_data",
		&int_data,
		PDI_OUT,
		"double_data",
		&double_data,
		PDI_OUT,
		"float_data",
		&float_data,
		PDI_OUT,
		"char_data",
		&char_data,
		PDI_OUT,
		NULL
	);

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI variables_write_test finalized\n");
	return 0;
}
