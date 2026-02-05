/*
 * SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <stdio.h>

#include <paraconf.h>

#include <pdi.h>

const char* CONFIG_YAML
	= "logging: trace                                                                      \n"
	  "plugins:                                                                            \n"
	  "  user_code:                                                                        \n"
	  "    on_event:                                                                       \n"
	  "      test:                                                                         \n"
	  "        test_null: {test_in: $main_out, test_out: $main_in, test_inout: $main_inout}\n";

void test_null()
{
	int data = 0;
	void* data_in = &data;
	void* data_out = &data;
	void* data_inout = &data;
	PDI_access("test_in", &data_in, PDI_IN);
	PDI_access("test_out", &data_out, PDI_OUT);
	PDI_access("test_inout", &data_inout, PDI_INOUT);

	if (data_in) fprintf(stderr, "data_in = %p\n", data_in);
	assert(!data_in && "data_in should be null");
	if (data_out) fprintf(stderr, "data_out = %p\n", data_out);
	assert(!data_out && "data_out should be null");
	if (data_inout) fprintf(stderr, "data_inout = %p\n", data_inout);
	assert(!data_inout && "data_inout should be null");

	PDI_release("test_inout");
	PDI_release("test_out");
	PDI_release("test_in");
}

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	void* data = NULL;

	PDI_multi_expose("test", "main_in", data, PDI_IN, "main_out", data, PDI_OUT, "main_inout", data, PDI_INOUT, NULL);

	PDI_finalize();
}
