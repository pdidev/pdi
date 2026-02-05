/*
 * SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <stdlib.h>
#include <pdi.h>

#define FATAL 1
#define WARN 0
#define CST0 -1
#define CST1 1
#define test_value(var, value, fatal) fct_test_value(var, value, fatal, __func__, __LINE__)

const char* CONFIG_YAML
	= "logging: trace    \n"
	  "metadata:         \n"
	  "data:             \n"
	  "  test_var: double\n"
	  "  input: int      \n"
	  "  output: int     \n"
	  "plugins:          \n"
	  "  user_code:      \n"
	  "    on_event:     \n"
	  "      testing:    \n"
	  "        test: {}  \n";

static void fct_test_value(int var, const int value, int fatal, const char* fct, int line)
{
	if (value != var) {
		fprintf(stdout, "Test in func %s line %3d, not working: value=%d, var=%d \n", fct, line, value, var);
		fflush(stdout);
		if (fatal) abort();
	} else {
		fprintf(stdout, "Test in func %s line %3d, working : value =%d = var \n", fct, line, value);
		fflush(stdout);
	}
	return;
}

void test(void)
{
	int* buffer = NULL;
	PDI_access("input", (void**)&buffer, PDI_IN); // Read something from input
	test_value(*buffer, CST0, FATAL);
	PDI_release("input");

	PDI_access("output", (void**)&buffer, PDI_OUT);
	*buffer = CST1; // Write something to output
	PDI_release("output");
}

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int in = CST0;
	int out = CST0;
	PDI_multi_expose(
		"testing",
		"input",
		&in,
		PDI_OUT, // export function input
		"output",
		&out,
		PDI_IN, // import function output
		NULL
	);
	test_value(out, CST1, FATAL);

	PDI_finalize();
}
