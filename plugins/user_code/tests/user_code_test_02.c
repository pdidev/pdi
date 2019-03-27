/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <stdlib.h>
#include <pdi.h>

#define FATAL 1
#define WARN 0
#define CST0 -1
#define CST1 1
#define test_value( var, value, fatal) fct_test_value( var, value, fatal, __func__, __LINE__)

const char* CONFIG_YAML =
    "metadata:                                        \n"
    "data:                                            \n"
    "  test_var: double                               \n"
    "  input: int                                     \n"
    "  output: int                                    \n"
    "plugins:                                         \n"
    "  user_code:                                     \n"
    "    on_event:                                    \n"
    "      testing:                                   \n"
    "        test: {var_in: $input, var_out: $output }\n"
    ;

static void fct_test_value(int var, const int value, int fatal, const char* fct, int line)
{
	if (value != var) {
		fprintf(stdout, "Test in func %s line %3d, not working: value=%d, var=%d\n", fct, line, value, var);
		fflush(stdout);
		if (fatal) abort();
	} else {
		fprintf(stdout, "Test in func %s line %3d, working : value=var=%d\n", fct, line, value);
		fflush(stdout);
	}
	return;
}

void test(void)
{
	int* buffer=NULL;
	if ( PDI_access("var_in", (void**)&buffer, PDI_IN) ) {
		assert(0&&"Could not access var_in");
	}
	test_value(*buffer, CST0, FATAL);
	PDI_release("var_in");
	
	PDI_access("var_out", (void**)&buffer, PDI_OUT);
	*buffer = CST1;
	PDI_release("var_out");
}


int main( int argc, char* argv[] )
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	int in = CST0;
	int out = CST0;
	PDI_multi_expose("testing",
	    "input", &in, PDI_OUT, // export data as function input
	    "output", &out, PDI_IN, // import data as function output
	    NULL);
	test_value(out, CST1, FATAL);
	
	PDI_finalize();
}

