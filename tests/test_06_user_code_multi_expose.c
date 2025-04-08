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
#define CST1 -24
#define CST2 3

#define test_value(var, value, fatal) fct_test_value(var, value, fatal, __func__, __LINE__)

const char* CONFIG_YAML
	= "logging: trace                          \n"
	  "data:                                   \n"
	  "  var1: int                             \n"
	  "  var2: int                             \n"
	  "plugins:                                \n"
	  "  user_code:                            \n"
	  "    on_data:                            \n"
	  "      var2:                             \n"
	  "        test_access_var1: { value: $var1 }\n"
	  "      var1:                             \n"
	  "        test_access_var2: { value: $var2 }\n";


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

// Test access of the first argument var1
void test_access_var1(void)
{
	int* value;//=NULL;
	PDI_access("value", (void**) &value, PDI_IN); // Read something from input
	PDI_release("value");
	test_value(*value, CST1, FATAL);
}

// Test access of the last argument var2
void test_access_var2(void)
{
	int* value;//NULL;
	PDI_access("value", (void**) &value, PDI_IN); // Read something from input
	PDI_release("value");
	test_value(*value, CST2, FATAL);
}

/// @brief Verify that the value of data in a multiexpose are given to PDI 
/// before the loop "on_data" event.
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(CONFIG_YAML));

	int var1 = (int) CST1;
	int var2 = (int) CST2;
	
	//
	PDI_multi_expose("my_test",
		"var1", &var1, PDI_OUT,
		"var2", &var2, PDI_OUT,
		NULL
	);

	PDI_finalize();
	return 0;
}
