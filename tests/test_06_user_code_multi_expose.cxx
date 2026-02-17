/*******************************************************************************
 * Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gtest/gtest.h>
#include <pdi.h>

// Function to check the value
void check_value(const char* var_name, int& var, const int expected_value)
{
	EXPECT_EQ(var, expected_value) << "Wrong value of " << var_name << ": " << var << " != " << expected_value;
}

// Define user_code function
extern "C" {

// Test access of the first argument var1
void test_access_var1(void)
{
	int* value;
	PDI_access("value", (void**)&value, PDI_IN); // Read something from input
	PDI_release("value");
	check_value("var1", *value, -4);
}

// Test access of the last argument var2
void test_access_var2(void)
{
	int* value;
	PDI_access("value", (void**)&value, PDI_IN); // Read something from input
	PDI_release("value");
	check_value("var2", *value, 3);
}

} // end extern "C"

/*
 * Name:               user_code_multi_expose_test
 *
 * Description:        Verify that the value of data in a multi_expose are given to PDI
 *                     before the loop "on_data" event. 
 */


TEST(test_06_user_code_multi_expose_test, 01)
{
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

	PDI_init(PC_parse_string(CONFIG_YAML));

	int var1 = -4;
	int var2 = 3;

	PDI_multi_expose("my_test", "var1", &var1, PDI_OUT, "var2", &var2, PDI_OUT, NULL);

	PDI_finalize();
}
