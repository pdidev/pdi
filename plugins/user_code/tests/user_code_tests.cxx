/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <pdi/testing.h>

extern "C" {

void onData_add_ten(void)
{
	int* buffer = NULL;
	PDI_access("var_inout", (void**)&buffer, PDI_IN);
	(*buffer) += 10;
	PDI_release("var_inout");
}

void onEvent_add_ten(void)
{
	int* buffer = NULL;
	PDI_access("var_in", (void**)&buffer, PDI_IN); // Read something from input
	PDI_release("var_in");
    
	PDI_access("var_out", (void**)&buffer, PDI_OUT);
	(*buffer) += 10; // Write something to output
	PDI_release("var_out");
}

void onEvent_add_one(void)
{
	int* buffer = NULL;
	PDI_access("var_in", (void**)&buffer, PDI_IN); // Read something from input
	PDI_release("var_in");
    
	PDI_access("var_out", (void**)&buffer, PDI_OUT);
	(*buffer) += 1; // Write something to output
	PDI_release("var_out");
}

void onEvent_add_two(void)
{
	int* buffer = NULL;
	PDI_access("var_in", (void**)&buffer, PDI_IN); // Read something from input
	PDI_release("var_in");
    
	PDI_access("var_out", (void**)&buffer, PDI_OUT);
	(*buffer) += 2; // Write something to output
	PDI_release("var_out");
}

}

class UserCode: public ::PDI::PdiTest
{};

TEST_F(UserCode, OnEventWhen)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  cond: int
data:
  input: int
  output: int
plugins:
  user_code:
    on_event:
      testing:
        when: $cond
        onEvent_add_ten: {var_in: $input, var_out: $output }
)=="));

	int cond = 0;
	PDI_expose("cond", &cond, PDI_OUT);

	int in = 0;
	int out = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, "output", &out, PDI_IN, NULL);
	EXPECT_EQ(in, 0);
    EXPECT_EQ(out, 0);

	// Function will be called because cond = 1
	cond = 1;
	PDI_expose("cond", &cond, PDI_OUT);

	in = 0;
	out = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, "output", &out, PDI_IN, NULL);
	EXPECT_EQ(in, 0);
    EXPECT_EQ(out, 10);
}

TEST_F(UserCode, OnDataWhen)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  cond: int
data:
  test_var: int
plugins:
  user_code:
    on_data:
      test_var:
        when: $cond=1
        onData_add_ten: {var_inout: $test_var}
)=="));

	int cond = 0;
	PDI_expose("cond", &cond, PDI_OUT);

	int test_var = 99;
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 99);

	// Function will be called because cond = 1
	cond = 1;
	PDI_expose("cond", &cond, PDI_OUT);

	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 109);
}

TEST_F(UserCode, OnEventListWhen)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  cond: int
data:
  input: int
  outputA: int
  outputB: int
  outputC: int
plugins:
  user_code:
    on_event:
      - testing:
          when: $cond
          onEvent_add_one: {var_in: $input, var_out: $outputA }
          onEvent_add_two: {var_in: $input, var_out: $outputB }
      - testing:
          onEvent_add_ten: {var_in: $input, var_out: $outputC }
)=="));

	int cond = 0;
	PDI_expose("cond", &cond, PDI_OUT);

	int in = 0;
	int outA = 0;
	int outB = 0;
	int outC = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, 
		                        "outputA", &outA, PDI_IN, 
								"outputB", &outB, PDI_IN, 
								"outputC", &outC, PDI_IN, 
								NULL);
	EXPECT_EQ(in, 0);
    EXPECT_EQ(outA, 0);
	EXPECT_EQ(outB, 0);
	EXPECT_EQ(outC, 10);

	// Function will be called because cond = 1
	cond = 1;
	PDI_expose("cond", &cond, PDI_OUT);

	in = 0;
	outA = 0;
	outB = 0;
	outC = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, 
		                        "outputA", &outA, PDI_IN, 
								"outputB", &outB, PDI_IN, 
								"outputC", &outC, PDI_IN, 
								NULL);
	EXPECT_EQ(in, 0);
    EXPECT_EQ(outA, 1);
	EXPECT_EQ(outB, 2);
	EXPECT_EQ(outC, 10);
}

TEST_F(UserCode, OnDataListWhen)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  cond: int
data:
  test_var: int
plugins:
  user_code:
    on_data:
      - test_var:
          when: $cond=1
          onData_add_ten: {var_inout: $test_var}
      - test_var:
          onData_add_ten: {var_inout: $test_var}
)=="));

	int cond = 0;
	PDI_expose("cond", &cond, PDI_OUT);

	int test_var = 99;
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 109);

	// Function will be called because cond = 1
	cond = 1;
	PDI_expose("cond", &cond, PDI_OUT);

	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 129);
}

