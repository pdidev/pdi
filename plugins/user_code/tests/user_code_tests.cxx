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

void add_ten(void)
{
	int* buffer = NULL;
	PDI_access("test_input", (void**)&buffer, PDI_IN);
	(*buffer) += 10;
	PDI_release("test_input");
}

void update(void)
{
	int* buffer = NULL;
	PDI_access("input", (void**)&buffer, PDI_IN); // Read something from input
	PDI_release("input");
    
	PDI_access("output", (void**)&buffer, PDI_OUT);
	(*buffer) += 10;// *buffer = CST1; // Write something to output
	PDI_release("output");
}

}

class UserCode: public ::PDI::PdiTest
{};

TEST_F(UserCode, WhenCondition)
{
	InitPdi(PC_parse_string(R"==(
logging: trace                          
metadata:                               
  cond: int                             
data:                                   
  test_var: int                         
  input: int                            
  output: int                           
plugins:                                
  user_code:                            
    on_event:                           
      testing:                          
        when: $cond                     
        update: {}                        
    on_data:                            
      test_var:                         
        when: $cond=1                   
        add_ten: {test_input: $test_var}
)=="));

	int cond = 0;
	PDI_expose("cond", &cond, PDI_OUT);

	int test_var = 99;
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 99);

	int in = 0;
	int out = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, "output", &out, PDI_IN, NULL);
	EXPECT_EQ(out, 0);

	// Function will be called because cond = 1
	cond = 1;
	PDI_expose("cond", &cond, PDI_OUT);

	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_EQ(test_var, 109);

	in = 0;
	out = 0;
	PDI_multi_expose("testing", "input", &in, PDI_OUT, "output", &out, PDI_IN, NULL);
	EXPECT_EQ(out, 10);
}
