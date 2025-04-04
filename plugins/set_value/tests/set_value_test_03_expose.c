/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <math.h>

#include <paraconf.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));
	PDI_event("init");

	// char
	char* value_char;
	PDI_access("value_char", (void**)&value_char, PDI_IN);
	assert(*value_char == 42);
	PDI_release("value_char");

	char* char_array;
	PDI_access("char_array", (void**)&char_array, PDI_IN);
	for (int i = 0; i < 3; i++) {
		assert(char_array[i] == i);
	}
	PDI_release("char_array");

	// short
	short* value_short;
	PDI_access("value_short", (void**)&value_short, PDI_IN);
	assert(*value_short == 4242);
	PDI_release("value_short");

	short* short_array;
	PDI_access("short_array", (void**)&short_array, PDI_IN);
	for (int i = 3; i < 6; i++) {
		assert(short_array[i - 3] == i);
	}
	PDI_release("short_array");

	// int
	int* value_int;
	PDI_access("value_int", (void**)&value_int, PDI_IN);
	assert(*value_int == 424242);
	PDI_release("value_int");

	int* int_array;
	PDI_access("int_array", (void**)&int_array, PDI_IN);
	for (int i = 6; i < 9; i++) {
		assert(int_array[i - 6] == i);
	}
	PDI_release("int_array");

	//long
	long* value_long;
	PDI_access("value_long", (void**)&value_long, PDI_IN);
	assert(*value_long == 424242424242);
	PDI_release("value_long");

	long* long_array;
	PDI_access("long_array", (void**)&long_array, PDI_IN);
	for (int i = 9; i < 12; i++) {
		assert(long_array[i - 9] == i);
	}
	PDI_release("long_array");

	//float
	float* value_float;
	PDI_access("value_float", (void**)&value_float, PDI_IN);
	assert(*value_float == 3.14159265f);
	PDI_release("value_float");

	float* float_array;
	PDI_access("float_array", (void**)&float_array, PDI_IN);
	assert(float_array[0] == 1.23456789f);
	assert(float_array[1] == 12.3456789f);
	assert(float_array[2] == 123.456789f);
	PDI_release("float_array");

	//double
	double* value_double;
	PDI_access("value_double", (void**)&value_double, PDI_IN);
	assert(*value_double == 3.14159265);
	PDI_release("value_double");

	double* double_array;
	PDI_access("double_array", (void**)&double_array, PDI_IN);
	assert(double_array[0] == 1.23456789);
	assert(double_array[1] == 12.3456789);
	assert(double_array[2] == 123.456789);
	PDI_release("double_array");

	char* string;
	PDI_access("string", (void**)&string, PDI_IN);
	assert(string[0] == 'a');
	assert(string[1] == 'a');
	assert(string[2] == 'a');
	assert(string[3] == 'a');
	PDI_release("string");


	PDI_event("increment");

	// char
	PDI_access("value_char", (void**)&value_char, PDI_IN);
	assert(*value_char == 43);
	PDI_release("value_char");

	PDI_access("char_array", (void**)&char_array, PDI_IN);
	for (int i = 0; i < 3; i++) {
		assert(char_array[i] == i + 1);
	}
	PDI_release("char_array");

	// short
	PDI_access("value_short", (void**)&value_short, PDI_IN);
	assert(*value_short == 4243);
	PDI_release("value_short");

	PDI_access("short_array", (void**)&short_array, PDI_IN);
	for (int i = 3; i < 6; i++) {
		assert(short_array[i - 3] == i + 1);
	}
	PDI_release("short_array");

	// int
	PDI_access("value_int", (void**)&value_int, PDI_IN);
	assert(*value_int == 424243);
	PDI_release("value_int");

	PDI_access("int_array", (void**)&int_array, PDI_IN);
	for (int i = 6; i < 9; i++) {
		assert(int_array[i - 6] == i + 1);
	}
	PDI_release("int_array");

	//long
	PDI_access("value_long", (void**)&value_long, PDI_IN);
	assert(*value_long == 424242424243);
	PDI_release("value_long");

	PDI_access("long_array", (void**)&long_array, PDI_IN);
	for (int i = 9; i < 12; i++) {
		assert(long_array[i - 9] == i + 1);
	}
	PDI_release("long_array");

	//float
	PDI_access("value_float", (void**)&value_float, PDI_IN);
	assert(fabsf(*value_float - 4.14159265f) < 0.000001f);
	PDI_release("value_float");

	PDI_access("float_array", (void**)&float_array, PDI_IN);
	assert(fabsf(float_array[0] - 2.23456789f) < 0.000001f);
	assert(fabsf(float_array[1] - 13.3456789f) < 0.000001f);
	assert(fabsf(float_array[2] - 124.456789f) < 0.000001f);
	PDI_release("float_array");

	//double
	PDI_access("value_double", (void**)&value_double, PDI_IN);
	assert(fabs(*value_double - 4.14159265) < 0.000001);
	PDI_release("value_double");

	PDI_access("double_array", (void**)&double_array, PDI_IN);
	assert(fabs(double_array[0] - 2.23456789) < 0.000001);
	assert(fabs(double_array[1] - 13.3456789) < 0.000001);
	assert(fabs(double_array[2] - 124.456789) < 0.000001);
	PDI_release("double_array");

	PDI_access("string", (void**)&string, PDI_IN);
	assert(string[0] == 'a');
	assert(string[1] == 'b');
	assert(string[2] == 'c');
	assert(string[3] == 'd');
	PDI_release("string");

	PDI_finalize();
}
