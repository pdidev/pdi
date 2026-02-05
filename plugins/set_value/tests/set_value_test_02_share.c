/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>

#include <paraconf.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

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
	assert(*value_float == 3.141592f);
	PDI_release("value_float");

	float* float_array;
	PDI_access("float_array", (void**)&float_array, PDI_IN);
	assert(float_array[0] == 1.234567f);
	assert(float_array[1] == 12.34567f);
	assert(float_array[2] == 123.4567f);
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

	PDI_finalize();
}
