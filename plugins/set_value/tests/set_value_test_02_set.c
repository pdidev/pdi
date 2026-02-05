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
	char value_char;
	PDI_expose("value_char", &value_char, PDI_IN);
	assert(value_char == 42);

	char char_array[3];
	PDI_expose("char_array", char_array, PDI_IN);
	for (int i = 0; i < 3; i++) {
		assert(char_array[i] == i);
	}

	// short
	short value_short;
	PDI_expose("value_short", &value_short, PDI_IN);
	assert(value_short == 4242);

	short short_array[3];
	PDI_expose("short_array", short_array, PDI_IN);
	for (int i = 3; i < 6; i++) {
		assert(short_array[i - 3] == i);
	}

	// int
	int value_int;
	PDI_expose("value_int", &value_int, PDI_IN);
	assert(value_int == 424242);

	int int_array[3];
	PDI_expose("int_array", int_array, PDI_IN);
	for (int i = 6; i < 9; i++) {
		assert(int_array[i - 6] == i);
	}

	//long
	long value_long;
	PDI_expose("value_long", &value_long, PDI_IN);
	assert(value_long == 424242424242);

	long long_array[3];
	PDI_expose("long_array", long_array, PDI_IN);
	for (int i = 9; i < 12; i++) {
		assert(long_array[i - 9] == i);
	}

	//float
	float value_float;
	PDI_expose("value_float", &value_float, PDI_IN);
	assert(value_float == 3.14159265f);

	float float_array[3];
	PDI_expose("float_array", float_array, PDI_IN);
	assert(float_array[0] == 1.23456789f);
	assert(float_array[1] == 12.3456789f);
	assert(float_array[2] == 123.456789f);

	//double
	double value_double;
	PDI_expose("value_double", &value_double, PDI_IN);
	assert(value_double == 3.14159265);

	double double_array[3];
	PDI_expose("double_array", double_array, PDI_IN);
	assert(double_array[0] == 1.23456789);
	assert(double_array[1] == 12.3456789);
	assert(double_array[2] == 123.456789);

	PDI_finalize();
}
