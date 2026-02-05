/*
 * SPDX-FileCopyrightText: 2023-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <paraconf.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

const char* CONFIG_YAML
	= "pdi:                                                                     \n"
	  "  data:                                                                  \n"
	  "    var_uint8_t: {type: array, size: 3, subtype: uint8_t}                \n"
	  "    var_uint16_t: {type: array, size: 3, subtype: uint16_t}              \n"
	  "    var_uint32_t: {type: array, size: 3, subtype: uint32_t}              \n"
	  "    var_uint64_t: {type: array, size: 3, subtype: uint64_t}              \n"
	  "    var_int8_t: {type: array, size: 3, subtype: int8_t}                  \n"
	  "    var_int16_t: {type: array, size: 3, subtype: int16_t}                \n"
	  "    var_int32_t: {type: array, size: 3, subtype: int32_t}                \n"
	  "    var_int64_t: {type: array, size: 3, subtype: int64_t}                \n"
	  "    var_float: {type: array, size: 3, subtype: float}                    \n"
	  "    var_double: {type: array, size: 3, subtype: double}                  \n"
	  "    var_char: {type: array, size: 3, subtype: char}                      \n"
	  "                                                                         \n"
	  "  plugins:                                                               \n"
	  "    json:                                                                \n"
	  "      - file : json_03_arrays_of_datatypes.json                          \n"
	  "        write :                                                          \n"
	  "          - var_uint8_t                                                  \n"
	  "          - var_uint16_t                                                 \n"
	  "          - var_uint32_t                                                 \n"
	  "          - var_uint64_t                                                 \n"
	  "          - var_int8_t                                                   \n"
	  "          - var_int16_t                                                  \n"
	  "          - var_int32_t                                                  \n"
	  "          - var_int64_t                                                  \n"
	  "          - var_float                                                    \n"
	  "          - var_double                                                   \n"
	  "          - var_char                                                     \n";

int main(void)
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* SCALARS */
	uint16_t _var_array_uint16_t[3] = {2, 22, 2};
	PDI_expose("var_uint16_t", &_var_array_uint16_t, PDI_OUT);

	uint32_t _var_array_uint32_t[3] = {3, 33, 3};
	PDI_expose("var_uint32_t", &_var_array_uint32_t, PDI_OUT);

	uint64_t _var_array_uint64_t[3] = {4, 44, 4};
	PDI_expose("var_uint64_t", &_var_array_uint64_t, PDI_OUT);

	int8_t _var_array_int8_t[3] = {5, 55, 5};
	PDI_expose("var_int8_t", _var_array_int8_t, PDI_OUT);

	int16_t _var_array_int16_t[3] = {6, 66, 6};
	PDI_expose("var_int16_t", &_var_array_int16_t, PDI_OUT);

	int32_t _var_array_int32_t[3] = {7, 77, 7};
	PDI_expose("var_int32_t", &_var_array_int32_t, PDI_OUT);

	int64_t _var_array_int64_t[3] = {8, 88, 8};
	PDI_expose("var_int64_t", &_var_array_int64_t, PDI_OUT);

	float _var_array_float[3] = {9, 99, 9};
	PDI_expose("var_float", &_var_array_float, PDI_OUT);

	double _var_array_double[3] = {10, 1010, 10};
	PDI_expose("var_double", &_var_array_double, PDI_OUT);

	// Strings are also considered as list of chars
	char _var_array_char[3] = {'a', 'b', 'c'};
	PDI_expose("var_char", &_var_array_char, PDI_OUT);

	PDI_finalize();
	return 0;
}
