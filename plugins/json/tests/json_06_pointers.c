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

int main()
{
	const char* CONFIG_YAML
		= "pdi:                                                        \n"
		  "  data:                                                     \n"
		  "    pvar_uint16_t: {type: pointer, subtype: uint16_t}       \n"
		  "    pvar_uint32_t: {type: pointer, subtype: uint32_t}       \n"
		  "    pvar_uint64_t: {type: pointer, subtype: uint64_t}       \n"
		  "    pvar_int8_t: {type: pointer, subtype: int8_t}           \n"
		  "    pvar_int16_t: {type: pointer, subtype: int16_t}         \n"
		  "    pvar_int32_t: {type: pointer, subtype: int32_t}         \n"
		  "    pvar_int64_t: {type: pointer, subtype: int64_t}         \n"
		  "    pvar_float: {type: pointer, subtype: float}             \n"
		  "    pvar_double: {type: pointer, subtype: double}           \n"
		  "    pvar_char: {type: pointer, subtype: char}               \n"
		  "                                                            \n"
		  "    pvar_string:                                            \n"
		  "      type: pointer                                         \n"
		  "      subtype: { type: array, subtype: char, size: 8 }      \n"
		  "    pvar_array:                                             \n"
		  "      type: pointer                                         \n"
		  "      subtype : { type: array, subtype: int32_t, size: 3 }  \n"
		  "    pvar_record:                                            \n"
		  "      type: pointer                                         \n"
		  "      subtype:                                              \n"
		  "        type: struct                                        \n"
		  "        members:                                            \n"
		  "            - var_int32_t1: int32_t                         \n"
		  "            - var_int32_t2: int32_t                         \n"
		  "            - var_char: char                                \n"
		  "            - var_double: double                            \n"
		  "                                                            \n"
		  "    var_int_pointer_array:                                  \n"
		  "      type: array                                           \n"
		  "      size: 2                                               \n"
		  "      subtype:                                              \n"
		  "        type: pointer                                       \n"
		  "        subtype: {type: array, size: 3, subtype: int32_t}   \n"
		  "                                                            \n"
		  "  plugins:                                                  \n"
		  "    json:                                                   \n"
		  "      - file: json_06_pointers.json                         \n"
		  "        write: [pvar_uint16_t, pvar_uint32_t, pvar_uint64_t, pvar_int8_t, pvar_int16_t, pvar_int32_t, pvar_int64_t, pvar_float, "
		  "pvar_double, pvar_char, pvar_array, pvar_string, pvar_record, var_int_pointer_array]\n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	// Scalars
	uint16_t _var_uint16_t = 2;
	uint32_t _var_uint32_t = 3;
	uint64_t _var_uint64_t = 4;

	int8_t _var_int8_t = 5;
	int16_t _var_int16_t = 6;
	int32_t _var_int32_t = 7;
	int64_t _var_int64_t = 8;
	float _var_float = 9;
	double _var_double = 10;

	char _var_char = 'a';

	uint16_t* _p_var_uint16_t = &_var_uint16_t;
	uint32_t* _p_var_uint32_t = &_var_uint32_t;
	uint64_t* _p_var_uint64_t = &_var_uint64_t;

	int8_t* _p_var_int8_t = &_var_int8_t;
	int16_t* _p_var_int16_t = &_var_int16_t;
	int32_t* _p_var_int32_t = &_var_int32_t;
	int64_t* _p_var_int64_t = &_var_int64_t;
	float* _p_var_float = &_var_float;
	double* _p_var_double = &_var_double;

	char* _p_var_char = &_var_char;

	PDI_expose("pvar_uint16_t", &_p_var_uint16_t, PDI_OUT);
	PDI_expose("pvar_uint32_t", &_p_var_uint32_t, PDI_OUT);
	PDI_expose("pvar_uint64_t", &_p_var_uint64_t, PDI_OUT);

	PDI_expose("pvar_int8_t", &_p_var_int8_t, PDI_OUT);
	PDI_expose("pvar_int16_t", &_p_var_int16_t, PDI_OUT);
	PDI_expose("pvar_int32_t", &_p_var_int32_t, PDI_OUT);
	PDI_expose("pvar_int64_t", &_p_var_int64_t, PDI_OUT);
	PDI_expose("pvar_float", &_p_var_float, PDI_OUT);
	PDI_expose("pvar_double", &_p_var_double, PDI_OUT);

	PDI_expose("pvar_char", &_p_var_char, PDI_OUT);

	/* STRING */
	char _var_string[] = "string12";
	char* _p_var_string = _var_string;
	PDI_expose("pvar_string", &_p_var_string, PDI_OUT);

	/* ARRAY */
	int32_t _var_array[3] = {11, 12, 13};
	int32_t* _p_var_array = _var_array;
	PDI_expose("pvar_array", &_p_var_array, PDI_OUT);

	/* RECORD */
	struct record_t {
		int32_t _var_int32t1;
		int32_t _var_int32t2;
		char _var_a; // padding
		double _var_double;
	};
	struct record_t _var_record1 = {14, 15, 'a', 16.};
	struct record_t* _p_var_record1 = &_var_record1;

	PDI_expose("pvar_record", &_p_var_record1, PDI_OUT);

	/* ARRAYS */
	int32_t _var_array1[3] = {1, 2, 3};
	int32_t _var_array2[3] = {4, 5, 6};
	int32_t* _var_int_pointer_array[2] = {_var_array1, _var_array2};
	PDI_expose("var_int_pointer_array", _var_int_pointer_array, PDI_OUT);

	PDI_finalize();
	return 0;
}
