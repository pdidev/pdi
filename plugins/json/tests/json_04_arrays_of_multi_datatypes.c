/*******************************************************************************
 * Copyright (C) 2023-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <paraconf.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

int main()
{
	const char* CONFIG_YAML
		= "pdi:                                                                  \n"
		  "  data:                                                               \n"
		  "    var_string_array:                                                 \n"
		  "      type: array                                                     \n"
		  "      size: 2                                                         \n"
		  "      subtype:                                                        \n"
		  "        type: array                                                   \n"
		  "        size: 8                                                       \n"
		  "        subtype: char                                                 \n"
		  "                                                                      \n"
		  "    var_array_array:                                                  \n"
		  "      type: array                                                     \n"
		  "      size: 2                                                         \n"
		  "      subtype: {type: array, size: 3, subtype: int32_t}               \n"
		  "                                                                      \n"
		  "    var_records_array:                                                \n"
		  "      type: array                                                     \n"
		  "      size: 2                                                         \n"
		  "      subtype:                                                        \n"
		  "        type: struct                                                  \n"
		  "        members:                                                      \n"
		  "          - var_int32t1: int32_t                                      \n"
		  "          - var_int32t2: int32_t                                      \n"
		  "          - var_char: char                                            \n"
		  "          - var_double: double                                        \n"
		  "  plugins:                                                            \n"
		  "    json:                                                             \n"
		  "      - file: json_04_arrays_of_multi_datatypes.json                  \n"
		  "        write: [var_string_array, var_array_array, var_records_array] \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* STRINGS */
	char _var_string_array[3][8] = {"string12", "string13", "string14"};
	PDI_expose("var_string_array", &_var_string_array, PDI_OUT);

	/* ARRAYS */
	int32_t _var_array_array[2][3] = {{1, 2, 3}, {4, 5, 6}};
	PDI_expose("var_array_array", _var_array_array, PDI_OUT);

	/* RECORDS */
	struct record_t {
		int32_t _var_int32t1;
		int32_t _var_int32t2;
		char _var_char; // padding
		double _var_double;
	};
	struct record_t _var_record1 = {14, 15, 'a', 16.};
	struct record_t _var_record2 = {17, 18, 'b', 19.};

	struct record_t _var_records_array[2] = {_var_record1, _var_record2};
	PDI_expose("var_records_array", &_var_records_array, PDI_OUT);

	PDI_finalize();
	return 0;
}
