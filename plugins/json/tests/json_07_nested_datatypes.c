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

const char* CONFIG_YAML
	= "pdi:                                                                 \n"
	  "  data:                                                              \n"
	  "    var_array_record_array_array:                                    \n"
	  "      type: array                                                    \n"
	  "      size: 2                                                        \n"
	  "      subtype:                                               		\n"
	  "        type: struct                                          		\n"
	  "        members:                                           			\n"
	  "          - var_int: int                                          	\n"
	  "          - var_double: double                                       \n"
	  "          - var_array_of_array:                                      \n"
	  "              type: array                                          	\n"
	  "              size: 3                                          		\n"
	  "              subtype:                                          		\n"
	  "                type: array                                          \n"
	  "                size: 4                                          	\n"
	  "                subtype: int                                         \n"
	  "    var_tuple_array_record:                                          \n"
	  "      type: tuple                                        			\n"
	  "      elements:                                          			\n"
	  "        - elem_int:                                          		\n"
	  "          type: int                                          		\n"
	  "        - elem_double:                                          		\n"
	  "          type: double                                          		\n"
	  "        - elem_array:                                          		\n"
	  "          type: array                                          		\n"
	  "          size: 2                                  					\n"
	  "          subtype:                                           		\n"
	  "            type: struct                                          	\n"
	  "            members:                                          		\n"
	  "              - var_int: int                                         \n"
	  "              - var_double: double                                   \n"
	  "  plugins:                                                           \n"
	  "    json:                                                            \n"
	  "      - file: json_07_nested_datatypes.json							\n"
	  "        write:                                           			\n"
	  "          - var_array_record_array_array                             \n"
	  "          - var_tuple_array_record                         			\n";

int main(void)
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	struct record_w_array_of_array {
		int _var_int;
		double _var_double;
		int _var_array[3][4];
	};

	struct record_w_array_of_array _var_raa_1 = {3, 33.3, {{311, 312, 313, 314}, {321, 322, 323, 324}, {331, 332, 333, 334}}};
	struct record_w_array_of_array _var_raa_2 = {4, 44.4, {{411, 412, 413, 414}, {421, 422, 423, 424}, {431, 432, 433, 434}}};

	struct record_w_array_of_array _var_mix[2] = {_var_raa_1, _var_raa_2};

	PDI_expose("var_array_record_array_array", _var_mix, PDI_OUT);

	struct sub_struct {
		int sub_int;
		double sub_double;
	};

	struct tuple_t {
		int elem_int;
		double elem_double;
		struct sub_struct elem_array[2];
	};
	struct tuple_t _var_tuple = {11, 12.3, {{22, 22.2}, {33, 33, 3}}};
	PDI_expose("var_tuple_array_record", &_var_tuple, PDI_OUT);

	PDI_finalize();
	return 0;
}
