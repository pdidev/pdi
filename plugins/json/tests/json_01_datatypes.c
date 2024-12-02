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
	= "pdi:                                                                    \n"
	  "  data:                                                                 \n"
	  "    var_uint8_t: uint8_t                                                \n"
	  "    var_uint16_t: uint16_t                                              \n"
	  "    var_uint32_t: uint32_t                                              \n"
	  "    var_uint64_t: uint64_t                                              \n"
	  "    var_int8_t: int8_t                                                  \n"
	  "    var_int16_t: int16_t                                                \n"
	  "    var_int32_t: int32_t                                                \n"
	  "    var_int64_t: int64_t                                                \n"
	  "    var_float: float                                                    \n"
	  "    var_double: double                                                  \n"
	  "    var_char: char                                                      \n"
	  "    var_tuple:                                                          \n"
	  "      type: tuple                                                       \n"
	  "      elements:                          						       \n"
	  "        - elem_int:                          						   \n"
	  "          type: int                          					       \n"
	  "        - elem_double:                         					       \n"
	  "          type: double                          					       \n"
	  "                                                                        \n"
	  "  plugins:                                                              \n"
	  "    json:                                                               \n"
	  "      - file : json_01_datatypes.json                                   \n"
	  "        write :                                                         \n"
	  "          - var_uint8_t                                                 \n"
	  "          - var_uint16_t                                                \n"
	  "          - var_uint32_t                                                \n"
	  "          - var_uint64_t                                                \n"
	  "          - var_int8_t                                                  \n"
	  "          - var_int16_t                                                 \n"
	  "          - var_int32_t                                                 \n"
	  "          - var_int64_t                                                 \n"
	  "          - var_float                                                   \n"
	  "          - var_double                                                  \n"
	  "          - var_char                                                    \n"
	  "          - var_tuple                                                   \n";

int main(void)
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	// Scalars
	// uint8_t  _var_uint8_t  = 1;
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

	struct tuple_t {
		int elem_int;
		double elem_double;
	};
	struct tuple_t _var_tuple = {11, 12.3};

	PDI_expose("var_uint16_t", &_var_uint16_t, PDI_OUT);
	PDI_expose("var_uint32_t", &_var_uint32_t, PDI_OUT);
	PDI_expose("var_uint64_t", &_var_uint64_t, PDI_OUT);

	PDI_expose("var_int8_t", &_var_int8_t, PDI_OUT);
	PDI_expose("var_int16_t", &_var_int16_t, PDI_OUT);
	PDI_expose("var_int32_t", &_var_int32_t, PDI_OUT);
	PDI_expose("var_int64_t", &_var_int64_t, PDI_OUT);
	PDI_expose("var_float", &_var_float, PDI_OUT);
	PDI_expose("var_double", &_var_double, PDI_OUT);

	PDI_expose("var_char", &_var_char, PDI_OUT);

	PDI_expose("var_tuple", &_var_tuple, PDI_OUT);

	PDI_finalize();
	return 0;
}
