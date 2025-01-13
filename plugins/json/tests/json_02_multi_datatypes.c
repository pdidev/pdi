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
	  "    var_array: { type: array, subtype: int32_t, size: 3 }            \n"
	  "    var_string: { type: array, subtype: char, size: 8 }              \n"
	  "    var_record:                                                      \n"
	  "        type: struct                                                 \n"
	  "        members:                                                     \n"
	  "            - var_int32_t1: int32_t                                  \n"
	  "            - var_int32_t2: int32_t                                  \n"
	  "            - var_char: char                                         \n"
	  "            - var_double: double                                     \n"
	  "            - var_array_dup: { type: array, subtype: int, size: 3 }  \n"
	  "                                                                     \n"
	  "  plugins:                                                           \n"
	  "    json:                                                            \n"
	  "      - file : json_02_multi_datatypes.json                          \n"
	  "        write : [var_string, var_array, var_record]                  \n";

int main()
{
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* STRING */
	char _var_string[] = "string12";
	PDI_expose("var_string", &_var_string, PDI_OUT);

	/* ARRAY */
	int32_t _var_array[3] = {11, 12, 13};
	PDI_expose("var_array", _var_array, PDI_OUT);

	/* RECORD */
	struct record_t {
		int32_t _var_int32t1;
		int32_t _var_int32t2;
		char _var_a; // padding
		double _var_double;
		int _var_array_dup[3];
	};
	struct record_t _var_record1 = {14, 15, 'a', 16., {17, 18, 19}};

	PDI_expose("var_record", &_var_record1, PDI_OUT);

	PDI_finalize();
	return 0;
}
