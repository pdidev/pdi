/*******************************************************************************
 * Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

int main(void)
{
	const char* CONFIG_YAML
	    = "pdi:                                                                  \n"
	        "  data:                                                               \n"
	        "    var_record_w_array:                                               \n"
	        "      type: array                                                     \n"
	        "      size: 2                                                         \n"
	        "      subtype:                                                        \n"
	        "        type: struct                                                  \n"
	        "        members:                                                      \n"
	        "          - var_int32t1: int32_t                                      \n"
	        "          - var_int32t2: int32_t                                      \n"
	        "          - var_char: char                                            \n"
	        "          - var_double: double                                        \n"
	        "          - var_string: {type: array, size: 10, subtype: char}        \n"
	        "          - var_array_double: {type: array, size: 3, subtype: double} \n"
	        "                                                                      \n"
	        "    var_mix:                                                          \n"
	        "      type: array                                                     \n"
	        "      size: 2                                                         \n"
	        "      subtype:                                                        \n"
	        "        type: struct                                                  \n"
	        "        members:                                                      \n"
	        "          - var_int32_t1: int32_t                                     \n"
	        "          - var_char1: char                                           \n"
	        "          - var_double1: double                                       \n"
	        "          - var_array1:                                               \n"
	        "              type: array                                             \n"
	        "              size: 3                                                 \n"
	        "              subtype:                                                \n"
	        "                type: struct                                          \n"
	        "                members:                                              \n"
	        "                  - var_double2: double                               \n"
	        "                  - var_array2:                                       \n"
	        "                      type: array                                     \n"
	        "                      size: 3                                         \n"
	        "                      subtype: double                                 \n"
	        "  plugins:                                                            \n"
	        "    json:                                                             \n"
	        "      var_mix: json_05_mixing_datatypes.json                         \n";
	        
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));
	
	/* RECORDS */
	struct record_w_array_t {
		int32_t _var_int32t1;
		int32_t _var_int32t2;
		char _var_char; // padding
		double _var_double;
		char _var_string[10];
		double _var_array_double[3];
	};
	struct record_w_array_t _var_record_w_array1 = {17, 18, 'b', 19., "abcdefghij", {1, 2, 3}};
	struct record_w_array_t _var_record_w_array2 = {20, 21, 'c', 22., "klmnopqrst", {1, 2, 3}};
	PDI_expose("var_record_w_array", &_var_record_w_array1, PDI_OUT);
	
	struct record_w_array_t _var_record_w_array_array[2] = {_var_record_w_array1, _var_record_w_array2};
	PDI_expose("var_record_w_array_array", _var_record_w_array_array, PDI_OUT);
	
	/* MIXING ARRAYS, RECORDS ETC */
	
	struct rec2_t {
		double _var_double2;
		double _var_array2[3];
	};
	
	struct rec1_t {
		int32_t var_int32_t1;
		char var_char1;
		double var_double1;
		struct rec2_t var_array1[3];
	};
	
	struct rec2_t _var_rec2_1 = {1, {5., 6., 7.}};
	struct rec2_t _var_rec2_2 = {2, {8., 9., 10.}};
	struct rec2_t _var_rec2_3 = {3, {11., 12., 13.}};
	
	struct rec1_t _var_rec1_1 = {100, 'a', 102., {_var_rec2_1, _var_rec2_2, _var_rec2_3}};
	struct rec1_t _var_rec1_2 = {200, 'b', 202., {_var_rec2_3, _var_rec2_2, _var_rec2_1}};
	
	struct rec1_t _var_mix[2] = {_var_rec1_1, _var_rec1_2};
	
	PDI_expose("var_mix", _var_mix, PDI_OUT);
	
	PDI_finalize();
	return 0;
}
