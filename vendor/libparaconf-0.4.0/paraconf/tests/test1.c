/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <paraconf.h>

#define TST_EXPECT(VALID) tst_expect_msg(VALID, #VALID)

void tst_expect_msg(int valid, const char* message)
{
	if ( valid ) {
		fprintf(stderr, "As expected: %s!\n", message);
	} else {
		fprintf(stderr, "Error, expected: %s!\n", message);
		exit(1);
	}
}

int main(int argc, char *argv[])
{
	if (argc != 2 ) {
		fprintf(stderr, "Error: expected 1 argument!\n");
		exit(1);
	}
	PC_tree_t conf = PC_parse_path(argv[1]);
	
	long a_int; PC_int(PC_get(conf, ".a_int"), &a_int);
	TST_EXPECT( a_int == 100 );
	double a_float; PC_double(PC_get(conf, ".a_float"), &a_float);
	TST_EXPECT(fabs(a_float-100.1)<1e-10);
	char* a_string; PC_string(PC_get(conf, ".a_string"), &a_string);
	TST_EXPECT(!strcmp("this is a string", a_string));
	free(a_string);
	int a_list_len; PC_len(PC_get(conf, ".a_list"), &a_list_len);
	TST_EXPECT(a_list_len==2);
	long a_list_0; PC_int(PC_get(conf, ".a_list[0]"), &a_list_0);
	TST_EXPECT(a_list_0==10);
	int a_map_len; PC_len(PC_get(conf, ".a_map"), &a_map_len);
	TST_EXPECT(a_map_len==2);
	char *a_map_0_k; PC_string(PC_get(conf, ".a_map{0}"), &a_map_0_k);
	TST_EXPECT(!strcmp("first", a_map_0_k));
	free(a_map_0_k);
	long a_map_0_v; PC_int(PC_get(conf, ".a_map<0>"), &a_map_0_v);
	TST_EXPECT(a_map_0_v == 20);
	long another_list_1; PC_int(PC_get(conf, ".another_list[1]"), &another_list_1);
	TST_EXPECT(another_list_1==31);
	long another_map_second; PC_int(PC_get(conf, ".another_map.second"), &another_map_second);
	TST_EXPECT(another_map_second==41);

	int a_true; PC_bool(PC_get(conf, ".a_true"), &a_true);
	TST_EXPECT(a_true == 1);
	int a_True; PC_bool(PC_get(conf, ".a_True"), &a_True);
	TST_EXPECT(a_True == 1);
	int a_TRUE; PC_bool(PC_get(conf, ".a_TRUE"), &a_TRUE);
	TST_EXPECT(a_TRUE == 1);
	int a_yes; PC_bool(PC_get(conf, ".a_yes"), &a_yes);
	TST_EXPECT(a_yes == 1);
	int a_Yes; PC_bool(PC_get(conf, ".a_Yes"), &a_Yes);
	TST_EXPECT(a_Yes == 1);
	int a_YES; PC_bool(PC_get(conf, ".a_YES"), &a_YES);
	TST_EXPECT(a_YES == 1);
	int a_false; PC_bool(PC_get(conf, ".a_false"), &a_false);
	TST_EXPECT(a_false == 0);
	int a_False; PC_bool(PC_get(conf, ".a_False"), &a_False);
	TST_EXPECT(a_False == 0);
	int a_FALSE; PC_bool(PC_get(conf, ".a_FALSE"), &a_FALSE);
	TST_EXPECT(a_FALSE == 0);
	int a_no; PC_bool(PC_get(conf, ".a_no"), &a_no);
	TST_EXPECT(a_no == 0);
	int a_No; PC_bool(PC_get(conf, ".a_No"), &a_No);
	TST_EXPECT(a_No == 0);
	int a_NO; PC_bool(PC_get(conf, ".a_NO"), &a_NO);
	TST_EXPECT(a_NO == 0);
	
	return 0;
}
