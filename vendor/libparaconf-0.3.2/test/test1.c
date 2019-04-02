#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <yaml.h>
#include <paraconf.h>

int main(int argc, char *argv[])
{

	PC_tree_t conf = PC_parse_path("test1.yml");
	
	long a_int; PC_int(PC_get(conf, ".a_int"), &a_int);
	assert(a_int==100);
	double a_float; PC_double(PC_get(conf, ".a_float"), &a_float);
	assert(fabs(a_float-100.1)<1e-10);
	char* a_string; PC_string(PC_get(conf, ".a_string"), &a_string);
	assert(!strcmp("this is a string", a_string));
	free(a_string);
	int a_list_len; PC_len(PC_get(conf, ".a_list"), &a_list_len);
	assert(a_list_len==2);
	long a_list_0; PC_int(PC_get(conf, ".a_list[0]"), &a_list_0);
	assert(a_list_0==10);
	int a_map_len; PC_len(PC_get(conf, ".a_map"), &a_map_len);
	assert(a_map_len==2);
	char *a_map_0_k; PC_string(PC_get(conf, ".a_map{0}"), &a_map_0_k);
	assert(!strcmp("first", a_map_0_k));
	free(a_map_0_k);
	long a_map_0_v; PC_int(PC_get(conf, ".a_map<0>"), &a_map_0_v);
	assert(a_map_0_v == 20);
	long another_list_1; PC_int(PC_get(conf, ".another_list[1]"), &another_list_1);
	assert(another_list_1==31);
	long another_map_second; PC_int(PC_get(conf, ".another_map.second"), &another_map_second);
	assert(another_map_second==41);

	int a_true; PC_bool(PC_get(conf, ".a_true"), &a_true);
	assert(a_true == 1);
	int a_True; PC_bool(PC_get(conf, ".a_True"), &a_True);
	assert(a_True == 1);
	int a_TRUE; PC_bool(PC_get(conf, ".a_TRUE"), &a_TRUE);
	assert(a_TRUE == 1);
	int a_yes; PC_bool(PC_get(conf, ".a_yes"), &a_yes);
	assert(a_yes == 1);
	int a_Yes; PC_bool(PC_get(conf, ".a_Yes"), &a_Yes);
	assert(a_Yes == 1);
	int a_YES; PC_bool(PC_get(conf, ".a_YES"), &a_YES);
	assert(a_YES == 1);
	int a_false; PC_bool(PC_get(conf, ".a_false"), &a_false);
	assert(a_false == 0);
	int a_False; PC_bool(PC_get(conf, ".a_False"), &a_False);
	assert(a_False == 0);
	int a_FALSE; PC_bool(PC_get(conf, ".a_FALSE"), &a_FALSE);
	assert(a_FALSE == 0);
	int a_no; PC_bool(PC_get(conf, ".a_no"), &a_no);
	assert(a_no == 0);
	int a_No; PC_bool(PC_get(conf, ".a_No"), &a_No);
	assert(a_No == 0);
	int a_NO; PC_bool(PC_get(conf, ".a_NO"), &a_NO);
	assert(a_NO == 0);
	
	return 0;
}
