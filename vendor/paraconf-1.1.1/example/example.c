/* Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
 *               root of the project or at https://github.com/pdidev/paraconf
 * 
 * SPDX-License-Identifier: MIT
 */

#include <assert.h>
#include <math.h>
#include <paraconf.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/* This file contains an example on how to use Paraconf 
 */


int main(int argc, char* argv[])
{
	// Creates a tree that contains all the parsed data of the parameter file
	if (argc != 2) {
		fprintf(stderr, "Error: expected 1 argument!\n");
		exit(1);
	}
	PC_tree_t conf = PC_parse_path(argv[1]);

	/*For the sake of the illustration a tree is 
	 *           .                // origin of the tree
	 *       /   |     \
	 * .a_int   .a_float  ...     // sub-trees/node/map
	 */

	// PC_get return the sub-tree under the node name ".a_int" (the dot is mandatory) if it is found.
	// and PC_int return the leaf assuming it's an int.
	long a_int;
	PC_int(PC_get(conf, ".a_int"), &a_int);
	/// Same for double
	double a_float;
	PC_double(PC_get(conf, ".a_float"), &a_float);
	/// Same for string
	char* a_string;
	PC_string(PC_get(conf, ".a_string"), &a_string);
	/// Same for bool
	int a_yes;
	PC_bool(PC_get(conf, ".a_yes"), &a_yes);
	printf("a_int=%ld a_float=%f a_string=%s a_yes=%s\n", a_int, a_float, a_string, a_yes ? "true" : "false");
	free(a_string);

	// The list and map number of elements are given by the PC_len function
	// Element of a list are accessed using the '[number]' syntax so [0] is the 1st element.
	printf("a_list=[ ");
	int a_list_len;
	PC_len(PC_get(conf, ".a_list"), &a_list_len);
	for (int ii = 0; ii < a_list_len; ++ii) {
		long a_list_ii;
		PC_int(PC_get(conf, ".a_list[%d]", ii), &a_list_ii);
		printf("%ld ", a_list_ii);
	}
	printf("]\n");

	// Sub-tree/node of a tree are accessed using the '{number}' or '<number>' is the 1st element.
	printf("a_map={   ");
	int a_map_len;
	PC_len(PC_get(conf, ".a_map"), &a_map_len);
	for (int ii = 0; ii < a_list_len; ++ii) {
		char* a_map_ii_k;
		PC_string(PC_get(conf, ".a_map{%d}", ii), &a_map_ii_k);
		long a_map_ii_v;
		PC_int(PC_get(conf, ".a_map<%d>", ii), &a_map_ii_v);
		printf("%s => %ld   ", a_map_ii_k, a_map_ii_v);
		free(a_map_ii_k);
	}
	printf("}\n");

	PC_errhandler_t errh = PC_errhandler(PC_NULL_HANDLER);
	PC_tree_t some_key = PC_get(conf, ".some_key");
	PC_errhandler(errh);

	printf("config %s `some_key'\n", PC_status(some_key) ? "does not contain" : "contains");

	PC_tree_destroy(&conf);

	return 0;
}
