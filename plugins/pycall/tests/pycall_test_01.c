/*
 * SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <stdio.h>
#include <stdlib.h>

#include <pdi.h>

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_string(
		"logging: trace                                 \n"
		"data: {a: {type: array, subtype: int, size: 3}}\n"
		"plugins:\n"
		"  pycall:\n"
		"    on_event:\n"
		"      testing:\n"
		"        with: { a_python: $a }\n"
		"        exec: \"print(' * [P] I received    $a =',a_python, flush=True); a_python[1]=7; print(' * [P] changed it to $a =',a_python, "
		"flush=True);\"\n"
		"    on_data:\n"
		"      a: \"print(' * [P] I received    $a =',a, flush=True); a[1]=7; print(' * [P] changed it to $a =',a, flush=True);\"\n"
	);
	PDI_init(conf);

	int a[3] = {1, 2, 3};

	printf(" * [C] starting with $a = [%d %d %d]\n", a[0], a[1], a[2]);

	PDI_share("a", a, PDI_INOUT);
	PDI_event("testing");
	PDI_reclaim("a");

	printf(" * [C] now I see     $a = [%d %d %d]\n", a[0], a[1], a[2]);
	if (a[0] != 1 || a[1] != 7 || a[2] != 3) {
		fprintf(stderr, "*** Error: expected [1, 7, 3]!\n");
		exit(1);
	}

	PDI_finalize();
	return EXIT_SUCCESS;
}
