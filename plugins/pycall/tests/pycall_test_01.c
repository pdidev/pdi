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

#include <stdio.h>
#include <stdlib.h>

#include <pdi.h>

int main( int argc, char* argv[] )
{
	PC_tree_t conf = PC_parse_string(
		"data: {a: {type: array, subtype: int, size: 3}}\n"
		"plugins:\n"
		"  pycall:\n"
		"    on_event:\n"
		"      testing:\n"
		"        exec: \"print(' * [P] I received    $a =',a); a[1]=7; print(' * [P] changed it to $a =',a);\"\n"
		"        with: { a: $a }\n"
	);
	PDI_init(conf);
	
	int a[3] = {1, 2, 3};
	
	printf(" * [C] starting with $a = [%d %d %d]\n", a[0], a[1], a[2]);
	
	PDI_share("a", a, PDI_INOUT);
	PDI_event("testing");
	PDI_reclaim("a");
	
	printf(" * [C] now I see     $a = [%d %d %d]\n", a[0], a[1], a[2]);
	if ( a[0] != 1 || a[1] != 7 || a[2] != 3 ) {
		fprintf(stderr, "*** Error: expected [1, 7, 3]!\n");
		exit(1);
	}
	
	PDI_finalize();
	return EXIT_SUCCESS;
}
