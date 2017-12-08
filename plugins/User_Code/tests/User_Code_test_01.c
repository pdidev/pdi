/*******************************************************************************
 * Copyright (c) 2015, Corentin Roussel - CEA (corentin.roussel@cea.fr)
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

#include <assert.h>
#include <stdlib.h>
// #include <unistd.h>
#include <mpi.h>
#include <pdi.h>

#define err_code -1
#define test_value( value, fatal) fct_test_value( value, fatal, __func__, __LINE__)

static void fct_test_value(int value, int fatal, const char *fct, int line){
	if(value == err_code) {
		fprintf(stdout, "Test in func %s line %3d, not working: value=%d \n", fct, line, value);
		fflush(stdout);
		if (fatal) exit(1);
	} else { 
		fprintf(stdout, "Test in func %s line %3d, working : value =%d \n", fct, line, value);
		fflush(stdout);
	}
	return;
}

void test(void){
	int *buffer=NULL;

	PDI_access("input", (void**)&buffer, PDI_IN);
	test_value(*buffer, 1);
	PDI_release("input");

	PDI_access("output", (void**)&buffer, PDI_OUT);
	*buffer=2;
	PDI_release("output");
}


int main( int argc, char *argv[] )
{
	int i,j;
	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);

	i=0;
	j=err_code;
	
	PDI_transaction_begin("testing");
	PDI_expose("input",&i,PDI_OUT);
	PDI_expose("output",&j,PDI_IN);
	PDI_transaction_end();
	
	test_value(j,1);

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
	return 0;
}
