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

#define FATAL 1
#define WARN 0
#define CST0 -1
#define CST1 1
#define test_value( var, value, fatal) fct_test_value( var, value, fatal, __func__, __LINE__)

static void fct_test_value(int var, const int value, int fatal, const char* fct, int line)
{
	if (value != var) {
		fprintf(stdout, "Test in func %s line %3d, not working: value=%d, var=%d \n", fct, line, value, var);
		fflush(stdout);
		if (fatal) abort();
	} else {
		fprintf(stdout, "Test in func %s line %3d, working : value =%d = var \n", fct, line, value);
		fflush(stdout);
	}
	return;
}

void test(void)
{
	int* buffer=NULL;
	PDI_access("var_in", (void**)&buffer, PDI_IN);
	test_value(*buffer, CST0, FATAL);
	PDI_release("var_in");
	
	buffer = NULL;
	PDI_access("var_out", (void**)&buffer, PDI_OUT);
	*buffer=CST1;
	PDI_release("var_out");
}

void succeed_on_failure(PDI_status_t status, const char* message, void* ctx)
{
	(void) ctx;
	fprintf(stderr, "%s ", message);
	if (status) {
		fprintf(stderr, "PDI is aborting. This is a success!\n");
		exit(0);
	}
	return;
}

int main( int argc, char* argv[] )
{
	PDI_errhandler_t local_errhandler;
	local_errhandler.func = &succeed_on_failure;
	local_errhandler.context = "context";
	int in,out;
	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");
	
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	
	PDI_init(conf, &world);
	PDI_errhandler_t std_handler = PDI_errhandler(local_errhandler); //changing err handler
	
	in=CST0;
	out=CST0;
	PDI_transaction_begin("testing");
	PDI_expose("input", &in, PDI_IN);
	PDI_expose("output", &out, PDI_IN);
	PDI_transaction_end();
	test_value(in, CST1, FATAL);
	
	PDI_errhandler(std_handler); // returning to standard PDI err_handler
	PDI_finalize();
	
	PC_tree_destroy(&conf);
	MPI_Finalize();
	abort();
}

