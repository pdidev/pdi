/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <mpi.h>

#include <assert.h>
#include <stdlib.h>

#include <pdi.h>

const char* CONFIG_YAML =
    "metadata:                                        \n"
    "data:                                            \n"
    "  test_var: double                               \n"
    "  input: int                                     \n"
    "  output: int                                    \n"
    "plugins:                                         \n"
    "  user_code:                                     \n"
    "    on_event:                                    \n"
    "      testing:                                   \n"
    "        test: {var_in: $input, var_out: $output }\n"
    ;

void test(void)
{
	void* buffer;
	if ( !PDI_access("var_in", &buffer, PDI_IN) ) {
		PDI_release("var_in");
	}
	
	if ( !PDI_access("var_out", &buffer, PDI_OUT) ) {
		PDI_release("var_out");
	}
}

void succeed_on_failure(PDI_status_t status, const char* message, void* ctx)
{
	if (status) {
		fprintf(stderr, "PDI error reported: `%s'. This is a success!\n", message);
		*((int*)ctx) = 1; // has_failed = 1
	}
}

int main( int argc, char* argv[] )
{
	int has_failed = 0;
	PDI_errhandler_t local_errhandler;
	local_errhandler.func = succeed_on_failure;
	local_errhandler.context = &has_failed;
	
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);
	
	PDI_errhandler_t std_handler = PDI_errhandler(local_errhandler); //changing err handler
	
	int in = 0;
	int out = 0;
	PDI_multi_expose("testing",
	    "input", &in, PDI_IN,
	    "output", &out, PDI_IN,
	    NULL);
	    
	PDI_errhandler(std_handler); // returning to standard PDI err_handler
	if ( !has_failed ) {
		fprintf(stderr, "Error expected but not reported, terminating\n");
		abort();
		
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}

