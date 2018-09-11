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
#include <stdio.h>

#include <paraconf.h>

#include <pdi.h>

const char* CONFIG_YAML =
"plugins:                                                                            \n"
"  user_code:                                                                        \n"
"    on_event:                                                                       \n"
"      test:                                                                         \n"
"        test_null: {test_in: $main_out, test_out: $main_in, test_inout: $main_inout}\n";

void test_null()
{
	int data = 0;
	void* data_in = &data;
	void* data_out = &data;
	void* data_inout = &data;
	PDI_access("test_in", &data_in, PDI_IN);
	PDI_access("test_out", &data_out, PDI_OUT);
	PDI_access("test_inout", &data_inout, PDI_INOUT);
	
	if ( data_in ) fprintf(stderr, "data_in = %p\n", data_in);
	assert(!data_in && "data_in should be null");
	if ( data_out ) fprintf(stderr, "data_out = %p\n", data_out);
	assert(!data_out && "data_out should be null");
	if ( data_inout ) fprintf(stderr, "data_inout = %p\n", data_inout);
	assert(!data_inout && "data_inout should be null");
	
	PDI_release("test_inout");
	PDI_release("test_out");
	PDI_release("test_in");
}

int main( int argc, char* argv[] )
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);
	
	void* data = NULL;
	
	PDI_multi_expose("test",
			"main_in", data, PDI_IN,
			"main_out", data, PDI_OUT,
			"main_inout", data, PDI_INOUT,
			NULL);
	    
	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
