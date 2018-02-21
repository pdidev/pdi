/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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
  
//The following is used for doxygen documentation:
 /**
 * \file test_plugin.c
 * \brief a pdi project file
 * \author J. Bigot (CEA)
 */

#include <mpi.h>
#include <iostream>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/data_reference.h>

namespace {

using namespace PDI;
using std::cout;

MPI_Comm my_comm;

void PDI_test_plugin_init(Context&, PC_tree_t, MPI_Comm *world)
{
	if (MPI_Comm_dup(*world, &my_comm)) return;
	
	int rank; if (MPI_Comm_rank(my_comm, &rank)) return;
	
	if ( rank == 0 ) {
		printf("Welcome to the test plugin!\n");
	}
}

void PDI_test_plugin_finalize(Context&)
{
	int rank; if (MPI_Comm_rank(my_comm, &rank)) return;
	
	if ( rank == 0 ) {
		printf("Goodbye from the test plugin!\n");
	}
	
	if (MPI_Comm_free(&my_comm)) return;
}

void PDI_test_plugin_event(Context&, const char *event)
{
	int rank; if (MPI_Comm_rank(my_comm, &rank)) return;
	
	if ( rank == 0 ) {
		printf("test plugin got an event: %s!\n", event);
	}
}

void PDI_test_plugin_data(Context&, const char* name, PDI::Data_ref)
{
	int rank; if (MPI_Comm_rank(my_comm, &rank)) return;
	
	if ( rank == 0 ) {
		cout << " =>> data becoming available to the test plugin: "<<name<<"!\n";
	}
}

}

PDI_PLUGIN(test_plugin)
