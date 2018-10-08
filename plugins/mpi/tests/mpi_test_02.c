/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <paraconf.h>
#include <pdi.h>

const char* YAML_CONFIG =
    "plugins:                  \n"
    "  mpi:                    \n"
    ;

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	PDI_init(conf);
	MPI_Comm* comm;
	
	PDI_access("MPI_COMM_WORLD", (void**)&comm, PDI_IN);
	assert(MPI_COMM_WORLD == *comm);
	PDI_release("MPI_COMM_WORLD");
	
	PDI_access("MPI_COMM_SELF", (void**)&comm, PDI_IN);
	assert(MPI_COMM_SELF == *comm);
	PDI_release("MPI_COMM_SELF");
	
	PDI_access("MPI_COMM_NULL", (void**)&comm, PDI_IN);
	assert(MPI_COMM_NULL == *comm);
	PDI_release("MPI_COMM_NULL");
	
	PDI_finalize();
	MPI_Finalize();
}
