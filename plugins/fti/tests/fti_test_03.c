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

#include <assert.h>
#include <fti.h>
#include <mpi.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm my_comm;
	MPI_Comm_dup(MPI_COMM_WORLD, &my_comm);
	
	PDI_init(conf);
	
	PDI_expose("my_comm", &my_comm, PDI_OUT);
	
	int status;
	PDI_expose("fti_status", &status, PDI_IN);
	assert(status == 0);
	
	MPI_Comm* fti_comm;
	PDI_access("FTI_COMM_WORLD", (void**)&fti_comm, PDI_IN);
	assert(*fti_comm == FTI_COMM_WORLD);
	PDI_release("FTI_COMM_WORLD");
	
	PDI_finalize();
	
	MPI_Finalize();
	PC_tree_destroy(&conf);
	
	return 0;
}
