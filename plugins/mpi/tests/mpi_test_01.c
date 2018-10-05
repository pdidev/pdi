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
    "metadata:                 \n"
    "  my_array:               \n"
    "    size: [3, 3]          \n"
    "    subsize: [2, 2]       \n"
    "    start: [1, 1]         \n"
    "    subtype: MPI_Comm\n"
    "    type: array           \n"
    "plugins:                  \n"
    "  mpi:                    \n"
    ;

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf, &world);
	
	MPI_Comm sparse_array[9]; // buffer: 3 x 3; data: 2 x 2; start: 1, 1
	MPI_Comm* dense_array;    // buffer: 2 x 2
	
	for (int i = 0; i < 9; i++) {
		MPI_Comm_dup(MPI_COMM_WORLD, sparse_array + i);
	}
	
	// metadata expose creates a dense copy inside PDI
	PDI_expose("my_array", sparse_array, PDI_OUT);
	for (int i = 0; i < 9; i++) {
		MPI_Comm_free(sparse_array + i);
	}
	
	PDI_access("my_array", (void**)&dense_array, PDI_IN);
	
	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	for (int i = 0; i < 4; i++) {
		int comm_rank;
		MPI_Comm_rank(dense_array[i], &comm_rank);
		assert(world_rank == comm_rank);
	}
	PDI_release("my_array");
	
	PDI_finalize();
	MPI_Finalize();
}
