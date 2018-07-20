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

#include <pdi.h>
#include <mpi.h>
#include <assert.h>

void verify_matrix(int comm_color)
{
	int matrix[4];
	PDI_expose("matrix", matrix, PDI_IN);
	if (comm_color) {
		for (int i = 0; i < 4; i++) {
			printf("m[%d] = %d\n", i, matrix[i]);
			assert(matrix[i] == i);
		}
	} else {
		for (int i = 0; i < 4; i++) {
			printf("m[%d] = %d\n", i, matrix[i]);
			assert(matrix[i] == (i+4));
		}
	}
}

int main(int argc, char* argv[])
{
	assert(argc == 2 && "Needs 1 single arg: config file");
	
	MPI_Init(&argc,&argv);
	MPI_Comm main_comm = MPI_COMM_WORLD;
	
	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(conf, &main_comm);
	
	int world_size;
	MPI_Comm_size(MPI_COMM_WORLD, &world_size);
	assert(world_size == 8);
	
	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	
	//split the communicator
	int comm_color = (world_rank < 4);
	
	MPI_Comm my_comm;
	MPI_Comm_split(MPI_COMM_WORLD, comm_color, 0, &my_comm);
	PDI_expose("my_comm", &my_comm, PDI_OUT);
	
	int my_comm_rank;
	MPI_Comm_rank(my_comm, &my_comm_rank);
	int pcoord[2] = {my_comm_rank/2, my_comm_rank%2};
	PDI_expose("pcoord", pcoord, PDI_OUT);
	
	// first half of processes writes
	if (comm_color) {
		int matrix = world_rank;
		printf("Exposing: %d\n", world_rank);
		PDI_expose("matrix", &matrix, PDI_OUT);
	}
	
	// verify file
	if (world_rank == 0) {
		MPI_Comm self = MPI_COMM_SELF;
		PDI_expose("my_comm", &self, PDI_OUT);
		verify_matrix(comm_color);
	}
	// exposing MPI_Comm must be collective
	PDI_expose("my_comm", &my_comm, PDI_OUT);
	
	// for better console output
	MPI_Barrier(MPI_COMM_WORLD);
	
	// second half of processes writes
	if (!comm_color) {
		int matrix = world_rank;
		printf("Exposing: %d\n", world_rank);
		PDI_expose("matrix", &matrix, PDI_OUT);
	}
	
	// wait for second half of processes
	MPI_Barrier(MPI_COMM_WORLD);
	
	// verify file
	if (world_rank == 0) {
		MPI_Comm self = MPI_COMM_SELF;
		PDI_expose("my_comm", &self, PDI_OUT);
		verify_matrix(!comm_color);
	}
	
	MPI_Comm_free(&my_comm);
	
	PDI_finalize();
	MPI_Finalize();
	return 0;
}