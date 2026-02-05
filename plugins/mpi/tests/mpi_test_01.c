/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>

#include <assert.h>

#include <paraconf.h>
#include <pdi.h>

const char* YAML_CONFIG
	= "logging: trace            \n"
	  "metadata:                 \n"
	  "  my_array:               \n"
	  "    size: [3, 3]          \n"
	  "    subsize: [2, 2]       \n"
	  "    start: [1, 1]         \n"
	  "    subtype: MPI_Comm\n"
	  "    type: array           \n"
	  "plugins:                  \n"
	  "  mpi:                    \n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	PDI_init(conf);

	MPI_Comm sparse_array[9]; // buffer: 3 x 3; data: 2 x 2; start: 1, 1
	MPI_Comm* dense_array; // buffer: 2 x 2

	for (int i = 0; i < 9; i++) {
		MPI_Comm_dup(MPI_COMM_WORLD, sparse_array + i);
	}

	// metadata expose creates a dense copy inside PDI
	PDI_expose("my_array", sparse_array, PDI_OUT);
	PDI_access("my_array", (void**)&dense_array, PDI_IN);

	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	for (int i = 0; i < 4; i++) {
		int comm_rank;
		MPI_Comm_rank(dense_array[i], &comm_rank);
		assert(world_rank == comm_rank);
	}
	PDI_release("my_array");

	for (int i = 0; i < 9; i++) {
		MPI_Comm_free(sparse_array + i);
	}

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
