/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>
#include <pdi.h>

const char* CONFIG_YAML
	= "logging: trace                                         \n"
	  "metadata:                                              \n"
	  "  pcoord: { type: array, size: 2, subtype: int }       \n"
	  "  my_comm: MPI_Comm                                    \n"
	  "data:                                                  \n"
	  "  matrix_in: {type: array, size: [2, 2], subtype: int} \n"
	  "  matrix_out: {type: array, size: [2, 2], subtype: int}\n"
	  "                                                       \n"
	  "plugins:                                               \n"
	  "  mpi:                                                 \n"
	  "  decl_hdf5:                                           \n"
	  "    file: data.h5                                      \n"
	  "    communicator: $my_comm                             \n"
	  "    datasets:                                          \n"
	  "      matrix_dataset:                                  \n"
	  "        type: array                                    \n"
	  "        subtype: int                                   \n"
	  "        size: [2, 2]                                   \n"
	  "    write:                                             \n"
	  "      matrix_out:                                      \n"
	  "        dataset: matrix_dataset                        \n"
	  "        memory_selection:                              \n"
	  "          size:  [1, 1]                                \n"
	  "        dataset_selection:                             \n"
	  "          size:  [1, 1]                                \n"
	  "          start: ['$pcoord[0]', '$pcoord[1]']          \n"
	  "    read:                                              \n"
	  "      matrix_in:                                       \n"
	  "        dataset: matrix_dataset                        \n"
	  "        memory_selection:                              \n"
	  "          size:  [2, 2]                                \n"
	  "        dataset_selection:                             \n"
	  "          size:  [2, 2]                                \n";

void verify_matrix(int comm_color, int world_rank)
{
	int matrix[4];
	PDI_expose("matrix_in", matrix, PDI_IN);
	if (comm_color) {
		for (int i = 0; i < 4; i++) {
			if (matrix[i] != i) {
				printf("[%d]: m[%d] != %d\n", world_rank, i, matrix[i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	} else {
		for (int i = 0; i < 4; i++) {
			if (matrix[i] != i + 4) {
				printf("[%d]: m[%d] != %d\n", world_rank, i + 4, matrix[i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	}
}

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	int world_size;
	MPI_Comm_size(MPI_COMM_WORLD, &world_size);
	if (world_size != 8) {
		printf("world_size must be 8 instead of %d.", world_size);
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

	//split the communicator
	int comm_color = (world_rank < 4);

	MPI_Comm my_comm;
	MPI_Comm_split(MPI_COMM_WORLD, comm_color, 0, &my_comm);
	PDI_expose("my_comm", &my_comm, PDI_OUT);

	int my_comm_rank;
	MPI_Comm_rank(my_comm, &my_comm_rank);
	int pcoord[2] = {my_comm_rank / 2, my_comm_rank % 2};
	PDI_expose("pcoord", pcoord, PDI_OUT);

	// first half of processes writes
	if (comm_color) {
		int matrix = world_rank;
		printf("Exposing: %d\n", world_rank);
		PDI_expose("matrix_out", &matrix, PDI_OUT);
	}

	// verify file
	if (world_rank == 0) {
		MPI_Comm self = MPI_COMM_SELF;
		PDI_expose("my_comm", &self, PDI_OUT);
		verify_matrix(comm_color, world_rank);
	}

	// for better console output
	MPI_Barrier(MPI_COMM_WORLD);

	// second half of processes writes
	if (!comm_color) {
		int matrix = world_rank;
		printf("Exposing: %d\n", world_rank);
		PDI_expose("matrix_out", &matrix, PDI_OUT);
	}

	// wait for second half of processes
	MPI_Barrier(MPI_COMM_WORLD);

	// verify file
	if (world_rank == 0) {
		MPI_Comm self = MPI_COMM_SELF;
		PDI_expose("my_comm", &self, PDI_OUT);
		verify_matrix(!comm_color, world_rank);
	}

	MPI_Comm_free(&my_comm);

	PDI_finalize();
	MPI_Finalize();
}
