/*
 * SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>
#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "mpi_independent_test.h5"

/**
* Test : Read a file using PDI with the plugin decl_hdf5 in parallel with the option mpio: INDEPENDENT.
*/

int main(int argc, char* argv[])
{
	printf("PDI mpi_independent_read_test started\n");
	MPI_Init(&argc, &argv);
	int mpi_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

	const char* CONFIG_YAML
		= "logging: trace                                               \n"
		  "metadata:                                                    \n"
		  "  mpi_rank: int                                              \n"
		  "data:                                                        \n"
		  "  array_data: { size: [5, 5], type: array, subtype: int }    \n"
		  "plugins:                                                     \n"
		  "  mpi: ~                                                     \n"
		  "  decl_hdf5:                                                 \n"
		  "    communicator: $MPI_COMM_WORLD                            \n"
		  "    file: mpi_independent_test.h5                            \n"
		  "    datasets:                                                \n"
		  "      array_data: {type: array, subtype: int, size: [5, 10]} \n"
		  "    read:                                                    \n"
		  "      mpio: INDEPENDENT                                      \n"
		  "      array_data:                                            \n"
		  "        - memory_selection:                                  \n"
		  "            size: [5, 5]                                     \n"
		  "          dataset_selection:                                 \n"
		  "            size: [5, 5]                                     \n"
		  "            start: [0, $mpi_rank * 5]                        \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);

	PDI_expose("mpi_rank", &mpi_rank, PDI_OUT);

	int test_array[5][5];
	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 5; j++) {
			test_array[i][j] = 0;
		}
	}
	PDI_expose("array_data", test_array, PDI_IN);

	for (int i = 0; i < 5; i++) {
		for (int j = 0; j < 5; j++) {
			if (test_array[i][j] != i * 10 + j + 5 * mpi_rank) {
				fprintf(stderr, "[%d][%d] %d != %d\n ", i, j, test_array[i][j], i * 10 + j + 5 * mpi_rank);
				return 1;
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	MPI_Finalize();

	printf("[Rank: %d] PDI mpi_read_test finalized\n", mpi_rank);
	return 0;
}
