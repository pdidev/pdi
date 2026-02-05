/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>
#include <assert.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "mpi_test.h5"

/**
* Test : Write a file using PDI with the plugin decl_hdf5 in parallel with the option mpio: COLLECTIVE(DEFAULT).
*/

int main(int argc, char* argv[])
{
	printf("PDI mpi_write_test started\n");
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
		  "    file: mpi_test.h5                                        \n"
		  "    datasets:                                                \n"
		  "      array_data: {type: array, subtype: int, size: [5, 10]} \n"
		  "    write:                                                   \n"
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
			test_array[i][j] = i * 10 + j + 5 * mpi_rank;
		}
	}

	PDI_expose("array_data", test_array, PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	MPI_Finalize();

	printf("[Rank: %d] HDF5 mpi_write_test finalized\n", mpi_rank);
	return 0;
}
