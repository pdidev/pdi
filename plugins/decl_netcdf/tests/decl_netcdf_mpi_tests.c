/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>
#include <pdi.h>

// Tests netcdf parallel write and read
int main(int argc, char* argv[])
{
	const char* CONFIG_YAML
		= "logging: trace                                                             \n"
		  "metadata:                                                                  \n"
		  "  mpi_rank: int                                                            \n"
		  "  # mpi_size = 4                                                           \n"
		  "data:                                                                      \n"
		  "  int_submatrix:                                                           \n"
		  "    type: array                                                            \n"
		  "    subtype: int                                                           \n"
		  "    size: [4, 4]                                                           \n"
		  "                                                                           \n"
		  "plugins:                                                                   \n"
		  "  mpi: ~                                                                   \n"
		  "  decl_netcdf:                                                             \n"
		  "    - file: 'test_06.nc'                                                   \n"
		  "      communicator: $MPI_COMM_WORLD                                        \n"
		  "      on_event: 'write'                                                    \n"
		  "      variables:                                                           \n"
		  "        int_matrix_var:                                                    \n"
		  "          type: array                                                      \n"
		  "          subtype: int                                                     \n"
		  "          size: [8, 8]                                                     \n"
		  "          dimensions: ['height', 'width']                                  \n"
		  "      write:                                                               \n"
		  "        int_submatrix:                                                     \n"
		  "          variable: int_matrix_var                                         \n"
		  "          variable_selection:                                              \n"
		  "            start: ['($mpi_rank / 2) * 4', '($mpi_rank % 2) * 4']          \n"
		  "            subsize: [4, 4]                                                \n"
		  "    - file: 'test_06.nc'                                                   \n"
		  "      communicator: $MPI_COMM_WORLD                                        \n"
		  "      on_event: 'read'                                                     \n"
		  "      variables:                                                           \n"
		  "        int_matrix_var:                                                    \n"
		  "          type: array                                                      \n"
		  "          subtype: int                                                     \n"
		  "          size: [8, 8]                                                     \n"
		  "      read:                                                                \n"
		  "        int_submatrix:                                                     \n"
		  "          variable: int_matrix_var                                         \n"
		  "          variable_selection:                                              \n"
		  "            start: ['($mpi_rank / 2) * 4', '($mpi_rank % 2) * 4']          \n"
		  "            subsize: [4, 4]                                                \n";

	MPI_Init(&argc, &argv);
	PDI_init(PC_parse_string(CONFIG_YAML));

	int mpi_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

	PDI_expose("mpi_rank", &mpi_rank, PDI_OUT);

	// init data
	int int_matrix[4][4];

	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix[i][j] = 100 * mpi_rank + i * 4 + j;
		}
	}

	// write data
	PDI_multi_expose("write", "int_submatrix", int_matrix, PDI_OUT, NULL);

	// zero data
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			int_matrix[i][j] = 0;
		}
	}

	// read data
	PDI_multi_expose("read", "int_submatrix", int_matrix, PDI_IN, NULL);

	// verify
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 4; j++) {
			printf("[%d][%d] %d ?= %d\n", i, j, int_matrix[i][j], 100 * mpi_rank + i * 4 + j);
			if (int_matrix[i][j] != 100 * mpi_rank + i * 4 + j) {
				printf("[%d][%d] %d != %d\n", i, j, int_matrix[i][j], 100 * mpi_rank + i * 4 + j);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	}

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
