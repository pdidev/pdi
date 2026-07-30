/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <array>
#include <filesystem>
#include <numeric>
#include <ranges>

#include <pdi.h>
#include <pdi/random_generator.h>

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

	std::mt19937_64 random_generator_used;
	random_generator_used.seed(1024 + mpi_rank); /// define a seed for each mpi process

	auto const int_matrix = PDI::make_random<std::array<std::array<int, 4>, 4>>(random_generator_used);

	// write data
	PDI_multi_expose("write", "int_submatrix", int_matrix.data(), PDI_OUT, NULL);

	std::array< std::array<int, 4>, 4> int_matrix_read{}; // initialize all elements by zero

	if (std::filesystem::exists("test_06.nc")) {
		// read data
		PDI_multi_expose("read", "int_submatrix", int_matrix_read.data(), PDI_IN, NULL);
		// verify
		for (int i = 0; i < 4; i++) {
			for (int j = 0; j < 4; j++) {
				if (int_matrix[i][j] != int_matrix_read[i][j]) {
					printf("[MPI %d] [%d][%d] %d != %d\n", mpi_rank, i, j, int_matrix[i][j], int_matrix_read[i][j]);
					MPI_Abort(MPI_COMM_WORLD, -1);
				}
			}
		}
	} else {
		printf("[MPI %d] the file `test_06.nc' doesn't exists");
		MPI_Abort(MPI_COMM_WORLD, -1);
	}

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
