/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <vector>
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: argc=%d \n", argc);
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		for (int ii = 0; ii < argc; ++ii) {
			fprintf(stderr, "Usage: argv[%d]=%s\n", ii, argv[ii]);
		}
		exit(1);
	}

	MPI_Init(&argc, &argv);

	MPI_Comm main_comm = MPI_COMM_WORLD;
	int world_size;
	MPI_Comm_size(MPI_COMM_WORLD, &world_size);

	// get specification tree
	PC_tree_t conf = PC_parse_path(argv[1]);

	int const dim = 2;
	long longval;
	PC_int(PC_get(conf, ".dimension"), &longval);
	if (longval != dim) {
		printf("Error in the dimension of the multidimensional data in the yaml file: %d (program) != %d (yaml).", dim, (int)longval);
		exit(1);
	}

	std::array<int, dim> size_with_ghost;
	std::array<int, dim> ghost_left;
	std::array<int, dim> ghost_right;

	// read datasize
	PC_int(PC_get(conf, ".datasize[0]"), &longval);
	size_with_ghost[0] = longval;

	PC_int(PC_get(conf, ".datasize[1]"), &longval);
	size_with_ghost[1] = longval;

	// read ghost left
	PC_int(PC_get(conf, ".ghost_left[0]"), &longval);
	ghost_left[0] = longval;

	PC_int(PC_get(conf, ".ghost_left[1]"), &longval);
	ghost_left[1] = longval;

	// read ghost_right
	PC_int(PC_get(conf, ".ghost_right[0]"), &longval);
	ghost_right[0] = longval;

	PC_int(PC_get(conf, ".ghost_right[1]"), &longval);
	ghost_right[1] = longval;

	std::array<int, dim> global_size_no_ghost{};

	for (int ii = 0; ii < dim; ++ii) {
		global_size_no_ghost[ii] = size_with_ghost[ii] - ghost_left[ii] - ghost_right[ii];
	}

	// initialize pdi
	PDI_init(PC_get(conf, ".pdi"));

	int is_client = 1;
	PDI_expose("is_client", &is_client, PDI_INOUT); // The order doesn't care
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // <-- allow plugin to set, returns Damaris client comm

	if (is_client) {
		// define the size
		PDI_expose("dim", &dim, PDI_INOUT);
		PDI_expose("datasize", size_with_ghost.data(), PDI_INOUT);
		PDI_expose("ghost_left", ghost_left.data(), PDI_INOUT);
		PDI_expose("ghost_right", ghost_right.data(), PDI_INOUT);
		PDI_expose("global_size", global_size_no_ghost.data(), PDI_INOUT);

		size_t total_size = size_with_ghost[0] * size_with_ghost[1];
		std::vector<int> int_values(total_size, 0);

		for (int ii = 0; ii < size_with_ghost[0]; ++ii) {
			for (int jj = 0; jj < size_with_ghost[1]; ++jj) {
				int_values[jj + ii * size_with_ghost[1]] = jj + ii * 100;
			}
		}

		PDI_multi_expose("write", "int_values", int_values.data(), PDI_OUT, NULL);
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();

	return EXIT_SUCCESS;
}
