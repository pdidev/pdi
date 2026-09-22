/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Institut National de Recherche en Sciences et Technologies du Numérique (INRIA)
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
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

int main(int argc, char* argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(1);
	}
	MPI_Init(&argc, &argv);

	MPI_Comm main_comm = MPI_COMM_WORLD;
	int world_size;
	MPI_Comm_size(main_comm, &world_size);
	if (world_size != 2) {
		fprintf(stderr, "Please use at least 2 mpi processes\n");
		exit(1);
	}

	// get specification tree
	PC_tree_t conf = PC_parse_path(argv[1]);

	// initialize pdi
	PDI_init(PC_get(conf, ".pdi"));

	// All processes must initialize Damaris with the XML configuration
	//  - client process = heat simulation process
	//  - server process = damaris process for writting hdf5 file.

	int max_iter = 35;
	int frequency = 10; // frequency of writting the data

	int is_client = 0;
	PDI_expose("is_client", &is_client, PDI_INOUT); // The order doesn't care
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // <-- allow plugin to set, returns Damaris client comm

	if (is_client) {
		PDI_multi_expose("init_data", "frequency", &frequency, PDI_OUT, NULL);

		printf("value of is_client %d=", is_client);
		if (is_client) {
			for (int ii = 0; ii < max_iter; ++ii) {
				fprintf(stdout, "expose iter=%d \n", ii);
				PDI_multi_expose("write", "iter", &ii, PDI_INOUT, NULL);
			}
		}
	}

	PDI_finalize();
	MPI_Finalize();
	PC_tree_destroy(&conf);

	return EXIT_SUCCESS;
}
