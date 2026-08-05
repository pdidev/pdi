/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

	long longval;
	PC_int(PC_get(conf, ".parallelism"), &longval);
	if (world_size != longval) {
		fprintf(stderr, "Please use at least %ld mpi processes\n",longval);
		exit(1);
	}

	PC_int(PC_get(conf, ".localsize"), &longval);
	int dsize = longval;
	if (dsize < 1) {
		fprintf(stderr, "size=%d is not positive\n", dsize);
		exit(1);
	}

	// initialize pdi
	PDI_init(PC_get(conf, ".pdi"));

	// All processes must initialize Damaris with the XML configuration
	//  - client process = heat simulation process
	//  - server process = damaris process for writting hdf5 file.

	int is_client = 1;
	PDI_expose("is_client", &is_client, PDI_INOUT); // The order doesn't care
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // <-- allow plugin to set, returns Damaris client comm

	printf("value of is_client %d=", is_client);
	if (is_client) {

		int psize_1d;
		MPI_Comm_size(main_comm, &psize_1d);
		int pcoord_1d;
		MPI_Comm_rank(main_comm, &pcoord_1d);

		PDI_expose("psize", &psize_1d, PDI_OUT);
		PDI_expose("pcoord", &pcoord_1d, PDI_OUT);

		PDI_expose("dsize", &dsize, PDI_OUT);

		int int_values[dsize];

		for (int ii = 0; ii < dsize; ++ii) {
			int_values[ii] = pcoord_1d*100 + ii;
		}

		PDI_multi_expose("write", "int_values", int_values, PDI_OUT, NULL);
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();

	return EXIT_SUCCESS;
}
