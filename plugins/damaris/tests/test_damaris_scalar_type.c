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
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

#define IMX 10

int main(int argc, char* argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: argc=%d \n", argc);
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		for (int ii=0; ii<argc; ++ii) {
			fprintf(stderr, "Usage: argv[%d]=%s\n", ii, argv[ii]);
		}
		exit(1);
	}
	MPI_Init(&argc, &argv);

	MPI_Comm main_comm = MPI_COMM_WORLD;
	int world_size;
	MPI_Comm_size(MPI_COMM_WORLD, &world_size);
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

	int nn_first_call = IMX / 2;

	int is_client = 0;
	PDI_expose("is_client", &is_client, PDI_INOUT); // The order doesn't care
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // <-- allow plugin to set, returns Damaris client comm

	printf("value of is_client %d=", is_client);
	if (is_client) {
		int size = nn_first_call;
		int int_values[IMX];
		int nb_calls = 0;
		for (int ii = 0; ii < size; ++ii) {
			int_values[ii] = 100 + ii;
		}
		PDI_expose("nn", &size, PDI_INOUT);
		PDI_multi_expose("write", "nn", &size, PDI_INOUT, "nbcalls", &nb_calls, PDI_INOUT, "int_values", int_values, PDI_OUT, NULL);

		// The value 'nn' can't not be updated (Error in Damaris xml input file ???)
		// change size of the vector give to pdi for the second call
		nb_calls = nb_calls + 1;
		size = IMX;

		for (int ii = size / 2; ii < size; ++ii) {
			int_values[ii] = 300 + ii;
		}
		PDI_expose("nn", &size, PDI_INOUT);
		PDI_multi_expose("write", "nn", &size, PDI_INOUT, "nbcalls", &nb_calls, PDI_INOUT, "int_values", int_values, PDI_OUT, NULL);
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();

	return EXIT_SUCCESS;
}
