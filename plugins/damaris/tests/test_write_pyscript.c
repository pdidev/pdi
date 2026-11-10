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

/* Writer for the Damaris `pyscript` tests.
 *
 * Every client owns PYSCRIPT_LOCAL_SIZE ints of a 1D `values` dataset, split by client rank.
 * At iteration `it`, values[i] = 1000 * rank + 10 * it + i. The plugin writes `values` to Damaris
 * and ends the Damaris iteration, which runs the configured Python script on the server cores.
 */

#include <mpi.h>
#include <paraconf.h>
#include <stdio.h>
#include <stdlib.h>

#include <pdi.h>

#define PYSCRIPT_LOCAL_SIZE 8
#define PYSCRIPT_NB_ITER 5

int main(int argc, char* argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(1);
	}
	MPI_Init(&argc, &argv);

	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(PC_get(conf, ".pdi"));

	int is_client = 0;
	MPI_Comm main_comm = MPI_COMM_WORLD;
	PDI_expose("is_client", &is_client, PDI_INOUT);
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // returns the Damaris client communicator

	if (is_client) {
		int psize, prank;
		MPI_Comm_size(main_comm, &psize);
		MPI_Comm_rank(main_comm, &prank);
		PDI_expose("psize", &psize, PDI_OUT);
		PDI_expose("prank", &prank, PDI_OUT);

		int values[PYSCRIPT_LOCAL_SIZE];
		for (int it = 0; it < PYSCRIPT_NB_ITER; ++it) {
			for (int i = 0; i < PYSCRIPT_LOCAL_SIZE; ++i) {
				values[i] = 1000 * prank + 10 * it + i;
			}
			PDI_multi_expose("newiter", "iter", &it, PDI_OUT, "values", values, PDI_OUT, NULL);
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();

	return EXIT_SUCCESS;
}
