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
#include <assert.h>
#include <math.h>
#include <paraconf.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "pdi.h"

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(1);
	}

	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm main_comm = MPI_COMM_WORLD;

	PDI_init(PC_get(conf, ".pdi"));

	int psize_1d;
	MPI_Comm_size(main_comm, &psize_1d);
	int pcoord_1d;
	MPI_Comm_rank(main_comm, &pcoord_1d);

	// PDI_expose("mpi_rank", &pcoord_1d, PDI_OUT);
	// PDI_expose("mpi_size", &psize_1d, PDI_OUT);

	long longval;

	int dsize[2];
	PC_int(PC_get(conf, ".globalsize[0]"), &longval);
	dsize[0] = longval;
	PC_int(PC_get(conf, ".globalsize[1]"), &longval);
	dsize[1] = longval;

	int psize[2];
	PC_int(PC_get(conf, ".parallelism.height"), &longval);
	psize[0] = longval;
	PC_int(PC_get(conf, ".parallelism.width"), &longval);
	psize[1] = longval;

	// check on distribution of MPI process
	assert(dsize[0] % psize[0] == 0);
	assert(dsize[1] % psize[1] == 0);
	assert(psize[1] * psize[0] == psize_1d);

	int cart_period[2] = {0, 0};
	MPI_Comm cart_com;
	MPI_Cart_create(main_comm, 2, psize, cart_period, 1, &cart_com);
	int pcoord[2];
	MPI_Cart_coords(cart_com, pcoord_1d, 2, pcoord);

	// compute local size
	dsize[0] = dsize[0] / psize[0];
	dsize[1] = dsize[1] / psize[1];

	int ii = 0;

	PDI_expose("iter", &ii, PDI_OUT);
	PDI_expose("dsize", dsize, PDI_OUT);
	PDI_expose("psize", psize, PDI_OUT);
	PDI_expose("pcoord", pcoord, PDI_OUT);

	int(*cur)[dsize[1]] = malloc(sizeof(int) * dsize[1] * dsize[0]);

	// initialize
	for (int yy = 0; yy < dsize[0]; ++yy) {
		for (int xx = 0; xx < dsize[1]; ++xx) {
			cur[yy][xx] = (pcoord_1d + 1);
		}
	}

	// defined a sub communicator for catalyst
	MPI_Comm subcomm;
	int world_rank = pcoord_1d;
	int color = (world_rank == 0 || world_rank == 3) ? 0 : MPI_UNDEFINED;

	MPI_Comm_split(MPI_COMM_WORLD, color, world_rank, &subcomm);
	PDI_multi_expose("catalyst_initialize", "catalyst_comm", &subcomm, PDI_OUT, NULL);

	// call catalyst execute
	PDI_multi_expose("newiter", "iter", &ii, PDI_INOUT, "main_field", cur, PDI_INOUT, NULL);

	// free cur
	free(cur);

	PDI_finalize();
	PC_tree_destroy(&conf);

	MPI_Finalize();
}
