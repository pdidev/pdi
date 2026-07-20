/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#ifndef WITHOUT_PARACONF
#include <paraconf.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "pdi.h"

void init(int dsize[2], int pcoord[2], double dat[dsize[0]][dsize[1]])
{
	for (int yy = 0; yy < dsize[0]; ++yy) {
		for (int xx = 0; xx < dsize[1]; ++xx) {
			dat[yy][xx] = 0;
		}
	}
	if (pcoord[1] == 0) {
		for (int yy = 0; yy < dsize[0]; ++yy) {
			dat[yy][0] = 1000000;
		}
	}
}

void iter(int dsize[2], double cur[dsize[0]][dsize[1]], double next[dsize[0]][dsize[1]])
{
	int xx, yy;
	for (xx = 0; xx < dsize[1]; ++xx) {
		next[0][xx] = cur[0][xx];
	}
	for (yy = 1; yy < dsize[0] - 1; ++yy) {
		next[yy][0] = cur[yy][0];
		for (xx = 1; xx < dsize[1] - 1; ++xx) {
			next[yy][xx]
				= (cur[yy][xx] * .5) + (cur[yy][xx - 1] * .125) + (cur[yy][xx + 1] * .125) + (cur[yy - 1][xx] * .125) + (cur[yy + 1][xx] * .125);
		}
		next[yy][dsize[1] - 1] = cur[yy][dsize[1] - 1];
	}
	for (xx = 0; xx < dsize[1]; ++xx) {
		next[dsize[0] - 1][xx] = cur[dsize[0] - 1][xx];
	}
}

void exchange(MPI_Comm cart_com, int dsize[2], double cur[dsize[0]][dsize[1]])
{
	MPI_Status status;
	int rank_source, rank_dest;
	static MPI_Datatype column, row;
	static int initialized = 0;

	if (!initialized) {
		MPI_Type_vector(dsize[0] - 2, 1, dsize[1], MPI_DOUBLE, &column);
		MPI_Type_commit(&column);
		MPI_Type_contiguous(dsize[1] - 2, MPI_DOUBLE, &row);
		MPI_Type_commit(&row);
		initialized = 1;
	}

	/* send down */
	MPI_Cart_shift(cart_com, 0, 1, &rank_source, &rank_dest);
	MPI_Sendrecv(
		&cur[dsize[0] - 2][1],
		1,
		row,
		rank_dest,
		100, /* send row before ghost */
		&cur[0][1],
		1,
		row,
		rank_source,
		100, /* receive 1st row (ghost) */
		cart_com,
		&status
	);

	/* send up */
	MPI_Cart_shift(cart_com, 0, -1, &rank_source, &rank_dest);
	MPI_Sendrecv(
		&cur[1][1],
		1,
		row,
		rank_dest,
		100, /* send column after ghost */
		&cur[dsize[0] - 1][1],
		1,
		row,
		rank_source,
		100, /* receive last column (ghost) */
		cart_com,
		&status
	);

	/* send to the right */
	MPI_Cart_shift(cart_com, 1, 1, &rank_source, &rank_dest);
	MPI_Sendrecv(
		&cur[1][dsize[1] - 2],
		1,
		column,
		rank_dest,
		100, /* send column before ghost */
		&cur[1][0],
		1,
		column,
		rank_source,
		100, /* receive 1st column (ghost) */
		cart_com,
		&status
	);

	/* send to the left */
	MPI_Cart_shift(cart_com, 1, -1, &rank_source, &rank_dest);
	MPI_Sendrecv(
		&cur[1][1],
		1,
		column,
		rank_dest,
		100, /* send column after ghost */
		&cur[1][dsize[1] - 1],
		1,
		column,
		rank_source,
		100, /* receive last column (ghost) */
		cart_com,
		&status
	);
}

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s <config_file>\n", argv[0]);
		exit(1);
	}

#ifndef WITHOUT_PARACONF
	PC_tree_t conf = PC_parse_path(argv[1]);
#endif

	MPI_Comm main_comm = MPI_COMM_WORLD;
#ifndef WITHOUT_PARACONF
	PDI_init(PC_get(conf, ".pdi"));
#endif

	PDI_expose("mpi_comm", &main_comm, PDI_INOUT);

	int psize_1d;
	MPI_Comm_size(main_comm, &psize_1d);
	int pcoord_1d;
	MPI_Comm_rank(main_comm, &pcoord_1d);

	PDI_expose("mpi_rank", &pcoord_1d, PDI_OUT);
	PDI_expose("mpi_size", &psize_1d, PDI_OUT);
	PDI_event("init");

	long longval;

	int dsize[2];
#ifndef WITHOUT_PARACONF
	PC_int(PC_get(conf, ".datasize[0]"), &longval);
#else
	longval = 8; // size is 8 by default because, why not
#endif
	dsize[0] = longval;
#ifndef WITHOUT_PARACONF
	PC_int(PC_get(conf, ".datasize[1]"), &longval);
#else
	longval = 8 * psize_1d; // default size is 8 * psize_1d so that it's easy to parallelize over psize_1d processes
#endif
	dsize[1] = longval;

	int psize[2];
#ifndef WITHOUT_PARACONF
	PC_int(PC_get(conf, ".parallelism.height"), &longval);
#else
	longval = 1;
#endif
	psize[0] = longval;
#ifndef WITHOUT_PARACONF
	PC_int(PC_get(conf, ".parallelism.width"), &longval);
#else
	longval = psize_1d; // all the parallelism is here by default
#endif
	psize[1] = longval;

	double duration;
#ifndef WITHOUT_PARACONF
	PC_double(PC_get(conf, ".duration"), &duration);
#else
	duration = 0.1;
#endif

	// get local & add ghosts to sizes
	assert(dsize[0] % psize[0] == 0);
	dsize[0] = dsize[0] / psize[0] + 2;
	assert(dsize[1] % psize[1] == 0);
	dsize[1] = dsize[1] / psize[1] + 2;

	assert(psize[1] * psize[0] == psize_1d);

	int cart_period[2] = {0, 0};
	MPI_Comm cart_com;
	MPI_Cart_create(main_comm, 2, psize, cart_period, 1, &cart_com);
	int pcoord[2];
	MPI_Cart_coords(cart_com, pcoord_1d, 2, pcoord);

	int ii = 0;
	PDI_expose("iter", &ii, PDI_OUT);
	PDI_expose("dsize", dsize, PDI_OUT);
	PDI_expose("psize", psize, PDI_OUT);
	PDI_expose("pcoord", pcoord, PDI_OUT);

	double(*cur)[dsize[1]] = malloc(sizeof(double) * dsize[1] * dsize[0]);
	double(*next)[dsize[1]] = malloc(sizeof(double) * dsize[1] * dsize[0]);

	init(dsize, pcoord, cur);

	PDI_event("main_loop");
	double start = MPI_Wtime();
	int next_reduce = 0;
	for (ii = 0;; ++ii) {
		PDI_multi_expose("newiter", "iter", &ii, PDI_INOUT, "main_field", cur, PDI_INOUT, NULL);

		iter(dsize, cur, next);
		exchange(cart_com, dsize, next);
		double(*tmp)[dsize[1]] = cur;
		cur = next;
		next = tmp;

		if (ii >= next_reduce) {
			double local_time, global_time;
			local_time = MPI_Wtime() - start;
			MPI_Allreduce(&local_time, &global_time, 1, MPI_DOUBLE, MPI_MAX, main_comm);
			if (global_time >= duration) {
				if (0 == pcoord_1d) printf("iter=%7d; time=%7.3f; STOP!!!\n", ii, global_time);
				break;
			}
			int rem_iter = .9 * (duration - global_time) * (ii + 1) / (global_time + 0.1);
			if (rem_iter < 1) rem_iter = 1;
			next_reduce = ii + rem_iter;
			if (0 == pcoord_1d) printf("iter=%7d; time=%7.3f; next_reduce=%7d\n", ii, global_time, next_reduce);
		}
	}
	PDI_event("finalization");
	PDI_expose("iter", &ii, PDI_OUT);
	PDI_expose("main_field", cur, PDI_OUT);

	PDI_finalize();

#ifndef WITHOUT_PARACONF
	PC_tree_destroy(&conf);
#endif

	free(cur);
	free(next);

	MPI_Finalize();
}
