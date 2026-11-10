/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 National Institute for Research in Digital Science and Technology (Inria)
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

/* Damaris plugin example: passing simulation data to Python (and Dask) via Damaris.
 *
 * Each client rank owns a [depth/size][height][width] slab of a global 3D cube and,
 * at every iteration, exposes it to PDI. The Damaris plugin writes the datasets listed
 * under `write:` and, once they are all written, calls damaris_end_iteration(), which
 * runs the configured `pyscript` on the Damaris server cores.
 *
 *   mpirun -np 5 ./3dmesh_py 3dmesh_py.yml -i 3 -v 2 -r      # Python only (1 server core)
 *   mpirun -np 6 ./3dmesh_py 3dmesh_dask.yml -i 3 -v 2 -r    # Python + Dask (2 server cores)
 */

#include <mpi.h>
#include <paraconf.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pdi.h"

static void print_usage(char* exename)
{
	fprintf(stderr, "Usage: %s <config.yml> [-v X] [-r] [-s Y] [-i I]\n", exename);
	fprintf(stderr, "-v  <X>    X = 0 default, do not print arrays\n");
	fprintf(stderr, "           X = 1 Verbose mode, prints arrays\n");
	fprintf(stderr, "           X = 2 Verbose mode, prints summation of arrays\n");
	fprintf(stderr, "-r         Array values set as rank of process\n");
	fprintf(stderr, "-s  <Y>    Y is integer time to sleep in seconds between iterations\n");
	fprintf(stderr, "-i  <I>    I is the number of iterations of simulation to run\n");
}

int main(int argc, char** argv)
{
	if (argc < 2) {
		print_usage(argv[0]);
		exit(1);
	}

	MPI_Init(&argc, &argv);

	int verbose = 0;
	int rank_only = 0;
	int sleep_time = 0;
	int max_cycles = 5; // default number of iterations to run
	for (int current_arg = 2; current_arg < argc; current_arg++) {
		if (strcmp(argv[current_arg], "-v") == 0 && current_arg + 1 < argc) {
			verbose = atoi(argv[++current_arg]);
		} else if (strcmp(argv[current_arg], "-r") == 0) {
			rank_only = 1;
		} else if (strcmp(argv[current_arg], "-s") == 0 && current_arg + 1 < argc) {
			sleep_time = atoi(argv[++current_arg]);
		} else if (strcmp(argv[current_arg], "-i") == 0 && current_arg + 1 < argc) {
			max_cycles = atoi(argv[++current_arg]);
		} else {
			print_usage(argv[0]);
			MPI_Finalize();
			exit(strcmp(argv[current_arg], "-h") == 0 ? 0 : 1);
		}
	}

	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(PC_get(conf, ".pdi"));

	// Global cube size [depth, height, width], read from the top of the YAML file
	long longval;
	int gsize[3];
	PC_int(PC_get(conf, ".cube.depth"), &longval);
	gsize[0] = longval;
	PC_int(PC_get(conf, ".cube.height"), &longval);
	gsize[1] = longval;
	PC_int(PC_get(conf, ".cube.width"), &longval);
	gsize[2] = longval;

	// Starts Damaris: server ranks run their loop inside this expose and come back with
	// is_client == 0 once the clients are done; client ranks get the client communicator.
	int is_client = 0;
	MPI_Comm comm = MPI_COMM_WORLD;
	PDI_expose("is_client", &is_client, PDI_INOUT);
	PDI_expose("mpi_comm", &comm, PDI_INOUT);

	if (is_client) {
		int rank_client, size_client;
		MPI_Comm_rank(comm, &rank_client);
		MPI_Comm_size(comm, &size_client);

		if (gsize[0] % size_client != 0) {
			fprintf(stderr, "ERROR: cube depth (%d) must be divisible by the number of clients (%d)\n", gsize[0], size_client);
			MPI_Abort(comm, 1);
		}

		// The layout dimensions and write position depend on these metadata
		PDI_expose("gsize", gsize, PDI_OUT);
		PDI_expose("psize", &size_client, PDI_OUT);
		PDI_expose("prank", &rank_client, PDI_OUT);

		const int local_depth = gsize[0] / size_client;
		const int local_height = gsize[1];
		const int local_width = gsize[2];
		const int rank_start = rank_client * local_depth * local_height * local_width;

		int(*cube)[local_height][local_width] = malloc(sizeof(int) * local_depth * local_height * local_width);
		float(*cube_f)[local_height][local_width] = malloc(sizeof(float) * local_depth * local_height * local_width);

		if (verbose > 0 && rank_client == 0) printf("C      Input parameters found: v=%d r=%d (0 is not found)\n", verbose, rank_only);

		for (int it = 0; it < max_cycles; it++) {
			double t1 = MPI_Wtime();
			int sequence = it; // this is the start of the sequence for this iteration
			long sumdata = 0;
			for (int d = 0; d < local_depth; d++) {
				for (int h = 0; h < local_height; h++) {
					for (int w = 0; w < local_width; w++) {
						cube[d][h][w] = sequence + rank_start;
						cube_f[d][h][w] = (float)(sequence + rank_start) + 0.5f;
						sumdata += cube[d][h][w];
						if (!rank_only) sequence++;
					}
				}
			}

			if (verbose == 1) { // print the values, one rank after the other
				for (int r = 0; r < size_client; r++) {
					if (r == rank_client) {
						printf("\nC      iteration %d rank %d:\n", it, rank_client);
						for (int d = 0; d < local_depth; d++) {
							for (int h = 0; h < local_height; h++) {
								for (int w = 0; w < local_width; w++) {
									printf("%2d\t", cube[d][h][w]);
								}
								printf("\n");
							}
							printf("\n");
						}
						fflush(stdout);
					}
					MPI_Barrier(comm);
				}
			}

			// Sum of the whole cube, used by the Python side to check what it received
			long reduction_result = 0;
			MPI_Allreduce(&sumdata, &reduction_result, 1, MPI_LONG, MPI_SUM, comm);
			double array_sum = (double)reduction_result;
			if (verbose == 2 && rank_client == 0) printf("C      iteration %d , Sum () = %ld\n", it, reduction_result);

			// The plugin writes every dataset listed under `write:`, then ends the Damaris iteration
			PDI_multi_expose(
				"newiter",
				"iter",
				&it,
				PDI_OUT,
				"cube_i",
				cube,
				PDI_OUT,
				"cube_f",
				cube_f,
				PDI_OUT,
				"last_iter",
				&max_cycles,
				PDI_OUT,
				"array_sum",
				&array_sum,
				PDI_OUT,
				NULL
			);

			if (sleep_time > 0) sleep(sleep_time);
			MPI_Barrier(comm);

			double t2 = MPI_Wtime();
			if (verbose > 0 && rank_client == 0) printf("C      iteration %d done in %f seconds\n", it, (t2 - t1));
		}

		free(cube);
		free(cube_f);
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();

	return 0;
}
