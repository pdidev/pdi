// Copyright (c) 2017, Institut National de Recherche en Informatique et en Automatique (Inria)
//               2017, EDF
//               2018-2021, Institut National de Recherche en Informatique et en Automatique (Inria)
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
//
// * Neither the name of the copyright holder nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
// TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include <melissa/api.h>
#include <pdi.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <mpi.h>
#include <math.h>

// Fortran interfaces
void read_file(int*, int*, double*, double*, double*);

void load(int*, int*, int*, int*, int*);

void init(double*, int*, int*, double*, double*, int*, double*, double*, double*);

void filling_A(double*, double*, double*, double*, int*, int*, double*);

void filling_F(int*, int*, double*, double*, double*, double*, double*, double*, double*, int*, int*, double*, double*, double*);

void conjgrad(double*, double*, double*, int*, int*, double*, int*, int*, int*, int*, int*, int*, int*);

void finalize(double*, double*, int*, int*, int*, int*, double*, double*, int*, int*);

int main(int argc, char** argv)
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_path("/home/john/build.heat-pde/pdi-melissa.yaml");
	PDI_init(conf);


	if (argc < 2 || argc > 6) {
		fprintf(stderr, "usage: %s <initial temperature> [boundary temperatures]\n", argv[0]);
		return EXIT_FAILURE;
	}

	// The initial temperature is stored in params[0]
	// The four next optional parameters are the boundary temperatures
	double params[5] = {0};

	for (int i = 0; i + 1 < argc; ++i) {
		params[i] = strtod(argv[i + 1], NULL);

		if (!isfinite(params[i])) {
			fprintf(stderr, "argument %d '%s' is not a finite floating-point number\n", i + 1, argv[i + 1]);
			return EXIT_FAILURE;
		}
	}

	// create dedicated MPI communicators for each simulation instance
	int me = -1;
	MPI_Comm_rank(MPI_COMM_WORLD, &me);
	int* p_appnum = NULL;
	int statinfo = -1;
	MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_APPNUM, &p_appnum, &statinfo);
	MPI_Comm comm_app;
	MPI_Comm_split(MPI_COMM_WORLD, *p_appnum, me, &comm_app);
	MPI_Comm_rank(comm_app, &me);
	int num_procs = -1;
	MPI_Comm_size(comm_app, &num_procs);
	int fcomm = MPI_Comm_c2f(comm_app);

	PDI_expose("mpi_communicator", &comm_app, PDI_OUT);

	// Neighbour ranks
	int next = me + 1;
	int previous = me - 1;

	if (me == num_procs - 1) {
		next = MPI_PROC_NULL;
	}
	if (me == 0) {
		previous = MPI_PROC_NULL;
	}

	int nx = 100; // x axis grid subdivisions
	int ny = 100; // y axis grid subdivisions
	double lx = 10.0; // domain size along x axis
	double ly = 10.0; // domain size along y axis
	double d = 1.0; // diffusion coefficient
	int num_time_steps = 100;
	double simulation_time = 1;
	double dx = lx / (nx + 1); // x axis step
	double dy = ly / (ny + 1); // y axis step
	double epsilon = 0.0001; // conjugated gradient precision

	// partition work over MPI processes of this simulation
	// i1: first global cell indices assigned to this process
	int i1 = -1;
	// in: last global cell index assigned to the current process
	int in = -1;
	int num_cells_global = nx * ny;
	load(&me, &num_cells_global, &num_procs, &i1, &in);

	// initialization
	int num_cells = in - i1 + 1;
	PDI_expose("num_cells", &num_cells, PDI_OUT);

	fprintf(stderr, "num_cells=%d\n", num_cells);
	double* u = malloc(num_cells * sizeof(double));
	double* f = malloc(num_cells * sizeof(double));
	init(u, &i1, &in, &dx, &dy, &nx, &lx, &ly, params);
	// initialize the tridiagonal matrix A:
	double a[3] = {0};
	double dt = simulation_time / num_time_steps;
	filling_A(&d, &dx, &dy, &dt, &nx, &ny, a);

	// melissa_init is the first Melissa function to call, and it is called only
	// once by each process in comm_app. It mainly contacts the server.
	//const char field_name[] = "temperature";
	//melissa_init(field_name, num_cells, comm_app);


	// main loop
	for (int n = 0; n < num_time_steps; ++n) {
		double t = 1.0 * (n + 1) / num_time_steps * simulation_time;
		// fill right-hand side vector F before each iteration
		filling_F(&nx, &ny, u, &d, &dx, &dy, &dt, &t, f, &i1, &in, &lx, &ly, params);
		// solve Au = F
		conjgrad(a, f, u, &nx, &ny, &epsilon, &i1, &in, &num_procs, &me, &next, &previous, &fcomm);
		// send u to the Melissa server
		//melissa_send(field_name, u);
		PDI_expose("temperature", u, PDI_OUT);
	}

	// melissa_finalize closes the connection with the server.
	// No Melissa function should be called after melissa_finalize.
	//melissa_finalize();
	PDI_finalize();

	free(u);
	free(f);

	MPI_Finalize();
}
