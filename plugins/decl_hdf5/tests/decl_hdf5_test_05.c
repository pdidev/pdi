/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   forcumentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   enforrse or promote products derived from this software without specific
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

#include <assert.h>
#include <unistd.h>
#include <mpi.h>
#include <pdi.h>
#include <time.h>

#define IMX 5
#define JMX 4
#define NI_GHOST 1
#define NJ_GHOST 2
#define DIM 2

int main(int argc, char* argv[])
{
	const int icst = -1; /// constants values in the ghost nodes
	const double rcst = -1.1;
	
	int nig = NI_GHOST, njg = NJ_GHOST;
	int ni = IMX, nj = JMX;
	int   values[JMX + 2*NJ_GHOST][IMX + NI_GHOST * 2] = {{0}}, cp_values[JMX + 2*NJ_GHOST][IMX + NI_GHOST * 2] = {{0}};
	double reals[JMX + 2*NJ_GHOST][IMX + NI_GHOST * 2] = {{0}},  cp_reals[JMX + 2*NJ_GHOST][IMX + NI_GHOST * 2] = {{0}};
	int i, j, input;
	int nit, njt;
	double starting_time = time(NULL); // random value
	
	/// MPI and parallel data or info
	int dims[DIM], coord[DIM], periodic[DIM];
	int istart, jstart;
	MPI_Comm comm2D;
	periodic[0] = 0;
	periodic[1] = 0;
	dims[0] = 2;
	dims[1] = 2;
	
	
	MPI_Init(&argc, &argv);
	assert(argc == 2 && "Needs 1 single arg: config file");
	
	PC_tree_t conf = PC_parse_path(argv[1]);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_status_t err = PDI_init(conf, &world);
	int rank; MPI_Comm_rank(world, &rank);
	{
		/// setting nb of procs.
		int size; MPI_Comm_size(world, &size);
		assert(size == 4 && "Run on 4 procs only.");
		PDI_expose("nproc",&size, PDI_OUT);
	}
	PDI_expose("rank", &rank, PDI_OUT);
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	
	MPI_Cart_create(world, DIM, dims, periodic, 0, &comm2D);
	MPI_Cart_coords(comm2D, rank, DIM, coord);
	
	istart = coord[1] * ni ;
	jstart = coord[0] * nj ;
	
	nit = 2*ni;
	njt = 2*nj;
	
	PDI_expose("nig", &nig, PDI_OUT); /// Ghost cells
	PDI_expose("njg", &njg, PDI_OUT);
	
	PDI_expose("ni", &ni, PDI_OUT); /// Size of the portion of the array for a given MPI task
	PDI_expose("nj", &nj, PDI_OUT);
	
	PDI_expose("nit", &nit, PDI_OUT); ///  size of the distributed array
	PDI_expose("njt", &njt, PDI_OUT);
	
	PDI_expose("istart", &istart, PDI_OUT); /// offset
	PDI_expose("jstart", &jstart, PDI_OUT);
	
	// Fill arrays
	for (j = 0; j < nj + 2*njg ; ++j) {
		for (i = 0; i < ni + 2*nig; ++i) {
			cp_values[j][i] = icst;
			cp_reals[j][i]  = rcst; /// array initialized with const values
		}
	}
	/// Values and reals == 0 in the ghost.
	double cst = -rcst;
	for (j = njg; j < nj + njg ; ++j) {
		for (i = nig; i < ni + nig; ++i) {
			values[j][i]    = i + coord[1]*ni  -nig + (j+coord[0]*nj-njg)*10;
			reals[j][i]     = i*cst + coord[1]*ni - nig*cst + (j+coord[0]*nj-njg)*10.; /// array that contains data
		}
	}
	
	input = 0;
	///  Test that export/exchange works
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("reals", &reals, PDI_OUT);     // output real
	PDI_expose("values", &values, PDI_INOUT); // output integers
	
	PDI_transaction_begin("useless_name");
	PDI_expose("myrank", &rank, PDI_OUT);
	PDI_expose("time", &starting_time, PDI_OUT);
	PDI_transaction_end();
	
	input = 1;
	///  Import should also work
	PDI_expose("input", &input, PDI_OUT); // update metadata => HDF5 now import only
	PDI_expose("reals", &cp_reals, PDI_IN);     // input real
	PDI_expose("values", &cp_values, PDI_INOUT);  // input integers
	
	{
		/// Testing scalar import
		int test;
		double tmp;
		PDI_expose("time",&tmp, PDI_IN);
		PDI_expose("myrank",&test, PDI_IN);
		if ( (starting_time != tmp) || (test != rank)) {
			fprintf(stderr, "Float   : %6f vs %6f (out/in)\n", starting_time, tmp);
			fprintf(stderr, "Integer : %6d vs %6d (out/in)\n", rank, test);
			MPI_Abort(MPI_COMM_WORLD, -1);
		}
	}
	
	/// So the data should be the same
	fprintf(stderr, "Data exported | Data imported\n");
	
	for (int j = njg; j < nj+njg ; ++j) { // Should be the same inside
		for (int i = nig; i < ni + nig; i++) {
			if ((values[j][i] !=  cp_values[j][i])  || (reals[j][i] != cp_reals[j][i])) {
				fprintf(stderr,"Ghost: integer (export) / integer(imported) :: %3d  %3d\n", values[j][i], cp_values[j][i]);
				fprintf(stderr,"Ghost: reals   (export) / reals (imported) :: %6f  %6f\n", reals[j][i], cp_reals[j][i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	}
	for (int j = 0; j < njg; j++) { // and should be icst/rcst outside
		for (int i = 0; i < nig; i++) {
			if ((icst !=  cp_values[j][i])  || (rcst != cp_reals[j][i])) {
				fprintf(stderr,"Ghost: integer (export) / integer(imported) :: %3d  %3d\n", icst, cp_values[j][i]);
				fprintf(stderr,"Ghost: reals   (export) / reals (imported) :: %6f  %6f\n", rcst, cp_reals[j][i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
		for (int i = ni + nig; i < ni + 2 * nig ; ++i) {
			if ((icst !=  cp_values[j][i]) || (rcst != cp_reals[j][i])) {
				fprintf(stderr,"Ghost: integer (export) / integer(imported) :: %3d  %3d\n", icst, cp_values[j][i]);
				fprintf(stderr,"Ghost: reals   (export) / reals (imported) :: %6f  %6f\n", rcst, cp_reals[j][i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	}
	for (int j = nj+njg ; j < nj + 2*njg; ++j ) {
		for (int i = 0; i < nig; i++) {
			if ((icst !=  cp_values[j][i]) || (rcst != cp_reals[j][i])) {
				fprintf(stderr,"Ghost: integer (export) / integer(imported) :: %3d  %3d\n", icst, cp_values[j][i]);
				fprintf(stderr,"Ghost: reals   (export) / reals (imported) :: %6f  %6f\n", rcst, cp_reals[j][i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
		for (int i = ni + nig; i < ni + 2 * nig ; ++i) {
			if ((icst !=  cp_values[j][i]) || (rcst != cp_reals[j][i])) {
				fprintf(stderr,"Ghost: integer (export) / integer(imported) :: %3d  %3d\n", icst, cp_values[j][i]);
				fprintf(stderr,"Ghost: reals   (export) / reals (imported) :: %6f  %6f\n", rcst, cp_reals[j][i]);
				MPI_Abort(MPI_COMM_WORLD, -1);
			}
		}
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
	return 0;
}