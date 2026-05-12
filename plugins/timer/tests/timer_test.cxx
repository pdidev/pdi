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

#include <mpi.h>
#include <cstdlib>
#include <pdi.h>

#define IMX 50
#define JMX 40
#define NI_GHOST 1
#define NJ_GHOST 2
#define DIM 2

constexpr char CONFIG_YAML[] = R"(
logging: debug
metadata:
  input: int
  ni: int
  nj: int
  nig: int
  njg: int
  nit: int
  njt: int
  istart: int
  jstart: int
data:
  reals:
    type: array
    subtype: double
    size: [$nj + 2*$njg, $ni + 2*$nig]
    subsize: [$nj, $ni]
    start: [$njg, $nig]
  values:
    type: array
    subtype: int
    size: [$nj + 2*$njg, $ni + 2*$nig]
    subsize: [$nj, $ni]
    start: [$njg, $nig]
plugins:
  mpi:
  timer: 
    - timer_AAA: {start: "decl_hdf5_start_timer", stop: "decl_hdf5_stop_timer"}
    - timer_BBB: "decl_hdf5"
    - timer_CCC: [decl_hdf5, event_toto]
    - timer_DDD: 
        start: "event_toto_start_timer"
        stop: "event_toto_stop_timer"

  decl_hdf5:
    - file: output_reals.h5
      communicator: $MPI_COMM_WORLD
      datasets:
        reals: {type: array, subtype: double, size: [$njt, $nit]}
      write:
        reals:
          dataset_selection: {start: [$jstart, $istart]}
    - file: output_values.h5
      communicator: $MPI_COMM_WORLD
      datasets:
        values: {type: array, subtype: int, size: [$njt, $nit]}
      write:
        values:
          dataset_selection: {start: [$jstart, $istart]}
)";

int main(int argc, char* argv[])
{
	const int icst = -1; /// constants values in the ghost nodes
	const double rcst = -1.01;

	int nig = NI_GHOST, njg = NJ_GHOST;
	int ni = IMX, nj = JMX;
	int values[JMX + 2 * NJ_GHOST][IMX + NI_GHOST * 2] = {{0}}, cp_values[JMX + 2 * NJ_GHOST][IMX + NI_GHOST * 2] = {{0}};
	double reals[JMX + 2 * NJ_GHOST][IMX + NI_GHOST * 2] = {{0}}, cp_reals[JMX + 2 * NJ_GHOST][IMX + NI_GHOST * 2] = {{0}};
	int i, j, input;
	int nit, njt;

	/// MPI and parallel data or info
	int dims[DIM], coord[DIM], periodic[DIM];
	int istart, jstart;
	MPI_Comm comm2D;
	periodic[0] = 0;
	periodic[1] = 0;
	dims[0] = 1;
	dims[1] = 1;

	int provided;
	MPI_Init(&argc, &argv);

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;

	PDI_init(conf);

	int rank;
	MPI_Comm_rank(world, &rank);
	int size;
	MPI_Comm_size(world, &size);

	MPI_Cart_create(world, DIM, dims, periodic, 0, &comm2D);
	MPI_Cart_coords(comm2D, rank, DIM, coord);

	istart = coord[1] * ni;
	jstart = coord[0] * nj;

	nit = 2 * ni;
	njt = 2 * nj;

	PDI_expose("nig", &nig, PDI_OUT); /// Ghost cells
	PDI_expose("njg", &njg, PDI_OUT);

	PDI_expose("ni", &ni, PDI_OUT); /// Size of the portion of the array for a given MPI task
	PDI_expose("nj", &nj, PDI_OUT);

	PDI_expose("nit", &nit, PDI_OUT); ///  size of the distributed array
	PDI_expose("njt", &njt, PDI_OUT);

	PDI_expose("istart", &istart, PDI_OUT); /// offset
	PDI_expose("jstart", &jstart, PDI_OUT);

	// Fill arrays
	for (j = 0; j < nj + 2 * njg; ++j) {
		for (i = 0; i < ni + 2 * nig; ++i) {
			cp_values[j][i] = icst;
			cp_reals[j][i] = rcst; /// array initialized with const values
		}
	}
	/// Values and reals == 0 in the ghost.
	double cst = -rcst;
	for (j = njg; j < nj + njg; ++j) {
		for (i = nig; i < ni + nig; ++i) {
			values[j][i] = (i + coord[1] * ni - nig) + (j + coord[0] * nj - njg) * 10;
			reals[j][i] = (i + coord[1] * ni - nig) * cst + (j + coord[0] * nj - njg) * 10 * cst;
		}
	}

	PDI_expose("rank", &rank, PDI_OUT);
	PDI_expose("input", &input, PDI_OUT);

	///  Test that export/exchange works
	PDI_multi_expose("", "reals", &reals, PDI_OUT, "values", &values, PDI_INOUT, NULL);

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
