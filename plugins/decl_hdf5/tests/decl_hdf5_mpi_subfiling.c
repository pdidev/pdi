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
#include <glob.h>
#include <unistd.h>
#include <pdi.h>

#define IMX 50
#define JMX 40
#define NI_GHOST 1
#define NJ_GHOST 2
#define DIM 2

const char* CONFIG_YAML
	= "logging: trace                                                   \n"
	  "metadata:                                                        \n"
	  "  input: int                                                     \n"
	  "  ni: int                                                        \n"
	  "  nj: int                                                        \n"
	  "  nig: int                                                       \n"
	  "  njg: int                                                       \n"
	  "  nit: int                                                       \n"
	  "  njt: int                                                       \n"
	  "  istart: int                                                    \n"
	  "  jstart: int                                                    \n"
	  "data:                                                            \n"
	  "  reals:                                                         \n"
	  "    type: array                                                  \n"
	  "    subtype: double                                              \n"
	  "    size: [$nj + 2*$njg, $ni + 2*$nig]                           \n"
	  "    subsize: [$nj, $ni]                                          \n"
	  "    start: [$njg, $nig]                                          \n"
	  "  values:                                                        \n"
	  "    type: array                                                  \n"
	  "    subtype: int                                                 \n"
	  "    size: [$nj + 2*$njg, $ni + 2*$nig]                           \n"
	  "    subsize: [$nj, $ni]                                          \n"
	  "    start: [$njg, $nig]                                          \n"
	  "plugins:                                                         \n"
	  "  mpi:                                                           \n"
	  "  decl_hdf5:                                                     \n"
	  "    file: subfiling.h5                                           \n"
	  "    communicator: $MPI_COMM_WORLD                                \n"
	  "    subfiling: true                                              \n"
	  "    datasets:                                                    \n"
	  "      reals:  {type: array, subtype: double, size: [$njt, $nit]} \n"
	  "      values: {type: array, subtype: int, size: [$njt, $nit]}    \n"
	  "    write:                                                       \n"
	  "      reals:                                                     \n"
	  "        when: $input=0                                           \n"
	  "        dataset_selection: {start: [$jstart, $istart]}           \n"
	  "      values:                                                    \n"
	  "        when: $input=0                                           \n"
	  "        dataset_selection: {start: [$jstart, $istart]}           \n";

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
	dims[0] = 2;
	dims[1] = 2;


	int provided;
	MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);

	if (provided != MPI_THREAD_MULTIPLE) {
		printf("provided level = %d, required level = %d\n", provided, MPI_THREAD_MULTIPLE);
		return -1;
	}
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf);
	int rank;
	MPI_Comm_rank(world, &rank);

	if (0 == rank) {
		remove("subfiling.h5");
	}

	{
		/// setting nb of procs.
		int size;
		MPI_Comm_size(world, &size);
		if (size != 4) {
			printf("Run on 4 procs only.");
			MPI_Abort(MPI_COMM_WORLD, -1);
		}
		PDI_expose("nproc", &size, PDI_OUT);
	}


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

	input = 0;
	PDI_expose("rank", &rank, PDI_OUT);
	PDI_expose("input", &input, PDI_OUT);

	///  Test that export/exchange works
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("reals", &reals, PDI_OUT); // output real
	PDI_expose("values", &values, PDI_INOUT); // output integers

	if (rank == 0) {
		glob_t results;
		int ret = glob("subfiling.h5.subfile_*", 0, NULL, &results);

		if (!ret) {
			printf("Found %zu file(s) matching the pattern:\n", results.gl_pathc);
			for (size_t i = 0; i < results.gl_pathc; i++) {
				printf(" - %s\n", results.gl_pathv[i]);
			}
		} else if (ret == GLOB_NOMATCH) {
			printf("No files found matching the pattern.\n");
		} else {
			printf("An error occurred during globbing.\n");
		}

		globfree(&results);
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
