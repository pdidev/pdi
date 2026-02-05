/*
 * SPDX-FileCopyrightText: 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
*
* description:
*
* In this test, we check that an element of datasets define as a regex
* allow to write a global data that verify these conditions:
*  - The global data is writting in one hdf5 file.
*  - The local data of a given mpi rank is writting in the global data using 'dataset_selection'
*  - The global data is saved in a datagroup that depends on 'iter_saved':
*   	- dataset: timestep${iter_saved:05}/reals
*/


#include <mpi.h>
#include <paraconf.h>
#include <time.h>
#include <unistd.h>
#include <pdi.h>

#define IMX 5
#define JMX 4
#define DIM 2


const char* CONFIG_YAML
	= "logging: trace                                                                \n"
	  "metadata:                                                                     \n"
	  "  input: int                                                                  \n"
	  "  ni: int                                                                     \n"
	  "  nj: int                                                                     \n"
	  "  nit: int                                                                    \n"
	  "  njt: int                                                                    \n"
	  "  istart: int                                                                 \n"
	  "  jstart: int                                                                 \n"
	  "  rank: int                                                                   \n"
	  "  nproc: int                                                                  \n"
	  "  iter_saved: int                                                             \n"
	  "data:                                                                         \n"
	  "  reals:                                                                      \n"
	  "    type: array                                                               \n"
	  "    subtype: double                                                           \n"
	  "    size: [$nj , $ni]                                                         \n"
	  "  values:                                                                     \n"
	  "    type: array                                                               \n"
	  "    subtype: int                                                              \n"
	  "    size: [$nj , $ni]                                                         \n"
	  "plugins:                                                                      \n"
	  "  mpi:                                                                        \n"
	  "  decl_hdf5:                                                                  \n"
	  "   - file: decl_hdf5_mpi_test_08_C.h5                                         \n"
	  "     communicator: $MPI_COMM_WORLD                                            \n"
	  "     collision_policy: replace_and_warn                                       \n"
	  "     on_event: write                                                          \n"
	  "     datasets:                                                                \n"
	  "       timestep.*/reals: {type: array, subtype: double, size: [$njt, $nit]}   \n"
	  "       timestep[0-9]+/values: {type: array, subtype: int, size: [$njt, $nit]} \n"
	  "     write:                                                                   \n"
	  "       reals:                                                                 \n"
	  "         when: $input=0                                                       \n"
	  "         dataset_selection: {start: [$jstart, $istart]}                       \n"
	  "         dataset: timestep${iter_saved:05}/reals                              \n"
	  "       values:                                                                \n"
	  "         when: $input=0                                                       \n"
	  "         dataset_selection: {start: [$jstart, $istart]}                       \n"
	  "         dataset: timestep${iter_saved:05}/values                             \n"
	  "   - file: decl_hdf5_mpi_test_08_C.h5                                         \n"
	  "     communicator: $MPI_COMM_WORLD                                            \n"
	  "     on_event: read                                                           \n"
	  "     datasets:                                                                \n"
	  "       timestep.*/reals: {type: array, subtype: double, size: [$njt, $nit]}   \n"
	  "       timestep[0-9]+values: {type: array, subtype: int, size: [$njt, $nit]}  \n"
	  "     read:                                                                    \n"
	  "       reals:                                                                 \n"
	  "         when: $input=1                                                       \n"
	  "         dataset_selection: {start: [$jstart, $istart]}                       \n"
	  "         dataset: timestep${iter_saved:05}/reals                              \n"
	  "       values:                                                                \n"
	  "         when: $input=1                                                       \n"
	  "         dataset_selection: {start: [$jstart, $istart]}                       \n"
	  "         dataset: timestep${iter_saved:05}/values                             \n";

int main(int argc, char* argv[])
{
	const int icst = -1; /// constants values in the ghost nodes
	const double rcst = -1.1;

	int ni = IMX, nj = JMX;
	int values[JMX][IMX] = {{0}}, cp_values[JMX][IMX] = {{0}};
	double reals[JMX][IMX] = {{0}}, cp_reals[JMX][IMX] = {{0}};
	int i, j, input;
	int nit, njt;
	int niterations = 4;

	/// MPI and parallel data or info
	int dims[DIM], coord[DIM], periodic[DIM];
	int istart, jstart;
	MPI_Comm comm2D;
	periodic[0] = 0;
	periodic[1] = 0;
	dims[0] = 2;
	dims[1] = 2;


	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	MPI_Comm world = MPI_COMM_WORLD;
	PDI_init(conf);
	int rank;
	MPI_Comm_rank(world, &rank);

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
	PDI_expose("rank", &rank, PDI_OUT);
	input = 0;
	PDI_expose("input", &input, PDI_OUT);

	MPI_Cart_create(world, DIM, dims, periodic, 0, &comm2D);
	MPI_Cart_coords(comm2D, rank, DIM, coord);

	istart = coord[1] * ni;
	jstart = coord[0] * nj;

	nit = 2 * ni;
	njt = 2 * nj;

	PDI_expose("ni", &ni, PDI_OUT); /// Size of the portion of the array for a given MPI task
	PDI_expose("nj", &nj, PDI_OUT);

	PDI_expose("nit", &nit, PDI_OUT); ///  size of the distributed array
	PDI_expose("njt", &njt, PDI_OUT);

	PDI_expose("istart", &istart, PDI_OUT); /// offset
	PDI_expose("jstart", &jstart, PDI_OUT);

	for (int iteration = 0; iteration < niterations; ++iteration) {
		PDI_expose("iter_saved", &iteration, PDI_INOUT); // save all iteration
		// Fill arrays
		for (j = 0; j < nj; ++j) {
			for (i = 0; i < ni; ++i) {
				cp_values[j][i] = icst;
				cp_reals[j][i] = rcst; /// array initialized with const values
				values[j][i] = iteration;
				reals[j][i] = (double)iteration;
			}
		}

		input = 0;
		///  Test that export/exchange works
		PDI_expose("input", &input, PDI_OUT);
		PDI_multi_expose(
			"write",
			"reals",
			&reals,
			PDI_OUT, // output real
			"values",
			&values,
			PDI_OUT, // output integers
			NULL
		);

		input = 1;
		///  Import should also work
		PDI_expose("input", &input, PDI_OUT); // update metadata => HDF5 now import only
		PDI_multi_expose(
			"read",
			"reals",
			&cp_reals,
			PDI_INOUT, // input real
			"values",
			&cp_values,
			PDI_INOUT, // input integers
			NULL
		);

		/// So the data should be the same
		fprintf(stderr, "Data exported | Data imported\n");

		for (int j = 0; j < nj; ++j) { // Should be the same inside
			for (int i = 0; i < ni; i++) {
				if ((values[j][i] != cp_values[j][i]) || (reals[j][i] != cp_reals[j][i])) {
					fprintf(stderr, "integer (export) / integer(imported) :: %3d  %3d\n", values[j][i], cp_values[j][i]);
					fprintf(stderr, "reals   (export) / reals (imported) :: %6f  %6f\n", reals[j][i], cp_reals[j][i]);
					MPI_Abort(MPI_COMM_WORLD, -1);
				}
			}
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	MPI_Finalize();
}
