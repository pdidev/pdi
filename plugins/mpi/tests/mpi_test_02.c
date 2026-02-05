/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <mpi.h>

#include <assert.h>

#include <paraconf.h>
#include <pdi.h>

const char* YAML_CONFIG
	= "logging: trace            \n"
	  "plugins:                  \n"
	  "  mpi:                    \n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	PDI_init(conf);
	MPI_Comm* comm;

	PDI_access("MPI_COMM_WORLD", (void**)&comm, PDI_IN);
	assert(MPI_COMM_WORLD == *comm);
	PDI_release("MPI_COMM_WORLD");

	PDI_access("MPI_COMM_SELF", (void**)&comm, PDI_IN);
	assert(MPI_COMM_SELF == *comm);
	PDI_release("MPI_COMM_SELF");

	PDI_access("MPI_COMM_NULL", (void**)&comm, PDI_IN);
	assert(MPI_COMM_NULL == *comm);
	PDI_release("MPI_COMM_NULL");

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
