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
	= "logging: trace                 \n"
	  "metadata:                      \n"
	  "  m_mpi_comm: MPI_Comm         \n"
	  "  m_mpi_comm_f: MPI_Comm_f     \n"
	  "data:                          \n"
	  "  mpi_comm: MPI_Comm           \n"
	  "  mpi_comm_f: MPI_Comm_f       \n"
	  "plugins:                       \n"
	  "  mpi:                         \n"
	  "    Comm_c2f:                  \n"
	  "      mpi_comm: m_mpi_comm_f   \n"
	  "    Comm_f2c:                  \n"
	  "      mpi_comm_f: m_mpi_comm   \n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	PDI_init(conf);

	//transtype MPI_Comm -> MPI_Comm_f
	MPI_Comm comm = MPI_COMM_WORLD;
	PDI_expose("mpi_comm", &comm, PDI_OUT);

	MPI_Fint* m_comm_f;
	PDI_access("m_mpi_comm_f", (void**)&m_comm_f, PDI_IN);
	int result;
	MPI_Comm_compare(comm, MPI_Comm_f2c(*m_comm_f), &result);
	assert(result == MPI_IDENT);
	PDI_release("m_mpi_comm_f");

	//transtype MPI_Comm_f -> MPI_Comm
	MPI_Fint comm_f = MPI_Comm_c2f(comm);
	PDI_expose("mpi_comm_f", &comm_f, PDI_OUT);

	MPI_Comm* m_comm;
	PDI_access("m_mpi_comm", (void**)&m_comm, PDI_IN);
	MPI_Comm_compare(MPI_Comm_f2c(comm_f), *m_comm, &result);
	assert(result == MPI_IDENT);
	PDI_release("m_mpi_comm");

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
