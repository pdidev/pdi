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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <paraconf.h>
#include <pdi.h>

const char* YAML_CONFIG
	= "logging:                        \n"
	  "  level: trace                  \n"
	  "  output: 'mpi_master_only.log' \n"
	  "data: { test_var: double }      \n"
	  "plugins:                        \n"
	  "  mpi:                          \n"
	  "    logging:                    \n"
	  "      master_only: true         \n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	PC_tree_t conf = PC_parse_string(YAML_CONFIG);
	if (PDI_init(conf)) {
		fprintf(stderr, "*** Error in PDI_init\n");
		exit(EXIT_FAILURE);
	}

	double test_var = 1.0;
	PDI_expose("test_var", &test_var, PDI_OUT);
	PDI_finalize();
	PC_tree_destroy(&conf);

	MPI_Barrier(MPI_COMM_WORLD);

	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	if (world_rank == 0) {
		FILE* f = fopen("mpi_master_only.log", "r");
		if (!f) {
			fprintf(stderr, "*** Error: log file was not created\n");
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
		char buf[65536];
		size_t n = fread(buf, 1, sizeof(buf) - 1, f);
		buf[n] = '\0';
		fclose(f);
		if (n == 0) {
			fprintf(stderr, "*** Error: log file is empty\n");
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
		// non-master ranks are tagged "MPI 000001"/"MPI 000002" in the pattern;
		// none of their messages should have made it into the file
		if (strstr(buf, "MPI 000001") || strstr(buf, "MPI 000002")) {
			fprintf(stderr, "*** Error: log file contains output from a non-master rank\n");
			MPI_Abort(MPI_COMM_WORLD, 1);
		}
	}

	MPI_Finalize();
	return 0;
}
