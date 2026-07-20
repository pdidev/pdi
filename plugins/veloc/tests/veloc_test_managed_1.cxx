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
#include <pdi.h>

constexpr char CONF_YAML[] = R"(
metadata: 
  ii: int
data:
  cp_status: int
  cp_counter: int
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    status: cp_status
    counter: cp_counter
    checkpoint_label: managed_test_series
    iteration: ii
    managed_checkpointing:
      protect_data: [ii, var]
      checkpoint_on_event : ckp
      recover_on_event: recover
)";
	  

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	int rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	
	PC_tree_t conf = PC_parse_string(CONF_YAML);
	PDI_init(conf);

	int cp_status;
	int cp_counter;

	int ii = 0;
	int var = 50;

	/* Checkpoint Part */

	// read default status 
	PDI_expose("cp_status", &cp_status, PDI_IN);

	if (cp_status != 1) {
		fprintf(stderr, "Rank %d veloc_test_managed_1 FAILED status value %d does not match expected value %d\n", rank, cp_status, 1);
		exit(1);
	}

	// write 2 checkpoints
	for (; ii < 2; ii++) {
		var = var + 1, PDI_multi_expose("ckp", "ii", &ii, PDI_INOUT, "var", &var, PDI_INOUT, NULL);
	}

	// read checkpoint counter 
	PDI_expose("cp_counter", &cp_counter, PDI_IN);

	if (cp_counter != 2) {
		fprintf(stderr, "Rank %d veloc_test_managed_1 FAILED counter value %d does not match expected value %d\n", rank, cp_counter, 2);
		exit(1);
	}

	/* Recovery Part */

	cp_status = 0;
	// write status to "recovery needed"
	PDI_expose("cp_status", &cp_status, PDI_OUT);
	
	ii = -1;
	var = -1;
	// recover latest checkpoint 
	PDI_multi_expose("recover", "ii", &ii, PDI_INOUT, "var", &var, PDI_INOUT, NULL);

	PDI_expose("cp_status", &cp_status, PDI_IN);

	if (cp_status != 1) {
		fprintf(stderr, "Rank %d :veloc_test_managed_1 FAILED status value %d does not match expected value %d\n", rank, cp_status, 1);
		exit(1);
	}

	if (ii != 1) {
		fprintf(stderr, "Rank %d : veloc_test_managed_1 FAILED recovered iter value %d does not match expected value %d\n", rank, ii, 1);
		exit(1);
	}

	if (var != 52) {
		fprintf(stderr, "Rank %d : veloc_test_managed_1 FAILED recovered var value %d does not match expected value %d\n", rank, var, 52);
		exit(1);
	}

	if(rank == 0){
		printf("veloc_test_managed_1 PASSED\n");
	}

	PDI_finalize();
	MPI_Finalize();

	return 0;
}
