/*******************************************************************************
 * Copyright (C) 2026 Centre National de la Recherche Scientifique (CNRS)
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
  arr_size: int
  comm: MPI_Comm
data:
  cp_status: int
  cp_counter: int
  arr:  {type: array, size: '$arr_size', subtype: double}
plugins:
  mpi:	
  veloc:
    config_file: veloc_config.cfg
    communicator: comm
    status: cp_status
    counter: cp_counter
    checkpoint_label: managed_test_sync
    iteration: ii
    managed_checkpointing:
      protect_data: [ii, arr]
      synchronize_on_event : sync
      when: '$ii % 2 = 0 '
)";
 
int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	int rank, size;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
 
	if (size != 3) {
		if (rank == 0) fprintf(stderr, "veloc_test_managed_comm must be run with 3 processes (got %d)\n", size);
		MPI_Abort(MPI_COMM_WORLD, 1);
	}
 
	// put only rank O and 1 in a communicator 
	const bool in_veloc = rank < 2;
	MPI_Comm comm;
	MPI_Comm_split(MPI_COMM_WORLD, in_veloc ? 0 : MPI_UNDEFINED, rank, &comm);
 
	PC_tree_t conf = PC_parse_string(CONF_YAML);
	PDI_init(conf);
 
	PDI_expose("comm", &comm, PDI_OUT);
 
	int cp_counter;
	int cp_status;
 
	int ii = 0;
	int arr_size = 20;
 
	PDI_expose("arr_size", &arr_size, PDI_OUT);
 
	double* arr = new double[arr_size];
	for (int i = 0; i < arr_size; i++) {
		arr[i] = 0.0;
	}
 
	// write checkpoints 
	for (; ii < 5; ++ii) {
		arr[ii] = ii * 2;
		PDI_multi_expose("sync", "ii", &ii, PDI_INOUT, "arr", arr, PDI_INOUT, NULL);
	}

	cp_status = 0;
	// write status to "recovery needed"
	PDI_expose("cp_status", &cp_status, PDI_OUT);

	// recover and keep writing checkpoints
	for (ii = 0; ii < 10; ++ii) {
		arr[ii] = ii * 2;
		PDI_multi_expose("sync", "ii", &ii, PDI_INOUT, "arr", arr, PDI_INOUT, NULL);
	}
 
	// read checkpoint counter
	PDI_expose("cp_counter", &cp_counter, PDI_IN);
 
	const int expected = in_veloc ?  5 : 0;
	if (cp_counter != expected){
		fprintf(stderr, "Rank %d veloc_test_with_comm:: counter value  %d does not match expected value %d\n", rank, cp_counter, expected);
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	// collective check over MPI_COMM_WORLD: only the 2 ranks in veloc comm wrote checkpoints
	int total_cp = 0;
	MPI_Allreduce(&cp_counter, &total_cp, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
	if (total_cp != 2 * 5) {
		fprintf(stderr, "Rank %d veloc_test_with_comm:: counter value  %d does not match expected value %d\n", rank, total_cp, 5*2);
		MPI_Abort(MPI_COMM_WORLD, 1);
	}

	if (rank == 0) printf("veloc_test_managed_comm PASSED\n");
 
	PDI_finalize();
	delete[] arr;
	MPI_Finalize();
 
	return 0;
}
 