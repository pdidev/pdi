/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Institut National de Recherche en Sciences et Technologies du Numérique (INRIA)
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
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

#include <unistd.h>

#define IMX 5

constexpr char CONFIG_FOR_WRITING[] = R"(
logging: debug
metadata:
  nn: int
  mpi_comm: MPI_Comm
  is_client: int
data:
  written_values: {size: ["$nn"], type: array, subtype: int}
plugins:
  trace: info
  mpi:
  damaris:
    architecture:
      sim_name: distinguish_data_and_dataset
      domains: 1
      dedicated:
        core: 1
        node: 0
    get_is_client: is_client
    client_comm_get: mpi_comm
    datasets:
      - dataset:
          name: written_values_ds
          layout: written_values_layout
          storage: hdf5_storage
    layouts:
      - layout:
          name: written_values_layout
          type: int
          global: ["$nn"]
          dimensions: ["$nn"]
          ghosts: '0:0'
          depends_on: [nn]
    storages:
      - storage:
          name: hdf5_storage
          type: HDF5
          file_mode: Collective
          files_path: ./HDF5_files/
    write:
      written_values:
        dataset: written_values_ds
        position: ['0']
    log:
      rotation_size: 5
      log_level: info
      flush: true
)";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	MPI_Comm main_comm = MPI_COMM_WORLD;
	int world_size;
	MPI_Comm_size(main_comm, &world_size);
	if (world_size != 2) {
		fprintf(stderr, "Please use at least 2 mpi processes\n");
		exit(1);
	}

	// initialize pdi
	PDI_init(PC_parse_string(CONFIG_FOR_WRITING));

	// All processes must initialize Damaris with the XML configuration
	//  - client process = heat simulation process
	//  - server process = damaris process for writting hdf5 file.

	int size = IMX;
	int written_values[size];

	for (int ii = 0; ii < size; ++ii) {
		written_values[ii] = 100 + ii;
	}
	int is_client = 0;
	PDI_expose("is_client", &is_client, PDI_INOUT); // The order doesn't care
	PDI_expose("mpi_comm", &main_comm, PDI_INOUT); // <-- allow plugin to set, returns Damaris client comm

	printf("value of is_client %d=", is_client);
	if (is_client) {
		PDI_multi_expose("write", "nn", &size, PDI_OUT, "written_values", written_values, PDI_OUT, NULL);
	}

	PDI_finalize();

	MPI_Finalize();

	return EXIT_SUCCESS;
}
