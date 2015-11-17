/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#include <pdi.h>
#include <pdi/plugin.h>


PDI_status_t PDI_hdf5_per_process_init(yaml_document_t* document, const yaml_node_t *conf, MPI_Comm *world)
{
	PDI_status_t err = PDI_OK;
	
	int rank; if (MPI_Comm_rank(*world, &rank)) { err = PDI_ERR_PLUGIN; goto init_err0; }
	
	if ( rank == 0 ) {
		printf("Welcome to the hdf5_per_process_init plugin!\n");
	}
	
init_err0:
	return PDI_OK;
}


PDI_status_t PDI_hdf5_per_process_finalize()
{
	return PDI_OK;
}

PDI_PLUGIN(hdf5_per_process)
