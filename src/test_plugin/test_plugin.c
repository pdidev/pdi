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
#include <pdi/state.h>

MPI_Comm my_world;

PDI_status_t PDI_test_plugin_init(yaml_document_t* document, const yaml_node_t *conf, MPI_Comm *world)
{
	my_world = *world;
	
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("Welcome to the test plugin!\n");
	}
	
	return PDI_OK;
}

PDI_status_t PDI_test_plugin_finalize()
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("Goodbye from the test plugin!\n");
	}
	
	return PDI_OK;
}

PDI_status_t PDI_test_plugin_event(const char *event)
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("test plugin got an event: %s!\n", event);
	}
	
	return PDI_OK;
}

PDI_status_t PDI_test_plugin_data_start(PDI_data_t *data, PDI_inout_t access)
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("data becoming available to the test plugin: %s!\n", data->name);
	}
	
	return PDI_OK;
}

PDI_status_t PDI_test_plugin_data_end(PDI_data_t *data)
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("data becoming unavailable to the test plugin: %s!\n", data->name);
	}
	
	return PDI_OK;
}

PDI_PLUGIN(test_plugin)
