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
#include <fti.h>
#include <string.h>

MPI_Comm my_world;

PDI_status_t PDI_fti_init(PC_tree_t conf, MPI_Comm *world)
{
	my_world = *world;

	char * fti_file;

	PC_string(PC_get(conf, ".config"), &fti_file);

	FTI_Init(fti_file,my_world);

	free(fti_file);
	
	return PDI_OK;
}

PDI_status_t PDI_fti_finalize()
{
	FTI_finalize();
	return PDI_OK;
}

PDI_status_t PDI_fti_event(const char *event)
{
	
	if(strcmp(event,"Snapshot")==0)
	{
		FTI_Snapshot();
	}
	
	return PDI_OK;
}

PDI_status_t PDI_fti_data_start(PDI_variable_t *data)
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf(">> data becoming available to the test plugin: %s!\n", data->name);
	}

	FTI_Protect();
	
	return PDI_OK;
}

PDI_status_t PDI_fti_data_end(PDI_variable_t *data)
{
	int rank; if (MPI_Comm_rank(my_world, &rank)) return PDI_ERR_PLUGIN;
	
	if ( rank == 0 ) {
		printf("<< data becoming unavailable to the test plugin: %s!\n", data->name);
	}
	
	return PDI_OK;
}

PDI_PLUGIN(fti)
