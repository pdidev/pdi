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

#include <string.h>

#include <pdi.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/datatype.h>

#include <fti.h>

int get_id(PDI_data_t *data)
{
	int id;
	PC_int(PC_get(data->config, ".id"), &id);
	return id;
}

PDI_status_t PDI_fti_plugin_init(PC_tree_t conf, MPI_Comm *world)
{
	char * fti_file; PC_string(PC_get(conf, ".config"), &fti_file);
	FTI_Init(fti_file, PDI_state.PDI_comm);
	free(fti_file);
	*world = FTI_COMM_WORLD;
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_finalize()
{
	FTI_Finalize();
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_event(const char *event)
{
	if( !strcmp(event, "Snapshot") ) {
		FTI_Snapshot();
	}
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_start(PDI_data_t *data)
{
	int size; PDI_data_size(&data->type, &size);
	//TODO: handle non-contiguous data correctly
	FTI_Protect(get_id(data), data->content.data, size, FTI_CHAR);
	return PDI_OK;
}

PDI_status_t PDI_fti_plugin_data_end(PDI_data_t *data)
{
	FTI_Protect(get_id(data), NULL, 0, FTI_CHAR);
	return PDI_OK;
}

PDI_PLUGIN(fti_plugin)
