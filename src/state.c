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

/**
\file api.c
\brief PDI internal data
\author J. Bigot (CEA)
**/

#include "config.h"

#include "pdi/state.h"
#include "status.h"
#include "pdi/plugin.h"


PDI_state_t PDI_state;


PDI_data_t *PDI_find_data(const char *name)
{
	PDI_data_t *data = NULL;
	for (int ii = 0; ii < PDI_state.nb_data; ++ii) {
		if (strcmp(PDI_state.data[ii].name, name)) continue;
		data = PDI_state.data + ii;
		break;
	}
	return data;
}


PDI_status_t PDI_data_unlink(PDI_data_t *data, int content_id)
{
	PDI_status_t status = PDI_OK;
	
	for (int ii = 0; ii < PDI_state.nb_plugins; ++ii) {
		PDI_handle_err(PDI_state.plugins[ii].data_end(data), err0);
	}
	
	if (data->content[content_id].access & PDI_MM_FREE) {
		free(data->content[content_id].data);
	}
	for (int ii = content_id; ii < data->nb_content - 1; ++ii) {
		data->content[ii] = data->content[ii + 1];
	}
	--data->nb_content;
	
	return status;
	
err0:
	return status;
}


