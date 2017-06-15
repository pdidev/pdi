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

//The following is used for doxygen documentation:
/**
* \file data.c
* \brief Functions to handle PDI data.
* \author J. Bigot (CEA)
*/

#include "config.h"

#include "pdi/data.h"
#include "status.h"


PDI_status_t PDI_data_destroy(PDI_data_t *var)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_datatype_destroy(&var->type), err0);
	free(var->name);
	for (int ii = 0; ii < var->nb_content; ++ii) {
		if (var->content[ii].access & PDI_MM_FREE) {
			free(var->content[ii].data);
		}
	}
	free(var->content);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_data_copy(PDI_data_t *to, const PDI_data_t *from){
	PDI_status_t status = PDI_OK;

	to->name = strdup(from->name);
	to->kind = from->kind;
	
	/// The type of the data
	PDI_handle_err(PDI_datatype_copy(&to->type,&from->type), err0);

	/// A reference to the data configuration
	to->config = from->config;

	to->nb_content = from->nb_content;
	to->content = malloc(to->nb_content * sizeof(PDI_data_value_t));
	for (int ii = 0; ii < to->nb_content; ii++){
		from->content[ii].data = to->content[ii].data;
		from->content[ii].access = to->content[ii].access;
	}
	
	return status;

err0:
	free(to->name);
	to->name = NULL;
	return status;
}


