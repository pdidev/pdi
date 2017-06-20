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

	/// Create a temporary copy 
	PDI_data_t copy;
	copy.name = strdup(from->name);
	copy.kind = from->kind;
	
	/// A reference to the data configuration
	copy.config = from->config;

	/// Copy data content
	copy.nb_content = from->nb_content;
	copy.content = malloc(copy.nb_content * sizeof(PDI_data_value_t));
	for (int ii = 0; ii < copy.nb_content; ii++){
		from->content[ii].access = copy.content[ii].access;
		if (copy.content[ii].access & PDI_MM_FREE) {
			/// For data that own their buffer, allocate a distinct buffer
			size_t dsize; PDI_handle_err(PDI_datatype_buffersize(&(copy.type), &dsize), err0);
			void *newval; newval = malloc(dsize);
			PDI_handle_err(PDI_buffer_copy(
			                   newval,
			                   &(copy.type),
			                   from->content[ii].data,
			                   &(from->type)),
			               err2);
			copy.content[ii].data = newval;
err2:
			if(status){
				free(newval);
				PDI_handle_err(status,err0);
			}
		} else { /// else share the reference
			from->content[ii].data = copy.content[ii].data;
		}
	}

	// If everything succeed transfert data from 'copy' to the destination 'to'.
	// Copy the type of the data directly into the 'to'
	PDI_handle_err(PDI_datatype_copy(&to->type, &from->type), err0);
	// The other properties
	to->name       = copy.name;       
	to->kind       = copy.kind;       
	to->config     = copy.config;     
	to->nb_content = copy.nb_content; 
	to->content    = copy.content;    

	
	return status;

err0: /// In case of error, the original data 'to' is unchanged/unmodified.
	return status;
}

