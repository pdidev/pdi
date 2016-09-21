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
 * \file conf.c
 * \brief Functions to load data and metadata 
 * \author J. Bigot (CEA)
 */

#include <paraconf.h>

#include "pdi.h"
#include "pdi/state.h"
#include "pdi/datatype.h"

#include "status.h"

#include "conf.h"

static PDI_status_t load_metadata(PC_tree_t node)
{
	PDI_status_t status = PDI_OK;
	
	int map_len; handle_PC_err(PC_len(node, &map_len), err0);
	
	PDI_state.metadata = realloc(PDI_state.metadata,
			( PDI_state.nb_metadata + map_len ) * sizeof(PDI_metadata_t)
		);
	
	int map_id;
	for ( map_id=0; map_id<map_len; ++map_id ) {
		PDI_metadata_t *cur_meta = PDI_state.metadata+PDI_state.nb_metadata;
		
		cur_meta->value = NULL;
		
		handle_PC_err(PC_string(PC_get(node, "{%d}", map_id), &cur_meta->name), err0);
		
		PC_tree_t map_value = PC_get(node, "<%d>", map_id);
		handle_PC_err(PC_status(map_value), err0);
		handle_error(PDI_datatype_load(map_value, &cur_meta->type), err0);
		
		++PDI_state.nb_metadata;
	}
	
err0:
	return status;
}

static PDI_status_t load_data(PC_tree_t node)
{
	PDI_status_t status = PDI_OK;
	
	int map_len; handle_PC_err(PC_len(node, &map_len), err0);
	
	PDI_state.data = realloc(PDI_state.data,
			( PDI_state.nb_data + map_len ) * sizeof(PDI_data_t)
		);
	
	int map_id;
	for ( map_id=0; map_id<map_len; ++map_id ) {
		PDI_data_t *cur_var = PDI_state.data+PDI_state.nb_data;
		
		handle_PC_err(PC_string(PC_get(node, "{%d}", map_id), &cur_var->name), err0);
		
		cur_var->content.access = 0;
		
		cur_var->config = PC_get(node, "<%d>", map_id);
		handle_PC_err(PC_status(cur_var->config), err0);
		handle_error(PDI_datatype_load(cur_var->config, &cur_var->type), err0);
		
		
		++PDI_state.nb_data;
	}
	
err0:
	return status;
}

PDI_status_t load_conf(PC_tree_t node)
{
	PDI_status_t status = PDI_OK;
	
	// Detect an invalid configuration as soon as possible
	handle_PC_err(PC_status(node), err0);
	
	PC_tree_t metadata = PC_get(node, ".metadata");
	handle_PC_err(PC_status(metadata), err0);
	handle_error(load_metadata(metadata), err0);
	
	PC_tree_t data = PC_get(node, ".data");
	handle_PC_err(PC_status(data), err0);
	handle_error(load_data(data), err0);
	
err0:
	return status;
}
