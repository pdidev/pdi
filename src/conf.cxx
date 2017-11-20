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
#include "pdi/data_descriptor.h"
#include "pdi/datatype.h"

#include "status.h"

#include "conf.h"

static PDI_status_t load_data(PC_tree_t node, PDI_datakind_t kind)
{
	PDI_status_t status = PDI_OK;
	
	int map_len; handle_PC_err(PC_len(node, &map_len), err0);
	
	int map_id;
	PDI_datatype_t *type; type = nullptr;
	char *c_name; c_name = nullptr;
	for (map_id = 0; map_id < map_len; ++map_id) {
		PC_tree_t config;
		
		
		handle_PC_err(PC_string(PC_get(node, "{%d}", map_id), &c_name), err0);
		std::string name(c_name);
		free(c_name);
		
		config = PC_get(node, "<%d>", map_id);
		handle_PC_err(PC_status(config), err0);
		
		PDI_datatype_t type;
		PDI_handle_err(PDI_datatype_load(&type, config), err1);
		
		PDI::Data_descriptor cur_data;
		PDI_handle_err(cur_data.init(name, config, (kind == PDI_DK_METADATA), type), err2);
		
		PDI_state.descriptors.insert({name, cur_data});
	}
	
	return status;
err2:
	PDI_datatype_destroy(type);
err1:
	delete(type);
err0:
	return status;
}

PDI_status_t load_conf(PC_tree_t node)
{
	PDI_status_t status = PDI_OK;
	
	// Detect an invalid configuration as soon as possible
	handle_PC_err(PC_status(node), err0);
	
	// no metadata is not an error
	{
		PC_tree_t metadata = PC_get(node, ".metadata");
		if (!PC_status(metadata)) {
			PDI_handle_err(load_data(metadata, PDI_DK_METADATA), err0);
		}
	}
	
	// no data is spurious, but not an error
	{
		PC_tree_t data = PC_get(node, ".data");
		if (!PC_status(data)) {
			PDI_handle_err(load_data(data, PDI_DK_DATA), err0);
		}
	}
	
	return status;
	
err0:
	return status;
}
