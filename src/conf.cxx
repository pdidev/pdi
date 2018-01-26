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
* \author Julien Bigot (CEA) <julien.bigot@cea.fr>
*/

#include "config.h"

#include "pdi.h"
#include "pdi/data_type.h"
#include "pdi/data_descriptor.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/state.h"
#include "pdi/status.h"

#include "conf.h"

using namespace PDI;
using std::string;

static PDI_status_t load_data(PC_tree_t node, bool is_metadata)
{
	int map_len = len(node);
	
	for (int map_id = 0; map_id < map_len; ++map_id) {
		Data_descriptor& dsc = PDI_state.desc(to_string(PC_get(node, "{%d}", map_id)));
		dsc.metadata(is_metadata);
		PC_tree_t config = PC_get(node, "<%d>", map_id);
		dsc.creation_template(Data_type::load(config), config);
	}
	
	return PDI_OK;
}

PDI_status_t load_conf(PC_tree_t node)
{
	// no metadata is not an error
	{
		PC_tree_t metadata = PC_get(node, ".metadata");
		if (!PC_status(metadata)) {
			load_data(metadata, true);
		}
	}
	
	// no data is spurious, but not an error
	{
		PC_tree_t data = PC_get(node, ".data");
		if (!PC_status(data)) {
			load_data(data, false);
		}
	}
	
	return PDI_OK;
}
