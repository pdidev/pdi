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

#include <paraconf.h>

#include "pdi.h"
#include "pdi/state.h"
#include "pdi/datatype.h"

#include "conf.h"

static PDI_status_t load_metadata(PC_tree_t node)
{
	PDI_status_t res = PDI_OK;
	if ( node.node->type != YAML_MAPPING_NODE ) return PDI_ERR_CONFIG;
	
	PC_status_t pc_st;
	int map_len; pc_st = PC_len(node, &map_len);
	if (pc_st.code) return PDI_ERR_CONFIG;
	
	PDI_state.metadata = realloc(PDI_state.metadata,
			( PDI_state.nb_metadata + map_len ) * sizeof(PDI_metadata_t)
		);
	
	int map_id;
	for ( map_id=0; map_id<map_len; ++map_id ) {
		PDI_metadata_t *cur_meta = PDI_state.metadata+PDI_state.nb_metadata;
		
		cur_meta->value = NULL;
		
		pc_st = PC_string(PC_get(node, "{%d}", map_id), &cur_meta->name); if ( pc_st.code ) return PDI_ERR_CONFIG;
		
		PC_tree_t map_value = PC_get(node, "<%d>", map_id);  if (pc_st.code) return PDI_ERR_CONFIG;
		res = PDI_datatype_load(map_value, &cur_meta->type); if (res) return res;
		
		++PDI_state.nb_metadata;
	}
	return res;
}

static PDI_status_t load_data(PC_tree_t node)
{
	PDI_status_t res = PDI_OK;
	if ( node.node->type != YAML_MAPPING_NODE ) return PDI_ERR_CONFIG;
	
	int map_len; if ( PC_len(node, &map_len).code ) return PDI_ERR_CONFIG;
	
	PDI_state.data = realloc(PDI_state.data,
			( PDI_state.nb_data + map_len ) * sizeof(PDI_data_t)
		);
	
	int map_id;
	for ( map_id=0; map_id<map_len; ++map_id ) {
		PDI_data_t *cur_dat = PDI_state.data+PDI_state.nb_data;
		
		if ( PC_string(PC_get(node, "{%d}", map_id), &cur_dat->name).code ) return PDI_ERR_CONFIG;
		cur_dat->content.memstatus = PDI_UNALOCATED;
		
		PC_tree_t map_value = PC_get(node, "<%d>.slice_type", map_id);
		if ( PC_status(map_value) ) return PDI_ERR_CONFIG;
		res = PDI_datatype_load(map_value, &cur_dat->slice_type); if (res) return res;
		
		map_value = PC_get(node, "<%d>.mem_type", map_id);
		if ( PC_status(map_value) ) return PDI_ERR_CONFIG;
		res = PDI_datatype_load(map_value, &cur_dat->mem_type); if (res) return res;
		
		++PDI_state.nb_data;
	}
	return res;
}

PDI_status_t load_conf(PC_tree_t node)
{
	PDI_status_t res = PDI_OK;
	if ( node.node->type != YAML_MAPPING_NODE ) return PDI_ERR_CONFIG;
	
	PC_tree_t metadata = PC_get(node, ".metadata");
	if ( PC_status(metadata) ) return PDI_ERR_CONFIG;
	res = load_metadata(metadata); if ( res ) return res;
	
	PC_tree_t data = PC_get(node, ".data");
	if ( PC_status(data) ) return PDI_ERR_CONFIG;
	res = load_data(data); if ( res ) return res;
	
	return res;
}
