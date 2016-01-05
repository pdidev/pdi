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

static PDI_status_t load_metadata_item(yaml_document_t *document, yaml_node_t *node, PDI_metadata_t *data)
{
	PDI_status_t res = PDI_OK;
	if ( node->type != YAML_MAPPING_NODE ) return PDI_ERR_CONFIG;
	data->value = NULL;
	yaml_node_pair_t *pair;
	for ( pair = node->data.mapping.pairs.start; pair < node->data.mapping.pairs.top; ++pair ) {
		PC_tree_t key_tree = {document, yaml_document_get_node(document, pair->key)};
		char *key = NULL;
		if ( PC_get_string(key_tree, "", &key, NULL) ) {
			free(key);
			return PDI_ERR_CONFIG;
		}
		if ( !strcmp(key, "name") ) {
			free(key);
			PC_tree_t value_tree = {document, yaml_document_get_node(document, pair->value)};
			data->name = NULL;
			if ( PC_get_string(value_tree, "", &data->name, NULL) ) {
				free(data->name);
				return PDI_ERR_CONFIG;
			}
		} else if ( !strcmp(key, "type") ) {
			free(key);
			PC_tree_t value_tree = {document, yaml_document_get_node(document, pair->value)};
			data->type = malloc(sizeof(PDI_type_t));
			res = PDI_datatype_load(value_tree, data->type);
			if ( res ) {
				free(data->type);
				return res;
			}
		}
		free(key);
	}
	return res;
}

static PDI_status_t load_metadata(yaml_document_t *document, yaml_node_t *node)
{
	PDI_status_t res = PDI_OK;
	if ( node->type != YAML_SEQUENCE_NODE ) return PDI_ERR_CONFIG;
	PDI_state.metadata = realloc(
			PDI_state.metadata,
			( PDI_state.nb_metadata
				+ node->data.sequence.items.top
				- node->data.sequence.items.start )
				* sizeof(PDI_metadata_t)
	);
	yaml_node_item_t *metadata;
	for (
			metadata = node->data.sequence.items.start;
			metadata < node->data.sequence.items.top;
			++metadata
	) {
		
		res = load_metadata_item(
			document,
			yaml_document_get_node(document, *metadata),
			PDI_state.metadata+PDI_state.nb_metadata);
		if ( res ) return res;
		++PDI_state.nb_metadata;
	}
	return res;
}

static PDI_status_t load_data(PC_tree_t node)
{
	PDI_status_t res = PDI_OK;
	if ( node.node->type != YAML_SEQUENCE_NODE ) return PDI_ERR_CONFIG;
	//TODO: implement
	return res;
}

PDI_status_t load_conf(PC_tree_t node)
{
	PDI_status_t res = PDI_OK;
	yaml_node_pair_t *pair;
	if ( node.node->type != YAML_MAPPING_NODE ) return PDI_ERR_CONFIG;
	for ( pair = node.node->data.mapping.pairs.start; pair < node.node->data.mapping.pairs.top; ++pair ) {
		char *key = NULL;
		PC_tree_t key_tree = {node.document, yaml_document_get_node(node.document, pair->key)};
		if ( PC_get_string(key_tree, "", &key, NULL) ) {
			free(key);
			return PDI_ERR_CONFIG;
		}
		if ( !strcmp(key, "metadata") ) {
			free(key);
			res = load_metadata(node.document, yaml_document_get_node(node.document, pair->value));
			if ( res ) return res;
		} else if ( !strcmp(key, "data") ) {
			free(key);
			PC_tree_t value_tree = {node.document, yaml_document_get_node(node.document, pair->value)};
			res = load_data(value_tree);
			if ( res ) return res;
		}
	}
	return res;
}
