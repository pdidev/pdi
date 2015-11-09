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
#include "pdi_state.h"

#include "conf.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256

static PDI_status_t load_datatype(yaml_document_t *document, yaml_node_t *node, PDI_type_t *type);

static PDI_status_t load_subarray(yaml_document_t *document, yaml_node_t *node, array_type_t *type)
{
	PDI_status_t res = PDI_OK;
	res = PC_get_int(document, node, ".ndims", &type->ndims); if ( res ) return res;
	
	int len;
	res = PC_get_len(document, node, ".array_of_sizes", &len);
	if ( len != type->ndims ) return PDI_CONFIG_ERROR;
	res = PC_get_len(document, node, ".array_of_subsizes", &len);
	if ( len != type->ndims ) return PDI_CONFIG_ERROR;
	res = PC_get_len(document, node, ".array_of_starts", &len);
	if ( len != type->ndims ) return PDI_CONFIG_ERROR;
	
	int ii;
	type->array_of_sizes = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_sizes[%d]", ii);
		char *expr;
		res = PC_get_string(document, node, idx, &expr); if ( res ) return res;
		res = PDI_value(expr, &type->array_of_sizes[ii]); if ( res ) return res;
	}
	
	type->array_of_subsizes = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_subsizes[%d]", ii);
		char *expr;
		res = PC_get_string(document, node, idx, &expr); if ( res ) return res;
		res = PDI_value(expr, &type->array_of_subsizes[ii]); if ( res ) return res;
	}
	
	type->array_of_starts = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_starts[%d]", ii);
		char *expr;
		res = PC_get_string(document, node, idx, &expr); if ( res ) return res;
		res = PDI_value(expr, &type->array_of_starts[ii]); if ( res ) return res;
	}
	
	char *order_str;
	res = PC_get_string(document, node, ".order", &order_str); if ( res ) return res;
	if ( !strcmp(order_str, "ORDER_C") ) {
		type->order = ORDER_C;
	} else if ( !strcmp(order_str, "ORDER_FORTRAN") ) {
		type->order = ORDER_FORTRAN;
	} else {
		return PDI_CONFIG_ERROR;
	}
	
	yaml_node_t *type_type;
	res = PC_get(document, node, ".type", &type_type); if ( res ) return res;
	
	res = load_datatype(document, type_type, &type->type); return res;
}

static PDI_status_t load_contiguous(yaml_document_t *document, yaml_node_t *node, array_type_t *type)
{
	PDI_status_t res = PDI_OK;
	
	type->ndims = 1;
	type->order = ORDER_C;
	
	type->array_of_starts = malloc(sizeof(PDI_data_t));
	res = PDI_value("0", type->array_of_starts); if ( res ) return res;
	
	type->array_of_sizes = malloc(sizeof(PDI_data_t));
	char *expr;
	res = PC_get_string(document, node, ".count", &expr);
	res = PDI_value(expr, type->array_of_sizes); if ( res ) return res;
	
	type->array_of_subsizes = type->array_of_sizes;
	
	yaml_node_t *type_type;
	res = PC_get(document, node, ".type", &type_type); if ( res ) return res;
	res = load_datatype(document, type_type, &type->type); return res;
}

static PDI_status_t load_datatype(yaml_document_t *document, yaml_node_t *node, PDI_type_t *type)
{
	char *scalar_type;
	if ( !PC_get_string(document, node, "", &scalar_type) ) {
		type->kind = SCALAR;
		if ( !strcmp(scalar_type, "int") ) {
			//TODO: adapt to the actual size of int
			type->scalar = INT32;
		} //TODO: handle missing types
		return PDI_OK;
	}
	
	char *kind;
	if ( PC_get_string(document, node, ".kind", &kind) ) {
		return PDI_CONFIG_ERROR;
	}
	
	if ( !strcmp(kind, "subarray") ) {
		type->kind = ARRAY;
		type->array = malloc( sizeof(array_type_t) );
		return load_subarray(document, node, type->array);
	} else if ( !strcmp(kind, "contiguous") ) {
		type->kind = ARRAY;
		type->array = malloc( sizeof(array_type_t) );
		return load_contiguous(document, node, type->array);
	}
	
	return PDI_CONFIG_ERROR;
}

static PDI_status_t load_metadata_item(yaml_document_t *document, yaml_node_t *node, PDI_metadata_t *data)
{
	PDI_status_t res = PDI_OK;
	if ( node->type != YAML_MAPPING_NODE ) return PDI_CONFIG_ERROR;
	data->value = NULL;
	data->memstatus = PDI_UNALOCATED;
	yaml_node_pair_t *pair;
	for ( pair = node->data.mapping.pairs.start; pair < node->data.mapping.pairs.top; ++pair ) {
		char *key;
		if ( PC_get_string(document, yaml_document_get_node(document, pair->key), "", &key) ) {
			return PDI_CONFIG_ERROR;
		}
		if ( !strcmp(key, "name") ) {
			if ( PC_get_string(document, yaml_document_get_node(document, pair->value), "", &data->name) ) {
				return PDI_CONFIG_ERROR;
			}
		} else if ( !strcmp(key, "type") ) {
			data->type = malloc(sizeof(PDI_type_t));
			res = load_datatype(document, yaml_document_get_node(document, pair->value), data->type);
			if ( res ) return res;
		}
	}
	return res;
}

static PDI_status_t load_metadata(yaml_document_t *document, yaml_node_t *node)
{
	PDI_status_t res = PDI_OK;
	if ( node->type != YAML_SEQUENCE_NODE ) return PDI_CONFIG_ERROR;
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

static PDI_status_t load_data(yaml_document_t *document, yaml_node_t *node)
{
	PDI_status_t res = PDI_OK;
	if ( node->type != YAML_SEQUENCE_NODE ) return PDI_CONFIG_ERROR;
	return res;
}

PDI_status_t load_conf(yaml_document_t *document, yaml_node_t *node)
{
	PDI_status_t res = PDI_OK;
	yaml_node_pair_t *pair;
	if ( node->type != YAML_MAPPING_NODE ) return PDI_CONFIG_ERROR;
	for ( pair = node->data.mapping.pairs.start; pair < node->data.mapping.pairs.top; ++pair ) {
		char *key;
		if ( PC_get_string(document, yaml_document_get_node(document, pair->key), "", &key) ) {
			return PDI_CONFIG_ERROR;
		}
		if ( !strcmp(key, "metadata") ) {
			res = load_metadata(document, yaml_document_get_node(document, pair->value));
			if ( res ) return res;
		} else if ( !strcmp(key, "data") ) {
			res = load_data(document, yaml_document_get_node(document, pair->value));
			if ( res ) return res;
		}
	}
	return res;
}
