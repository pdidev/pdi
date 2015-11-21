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

#include "datatype.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256

static PDI_status_t load_subarray(yaml_document_t *document, yaml_node_t *node, array_type_t *type)
{
	PDI_status_t res = PDI_OK;
	res = PC_get_int(document, node, ".ndims", &type->ndims); if ( res ) return res;
	
	int len;
	res = PC_get_len(document, node, ".array_of_sizes", &len);
	if ( len != type->ndims ) return PDI_ERR_CONFIG;
	res = PC_get_len(document, node, ".array_of_subsizes", &len);
	if ( len != type->ndims ) return PDI_ERR_CONFIG;
	res = PC_get_len(document, node, ".array_of_starts", &len);
	if ( len != type->ndims ) return PDI_ERR_CONFIG;
	
	int ii;
	type->array_of_sizes = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_sizes[%d]", ii);
		char *expr = NULL;
		res = PC_get_string(document, node, idx, &expr, 0); if (res) goto sizes_free;
		res = PDI_value(expr, &type->array_of_sizes[ii]); if (res) goto sizes_free;
sizes_free:
		free(expr);
		if ( res ) return res;
	}
	
	type->array_of_subsizes = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_subsizes[%d]", ii);
		char *expr = NULL;
		res = PC_get_string(document, node, idx, &expr, 0); if (res) goto subsizes_free;
		res = PDI_value(expr, &type->array_of_subsizes[ii]); if (res) goto subsizes_free;
subsizes_free:
		free(expr);
		if ( res ) return res;
	}
	
	type->array_of_starts = malloc(type->ndims*sizeof(PDI_value_t));
	for ( ii=0; ii<type->ndims; ++ii ) {
		char idx[IDX_BUF_SIZE];
		snprintf(idx, IDX_BUF_SIZE, ".array_of_starts[%d]", ii);
		char *expr = NULL;
		res = PC_get_string(document, node, idx, &expr, 0); if (res) goto starts_free;
		res = PDI_value(expr, &type->array_of_starts[ii]); if (res) goto starts_free;
starts_free:
		free(expr);
		if ( res ) return res;
	}
	
	char *order_str = NULL;
	res = PC_get_string(document, node, ".order", &order_str, 0); if ( res ) goto order_free;
	if ( !strcmp(order_str, "ORDER_C") ) {
		type->order = ORDER_C;
	} else if ( !strcmp(order_str, "ORDER_FORTRAN") ) {
		type->order = ORDER_FORTRAN;
	} else {
		res = PDI_ERR_CONFIG;
		goto order_free;
	}
order_free:
	free(order_str);
	if ( res ) return res;
	
	yaml_node_t *type_type;
	res = PC_get(document, node, ".type", &type_type); if ( res ) return res;
	
	res = PDI_datatype_load(document, type_type, &type->type); return res;
}

static PDI_status_t load_contiguous(yaml_document_t *document, yaml_node_t *node, array_type_t *type)
{
	PDI_status_t res = PDI_OK;
	
	type->ndims = 1;
	type->order = ORDER_C;
	
	type->array_of_starts = malloc(sizeof(PDI_value_t));
	res = PDI_value("0", type->array_of_starts); if ( res ) return res;
	
	type->array_of_sizes = malloc(sizeof(PDI_value_t));
	char *expr = NULL;
	res = PC_get_string(document, node, ".count", &expr, 0); if ( res ) goto expr_free;
	res = PDI_value(expr, type->array_of_sizes); if ( res ) goto expr_free;
expr_free:
	free(expr);
	if ( res ) return res;
	
	type->array_of_subsizes = type->array_of_sizes;
	
	yaml_node_t *type_type;
	res = PC_get(document, node, ".type", &type_type); if ( res ) return res;
	res = PDI_datatype_load(document, type_type, &type->type); return res;
}

PDI_status_t PDI_datatype_load(yaml_document_t *document, yaml_node_t *node, PDI_type_t *type)
{
	char *buf_str = NULL; 
	if ( !PC_get_string(document, node, "", &buf_str, 0) ) {
		type->kind = SCALAR;
		if ( !strcmp(buf_str, "int") ) {
			//TODO: adapt to the actual size of int
			type->scalar = INT32;
		} //TODO: handle missing types
		free(buf_str);
		return PDI_OK;
	}
	
	char *kind = NULL;
	if ( PC_get_string(document, node, ".kind", &kind, 0) ) {
		free(kind);
		return PDI_ERR_CONFIG;
	}
	
	if ( !strcmp(kind, "subarray") ) {
		free(kind);
		type->kind = ARRAY;
		type->array = malloc( sizeof(array_type_t) );
		return load_subarray(document, node, type->array);
	} else if ( !strcmp(kind, "contiguous") ) {
		free(kind);
		type->kind = ARRAY;
		type->array = malloc( sizeof(array_type_t) );
		return load_contiguous(document, node, type->array);
	}
	
	free(kind);
	return PDI_ERR_CONFIG;
}
