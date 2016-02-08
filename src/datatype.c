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
#include "pdi/value.h"

#include "pdi/datatype.h"

#include "status.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256

static PDI_status_t load_array(PC_tree_t node, PDI_array_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	if ( !PC_len(PC_get(node, ".sizes"), &type->ndims) ) { // multi dim array
		PC_errhandler(pc_handler); // aka PC_end_try
		type->sizes = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".sizes[%d]", ii), &expr), err1);
			handle_err(PDI_value_parse(expr, &type->sizes[ii]), err1);
err1:
			free(expr);
			handle_err(status, err0);
		}
	} else { //single dim array
		PC_errhandler(pc_handler); // aka PC_end_try
		type->ndims = 1;
		type->sizes = malloc(type->ndims*sizeof(PDI_value_t));
		char *expr; handle_PC_err(PC_string(PC_get(node, ".size"), &expr), err5);
		handle_err(PDI_value_parse(expr, type->sizes), err5);
err5:
		free(expr);
		handle_err(status, err0);
	}
	
	int len; 
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	if ( !PC_len(PC_get(node, ".subsizes"), &len) ) {
		PC_errhandler(pc_handler); // aka PC_end_try
		if ( len != type->ndims ) handle_err(handle_error(PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", len, type->ndims), err0);	
		type->subsizes = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".subsizes[%d]", ii), &expr), err2);
			handle_err(PDI_value_parse(expr, &type->subsizes[ii]), err2);
err2:
			free(expr);
			handle_err(status, err0);
		}
		
	} else { // no subsize, default to full size
		PC_errhandler(pc_handler); // aka PC_end_try
		type->subsizes = type->sizes;
	}
	
	
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	if ( !PC_len(PC_get(node, ".starts"), &len) ) {
		PC_errhandler(pc_handler); // aka PC_end_try
		if ( len != type->ndims ) handle_err(handle_error(PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", len, type->ndims), err0);
		type->starts = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".starts[%d]", ii), &expr), err3);
			handle_err(PDI_value_parse(expr, &type->starts[ii]), err3);
err3:
			free(expr);
			handle_err(status, err0);
		}
	} else { // no start, start at 0 everywhere
		PC_errhandler(pc_handler); // aka PC_end_try
		type->starts = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			handle_err(PDI_value_parse("0", &type->starts[ii]), err0);
		}
	}
	
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	char *order_str; 
	if ( !PC_string(PC_get(node, ".order"), &order_str) ) {
		PC_errhandler(pc_handler); // aka PC_end_try
		if ( !strcmp(order_str, "ORDER_C") ) {
			type->order = PDI_ORDER_C;
		} else if ( !strcmp(order_str, "ORDER_FORTRAN") ) {
			type->order = PDI_ORDER_FORTRAN;
		} else {
			handle_err(handle_error(PDI_ERR_CONFIG, "Invalid order, should be either ORDER_C or ORDER_FORTRAN"), err4);
		}
err4:
		free(order_str);
		handle_err(status, err0);
	} else { // default to ORDER_C if none specified
		//TODO: change to calling language order
		type->order = PDI_ORDER_C;
	}
	
	PC_tree_t type_type = PC_get(node, ".type");
	handle_PC_err(PC_status(type_type), err0);
	handle_err(PDI_datatype_load(type_type, &type->type), err0);
	
err0:
	return status;
}

PDI_status_t PDI_datatype_load(PC_tree_t node, PDI_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	char *buf_str = NULL; PC_string(node, &buf_str);
	PC_errhandler(pc_handler); // aka PC_end_try
	
	if ( buf_str ) { // case where the datatype is primitive
		type->kind = PDI_K_SCALAR;
		if ( !strcmp(buf_str, "int") ) {
			//TODO: adapt to the actual size of int
			type->c.scalar = PDI_T_INT32;
		} else if ( !strcmp(buf_str, "double") ) {
			type->c.scalar = PDI_T_DOUBLE;
		} else {
			//TODO: handle missing types
			handle_err(handle_error(PDI_ERR_VALUE, "Unknown primitive type: `%s'", buf_str), err0);
		}
	} else { // case where the datatype is composite
		type->kind = PDI_K_ARRAY;
		type->c.array = malloc( sizeof(PDI_array_type_t) );
		handle_err(load_array(node, type->c.array), err0);
	}
	
err0:
	free(buf_str);
	return status;
}
