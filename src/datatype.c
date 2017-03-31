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
 * \file datatype.c
 * \brief Function to handle PDI data type.
 * Currently, structures management is not implmented
 * \author J. Bigot (CEA)
 */

#include <stdint.h>

#include <paraconf.h>

#include "pdi.h"
#include "pdi/value.h"

#include "pdi/datatype.h"

#include "status.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256

typedef struct val_size_s
{
	int ndims;

	size_t *sizes;

	size_t *subsizes;

	size_t *starts;

	size_t type;

} val_size_t;

PDI_status_t size(const PDI_type_t *type, val_size_t *result);
PDI_status_t PDI_array_datatype_is_dense(const PDI_array_type_t *type, int *is_dense);

PDI_status_t size_destroy(val_size_t *result)
{
	free(result->sizes);
	free(result->subsizes);
	free(result->starts);
	return PDI_OK;
}

val_size_t size_new()
{
	val_size_t new_size;
	new_size.ndims=-1;
	new_size.sizes=NULL;      
	new_size.subsizes=NULL;   
	new_size.starts=NULL;     
	new_size.type=PDI_T_UNDEF;
	return new_size;
}

PDI_status_t scal_size(PDI_scalar_type_t type, val_size_t *result)
{
	PDI_status_t status = PDI_OK;

	result->ndims = 0;
	result->sizes = NULL;
	result->subsizes = NULL;
	result->starts = NULL;

	switch ( type ) {
	case PDI_T_INT8:
		result->type = sizeof(int8_t);
		break;
	case PDI_T_INT16:
		result->type = sizeof(int16_t);
		break;
	case PDI_T_INT32:
		result->type = sizeof(int32_t);
		break;
	case PDI_T_INT64:
		result->type = sizeof(int64_t);
		break;
	case PDI_T_FLOAT:
		result->type = sizeof(float);
		break;
	case PDI_T_DOUBLE:
		result->type = sizeof(double);
		break;
	case PDI_T_LONG_DOUBLE:
		result->type = sizeof(long double);
		break;
	default:
		PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Unknown PDI type: #%d", type), err0);
	}

	return status;
err0:
	return status;
}

PDI_status_t array_size(const PDI_array_type_t *type, val_size_t *result)
{
	PDI_status_t status = PDI_OK;

	PDI_handle_err(size(&type->type, result), err0);
	for ( int dim=0; dim<type->ndims; ++dim ) {
		long size; PDI_handle_err(PDI_value_int(&(type->sizes[dim]), &size), err1);
		long subsize; PDI_handle_err(PDI_value_int(&(type->subsizes[dim]), &subsize), err1);
		if ( (size == subsize) && (result->ndims == 0) ) {
			result->type *= size;
		} else {
			long start; PDI_handle_err(PDI_value_int(type->starts, &start), err1);
			++result->ndims;
			result->sizes = realloc(result->sizes, result->ndims*sizeof(size_t));
			result->subsizes = realloc(result->subsizes, result->ndims*sizeof(size_t));
			result->starts = realloc(result->starts, result->ndims*sizeof(size_t));
			result->sizes[result->ndims-1] = size;
			result->subsizes[result->ndims-1] = subsize;
			result->starts[result->ndims-1] = start;
		}
	}

	return status;
err1:
	size_destroy(result);
err0:
	return status;
}

PDI_status_t size(const PDI_type_t *type, val_size_t *result)
{
	PDI_status_t status = PDI_OK;

	switch( type->kind ) {
	case PDI_K_SCALAR: {
		PDI_handle_err(scal_size(type->c.scalar, result), err0);
	} break;
	case PDI_K_ARRAY: {
		PDI_handle_err(array_size(type->c.array, result), err0);
	} break;
	case PDI_K_STRUCT: {
		PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Not implemented yet"), err0);
	} break;
	}

	return status;
err0:
	return status;
}


void do_copy_dense_to_sparse(int dim, const void *from, void *to, const val_size_t *to_size)
{
	if ( dim == to_size->ndims ) { 
		memcpy(to, from, to_size->type);
	} else {
		size_t blocksize = to_size->type; 
		for (int ii=dim+1; ii<to_size->ndims; ++ii) blocksize*= to_size->sizes[ii];  
		to = (char*)to + to_size->starts[dim]*blocksize; 
		for (size_t ii=0; ii<to_size->subsizes[dim]; ++ii) { 
			do_copy_dense_to_sparse(dim+1, from, to, to_size); 
			from = (char*)from + blocksize;
			to = (char*)to + blocksize;
		}
		int end = to_size->sizes[dim]-to_size->starts[dim]-to_size->subsizes[dim];
		to = (char*)to + end*blocksize;
	}
}

void do_copy_sparse_to_dense(int dim, const void *from, const val_size_t *from_size, void *to)
{
	if ( dim == from_size->ndims ) { 
		memcpy(to, from, from_size->type);
	} else {
		size_t blocksize = from_size->type; 
		for (int ii=dim+1; ii<from_size->ndims; ++ii) blocksize*= from_size->sizes[ii];  
		from = (char*)from + from_size->starts[dim]*blocksize; 
		for (size_t ii=0; ii<from_size->subsizes[dim]; ++ii) { 
			do_copy_sparse_to_dense(dim+1, from, from_size, to); 
			from = (char*)from + blocksize;
			to = (char*)to + blocksize;
		}
		int end = from_size->sizes[dim]-from_size->starts[dim]-from_size->subsizes[dim];
		from = (char*)from + end*blocksize;
	}
}


PDI_status_t PDI_copy(const void *from, const PDI_type_t *from_type, void *to, const PDI_type_t *to_type)
{
	PDI_status_t status = PDI_OK;
	val_size_t tsize;

	switch(from_type->kind){
		case PDI_K_SCALAR:
			// TODO[CR]: add the case of array with 1 element ?
			if( to_type->kind != PDI_K_SCALAR ) return PDI_ERR_VALUE;
			/// PDI does not perform type conversion
			if( to_type->c.scalar != from_type->c.scalar ) return PDI_ERR_VALUE;

			PDI_handle_err( scal_size(from_type->c.scalar, &tsize), err0 );
			memcpy(to, from, sizeof(tsize.type)); 
			size_destroy(&tsize);
			break;

		case PDI_K_ARRAY:
			if( to_type->kind != PDI_K_ARRAY ) return PDI_ERR_VALUE;
			//TODO: For now, do not handle imbricated arrays
			if( to_type->c.array->type.kind != PDI_K_SCALAR ) return PDI_UNAVAILABLE;
			/// PDI does not perform type conversion
			if( to_type->c.array->type.c.scalar != from_type->c.array->type.c.scalar ) return PDI_ERR_VALUE;

			int from_is_dense=0;
			int to_is_dense=0;
			PDI_handle_err(PDI_array_datatype_is_dense(to_type->c.array, &from_is_dense), err0);
			PDI_handle_err(PDI_array_datatype_is_dense(to_type->c.array, &to_is_dense), err0);
			/// Both arrays can't be sparse. 
			if( from_is_dense == 0 && to_is_dense == 0 ) return PDI_ERR_VALUE;

			/// from is dense, dense part is copied inside the other array
			if( from_is_dense ){
				PDI_handle_err( size(to_type, &tsize), err0);
				do_copy_dense_to_sparse(0, from, to, &tsize); 
			} else { //< from is sparse 
				PDI_handle_err( size(from_type, &tsize), err0);
				do_copy_sparse_to_dense(0, from, &tsize, to); 
			}
			size_destroy(&tsize);
			break;

		case PDI_K_STRUCT:
			return PDI_UNAVAILABLE;
	}

err0:
	return status;
}

PDI_status_t PDI_datatype_copy_dense(const PDI_type_t *type, PDI_type_t *dense)
{
	int status=PDI_OK;
	switch(type->kind){
		case PDI_K_SCALAR:
			dense->kind = PDI_K_SCALAR;
			dense->c.scalar = type->c.scalar;
			break;

		case PDI_K_ARRAY:
			// Set PDI_type
			dense->kind = PDI_K_ARRAY;
			dense->c.array = malloc(sizeof(PDI_array_type_t));

			int ndims = type->c.array->ndims;
			// Set PDI_array_type
			PDI_handle_err(PDI_datatype_copy_dense(&type->c.array->type, &dense->c.array->type), err0);
			dense->c.array->order = type->c.array->order;

			// set sizes
			dense->c.array->sizes = malloc( ndims*sizeof(PDI_value_t) );
			dense->c.array->subsizes = malloc( ndims*sizeof(PDI_value_t) );
			dense->c.array->starts = malloc( ndims*sizeof(PDI_value_t) );
			
			dense->c.array->ndims = ndims;
			for( int ii=0; ii<ndims; ++ii ){
				PDI_handle_err( PDI_value_copy(
					&type->c.array->sizes[ii], &( dense->c.array->sizes[ii] )), err1);
				PDI_handle_err( PDI_value_copy(
					&type->c.array->subsizes[ii], &( dense->c.array->subsizes[ii])), err1);
				PDI_handle_err( PDI_value_copy(
					&type->c.array->starts[ii],  &( dense->c.array->starts[ii])) , err1);
			}
			break;

		case PDI_K_STRUCT:
			return PDI_UNAVAILABLE;
	}
	return PDI_OK;

err1:
	free(dense->c.array->sizes);
	free(dense->c.array->subsizes);
	free(dense->c.array->starts);
err0:
	free(dense->c.array);
	return PDI_ERR_IMPL;
}

/** Indicate if a given array_type is dense or not 
 * 
 * \param array_type the type that is checked
 * \param is_dense an integer that stores 1 if the array is dense and 0 otherwise. 
 * \return an exit status code
 */
PDI_status_t PDI_array_datatype_is_dense(const PDI_array_type_t *type, int *is_dense)
{
	PDI_status_t status = PDI_OK;
	*is_dense=1;
	for( int dim=0; dim<type->ndims; ++dim){
		long size; PDI_handle_err(PDI_value_int(&(type->sizes[dim]), &size), err0);
		long subsize; PDI_handle_err(PDI_value_int(&(type->subsizes[dim]), &subsize), err0);
		long start; PDI_handle_err(PDI_value_int(&(type->starts[dim]), &start), err0);
		if( (start > 0) || size > subsize ){
			*is_dense=0;
			break;
		}
	}

	return PDI_OK;
err0:
	return PDI_ERR_IMPL;
}

PDI_status_t PDI_datatype_is_dense(const PDI_type_t *type, int *is_dense){
	switch(type->kind){
		case PDI_K_SCALAR:
			*is_dense=1;
			return PDI_OK;

		case PDI_K_ARRAY:
			return PDI_array_datatype_is_dense(type->c.array, is_dense);

		case PDI_K_STRUCT:
			return PDI_UNAVAILABLE;
	}
	return PDI_OK;
}



static PDI_status_t load_array(PC_tree_t node, PDI_array_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	PDI_status_t sizes_invalid = PC_len(PC_get(node, ".sizes"), &type->ndims);
	PC_errhandler(pc_handler); // aka PC_end_try
	
	if ( !sizes_invalid ) { // multi dim array
		type->sizes = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".sizes[%d]", ii), &expr), err1);
			PDI_handle_err(PDI_value_parse(expr, &type->sizes[ii]), err1);
err1:
			free(expr);
			PDI_handle_err(status, err0);
		}
	} else { //single dim array
		type->ndims = 1;
		type->sizes = malloc(type->ndims*sizeof(PDI_value_t));
		char *expr; handle_PC_err(PC_string(PC_get(node, ".size"), &expr), err5);
		PDI_handle_err(PDI_value_parse(expr, type->sizes), err5);
err5:
		free(expr);
		PDI_handle_err(status, err0);
	}
	
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	int len; PC_status_t invalid_subsizes = PC_len(PC_get(node, ".subsizes"), &len);
	PC_errhandler(pc_handler); // aka PC_end_try
	
	if ( !invalid_subsizes ) {
		if ( len != type->ndims ) PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", len, type->ndims), err0);	
		type->subsizes = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".subsizes[%d]", ii), &expr), err2);
			PDI_handle_err(PDI_value_parse(expr, &type->subsizes[ii]), err2);
err2:
			free(expr);
			PDI_handle_err(status, err0);
		}
		
	} else { // no subsize, default to full size
		type->subsizes = type->sizes;
	}
	
	
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	if ( !PC_len(PC_get(node, ".starts"), &len) ) {
		PC_errhandler(pc_handler); // aka PC_end_try
		if ( len != type->ndims ) PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", len, type->ndims), err0);
		type->starts = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			char *expr; handle_PC_err(PC_string(PC_get(node, ".starts[%d]", ii), &expr), err3);
			PDI_handle_err(PDI_value_parse(expr, &type->starts[ii]), err3);
err3:
			free(expr);
			PDI_handle_err(status, err0);
		}
	} else { // no start, start at 0 everywhere
		PC_errhandler(pc_handler); // aka PC_end_try
		type->starts = malloc(type->ndims*sizeof(PDI_value_t));
		for ( int ii=0; ii<type->ndims; ++ii ) {
			PDI_handle_err(PDI_value_parse("0", &type->starts[ii]), err0);
		}
	}
	
	// Order: C or fortran ordering, default is C
	pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
	char *order="c"; PC_string(PC_get(node, ".order"), &order);
	PC_errhandler(pc_handler); // aka PC_end_try
	// in case of error, char *order is not modified so order=="c" is true
	if( (!strcmp(order, "c"))||(!strcmp(order, "C")) ){
		type->order=PDI_ORDER_C;
	} else if( (!strcmp(order, "fortran"))||(!strcmp(order, "Fortran")) ){
		type->order=PDI_ORDER_FORTRAN;
	} else {
		PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Incorrect array ordering: `%s'", order), err0);
	}

	PC_tree_t type_type = PC_get(node, ".type");
	handle_PC_err(PC_status(type_type), err0);
	PDI_handle_err(PDI_datatype_load(type_type, &type->type), err0);
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
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Unknown primitive type: `%s'", buf_str), err0);
		}
	} else { // case where the datatype is composite
		type->kind = PDI_K_ARRAY;
		type->c.array = malloc( sizeof(PDI_array_type_t) );
		PDI_handle_err(load_array(node, type->c.array), err0);
	}
	
err0:
	free(buf_str);
	return status;
}

PDI_status_t PDI_data_size(const PDI_type_t *type, size_t *result)
{
	PDI_status_t status = PDI_OK;
	val_size_t valsz = size_new();
	PDI_handle_err(size(type, &valsz), err0);
	*result = valsz.type;
	for ( int dim=0; dim<valsz.ndims; ++dim ) {
		*result *= valsz.subsizes[dim];
	}
	size_destroy(&valsz);

	return status;
err0:
	return status;
}

PDI_status_t PDI_array_datatype_destroy(PDI_array_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_datatype_destroy(&type->type), err0);
	// don't free subsizes in case it was a copy of sizes
	if ( type->subsizes != type->sizes) free(type->subsizes);

	free(type->sizes);
	free(type->starts);
	
	return status;
err0:
	return status;
}

PDI_status_t PDI_datatype_destroy(PDI_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	switch (type->kind) {
	case PDI_K_ARRAY: {
		PDI_array_datatype_destroy(type->c.array);
		free(type->c.array);
	} break;
	case PDI_K_STRUCT: {
		PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Structs not implemented yet"), err0);
		free(type->c.struct_);
	} break;
	case PDI_K_SCALAR: {
		// nothing to do here
	} break;
	}
	
	return status;
err0:
	return status;
}
