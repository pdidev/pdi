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

#include "config.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256


using PDI::Value;

/// ordering of array
typedef enum PDI_order_e {
	PDI_ORDER_C,
	PDI_ORDER_FORTRAN
} PDI_order_t;


/// Descriptor of a buffer and its content
typedef struct buffer_descriptor_s {
	/// number of dimensions of the array
	int ndims;
	
	/* Array of sizes of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is sizes[0], the fastest sizes[ndims-1]
	 *
	 * The stride for a given dimension is the product of the element size by all
	 * size of dimensions with a lower index.
	 */
	size_t *sizes;
	
	/* Array of subsizes of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is subsizes[0], the fastest subsizes[ndims-1]
	 *
	 * The subsize describes the part of the array actually containing data.
	 */
	size_t *subsizes;
	
	/* Array of start of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is starts[0], the fastest starts[ndims-1]
	 *
	 * The start is the first index in a given dimension containing data.
	 */
	size_t *starts;
	
	/// Size of the dense block contained in the array
	size_t dense_size;
	
} buffer_descriptor_t;


PDI_datatype_t PDI_UNDEF_TYPE = { PDI_K_SCALAR, { PDI_T_UNDEF } };


static PDI_status_t bufdesc_destroy(buffer_descriptor_t *result)
{
	free(result->sizes);
	free(result->subsizes);
	free(result->starts);
	return PDI_OK;
}


static PDI_status_t bufdesc_datasize(const buffer_descriptor_t *bufdesc, size_t *result)
{
	PDI_status_t status = PDI_OK;
	
	*result = bufdesc->dense_size;
	for (int dim = 0; dim < bufdesc->ndims; ++dim) {
		*result *= bufdesc->subsizes[dim];
	}
	
	return status;
}


static PDI_status_t bufdesc_buffersize(const buffer_descriptor_t *bufdesc, size_t *result)
{
	PDI_status_t status = PDI_OK;
	
	*result = bufdesc->dense_size;
	for (int dim = 0; dim < bufdesc->ndims; ++dim) {
		*result *= bufdesc->sizes[dim];
	}
	
	return status;
}


/** Return a reordered index, i.e. a C style index given either a C for Fortran
 *  style one
 * \param ordered_index the initial C or Fortran ordered index
 * \param order the style of the initial index (C/Fortran)
 * \param size the size of the dimension the index accesses
 * \return the reordered index (C style)
 */
static int ridx(int ordered_index, PDI_order_t order, int size)
{
	if (order == PDI_ORDER_FORTRAN) return size - ordered_index - 1;
	return ordered_index;
}


static PDI_status_t array_datatype_load(PC_tree_t node, PDI_array_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PDI_array_type_t res_type;
	
	// Order: C or fortran ordering, default is C
	PDI_order_t order = PDI_ORDER_C;
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		char *order_str = strdup("c");
		PC_string(PC_get(node, ".order"), &order_str);
		if ((!strcmp(order_str, "c")) || (!strcmp(order_str, "C"))) {
			order = PDI_ORDER_C;
		} else if ((!strcmp(order_str, "fortran")) || (!strcmp(order_str, "Fortran"))) {
			order = PDI_ORDER_FORTRAN;
		} else {
			PDI_status_t err_status = PDI_make_err(PDI_ERR_CONFIG, "Incorrect array ordering: `%s'", order_str);
			free(order_str);
			PDI_handle_err(err_status, err0);
		}
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	// sizes for a multidim array
	PC_status_t invalid_sizes;
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		invalid_sizes = PC_len(PC_get(node, ".sizes"), &res_type.ndims);
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	if (!invalid_sizes) {   // if multi dim array
		res_type.sizes = (PDI_value_t *) malloc(res_type.ndims * sizeof(PDI_value_t));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr; handle_PC_err(PC_string(PC_get(node, ".sizes[%d]", ii), &expr), err0);
			res_type.sizes[ri] = Value{expr};
			free(expr);
			continue;
			
err1a:
			free(expr); // in case of error
			for (int jj = 0; jj < ii; ++jj) {
				ri = ridx(jj, order, res_type.ndims);
				PDI_value_destroy(&res_type.sizes[ri]);
			}
			PDI_handle_err(status, err0);
		}
	} else { // else single dim array
		res_type.ndims = 1;
		res_type.sizes = (PDI_value_t *) malloc(res_type.ndims * sizeof(PDI_value_t));
		char *expr; handle_PC_err(PC_string(PC_get(node, ".size"), &expr), err0);
		*res_type.sizes = Value{expr};
err1b:
		free(expr);
		PDI_handle_err(status, err0);
	}
	
	PC_status_t invalid_subsizes;
	{
		int len;
		{
			PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
			invalid_subsizes = PC_len(PC_get(node, ".subsizes"), &len);
			PC_errhandler(pc_handler); // aka PC_end_try
		}
		if (!invalid_subsizes && len != res_type.ndims) {
			PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", len, res_type.ndims), err1);
		}
	}
	
	if (!invalid_subsizes) {
		res_type.subsizes = (PDI_value_t *) malloc(res_type.ndims * sizeof(PDI_value_t));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr; handle_PC_err(PC_string(PC_get(node, ".subsizes[%d]", ii), &expr), err1);
			res_type.subsizes[ri] = Value{expr};
			free(expr);
			continue;
err2a:
			free(expr);
			for (int jj = 0; jj < ii; ++jj) {
				ri = ridx(jj, order, res_type.ndims);
				PDI_value_destroy(&res_type.sizes[ri]);
			}
			PDI_handle_err(status, err1);
		}
	} else { // no subsize, default to full size
		res_type.subsizes = res_type.sizes;
	}
	
	PC_status_t invalid_starts;
	{
		int len;
		{
			PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
			invalid_starts = PC_len(PC_get(node, ".starts"), &len);
			PC_errhandler(pc_handler); // aka PC_end_try
		}
		if (!invalid_starts && len != res_type.ndims) {
			PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", len, res_type.ndims), err2);
		}
	}
	
	res_type.starts = (PDI_value_t *)malloc(res_type.ndims * sizeof(PDI_value_t));
	if (!invalid_starts) {
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr; handle_PC_err(PC_string(PC_get(node, ".starts[%d]", ii), &expr), err2);
			res_type.starts[ri] = Value{expr};
			free(expr);
			continue;
err3a: {
				// error block
				free(expr); // freeing all memory
				for (int jj = 0; jj < ii; ++jj) {
					ri = ridx(jj, order, res_type.ndims);
					PDI_value_destroy(&res_type.sizes[ri]);
				}
				PDI_handle_err(status, err2);
			}
		}
	} else { // no start, start at 0 everywhere
		res_type.starts = (PDI_value_t *) malloc(res_type.ndims * sizeof(PDI_value_t));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			res_type.starts[ii] = Value{"0"};
		}
	}
	
	PC_tree_t type_type;
	type_type = PC_get(node, ".type"); // keep separate line, otherwise get error with err2 jump
	handle_PC_err(PC_status(type_type), err3);
	PDI_handle_err(PDI_datatype_load(&res_type.type, type_type), err3);
	
	*type = res_type;
	return status;
	
	
err3: { // handling errors after "starts" have been allocated
		for (int jj = 0; jj < res_type.ndims ; ++jj)
			PDI_value_destroy(&res_type.subsizes[jj]);
		free(res_type.starts);
	}
err2: { // --------------------  "subsizes"  -------------------
		if (res_type.subsizes != res_type.sizes) { // subsizes exist
			for (int jj = 0; jj < res_type.ndims ; ++jj)
				PDI_value_destroy(&res_type.subsizes[jj]);
			free(res_type.subsizes);
		}
	}
err1: { // --------------------  "sizes"  -----------------------
		for (int jj = 0; jj < res_type.ndims ; ++jj)
			PDI_value_destroy(&res_type.sizes[jj]);
		free(res_type.sizes);
	}
err0:
	return status;
}


static PDI_status_t struct_datatype_load(PC_tree_t node, PDI_struct_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	(void) node; // to prevent unused parameter warnings
	(void) type; // to prevent unused parameter warnings
	
	PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Structure support not implemented yet"), err0);
	
	return status;
	
err0:
	return status;
}


static PDI_status_t scalar_datatype_load(PC_tree_t node, PDI_scalar_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	char *tname = NULL;
	long kind = 0;
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		PC_string(PC_get(node, ".type"), &tname);
		PC_int(PC_get(node, ".kind"), &kind);
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	if (!tname) {
		handle_PC_err(PC_string(node, &tname), err0);
	}
	
	// For Fortran, we assume kind means number of bytes... TODO: autodetect
	if (!strcmp(tname, "char") && kind == 0) { // C char
		*type = PDI_T_INT8;
	} else if (!strcmp(tname, "int") && kind == 0) { // C int
		switch (sizeof(int)) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Unsupported int size: %d", sizeof(int) * 8), err1);
		}
	} else if (!strcmp(tname, "int8") && kind == 0) { // C int8
		*type = PDI_T_INT8;
	} else if (!strcmp(tname, "int16") && kind == 0) { // C int16
		*type = PDI_T_INT16;
	} else if (!strcmp(tname, "int32") && kind == 0) { // C int32
		*type = PDI_T_INT32;
	} else if (!strcmp(tname, "int64") && kind == 0) { // C int64
		*type = PDI_T_INT64;
	} else if (!strcmp(tname, "float") && kind == 0) { // C float
		*type = PDI_T_FLOAT;
	} else if (!strcmp(tname, "double") && kind == 0) { // C double
		*type = PDI_T_DOUBLE;
	} else if (!strcmp(tname, "character")) {   // Fortran character
		if (kind == 0) kind = PDI_CHARACTER_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid kind for character: `%s'", tname), err1);
		}
	} else if (!strcmp(tname, "integer")) {   // Fortran integer
		if (kind == 0) kind = PDI_INTEGER_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname), err1);
		}
	} else if (!strcmp(tname, "logical")) {   // Fortran integer
		if (kind == 0) kind = PDI_LOGICAL_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname), err1);
		}
	} else if (!strcmp(tname, "real")) {   // Fortran real
		if (kind == 0) kind = PDI_REAL_DEFAULT_KIND;
		switch (kind) {
		case 4:
			*type = PDI_T_FLOAT;
			break;
		case 8:
			*type = PDI_T_DOUBLE;
			break;
		case 16:
			*type = PDI_T_LONG_DOUBLE;
			break;
		default:
			PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid kind for real: `%s'", tname), err1);
		}
	} else {
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE, "Invalid scalar type: `%s'", tname), err1);
	}
	
	free(tname);
	return status;
err1:
	free(tname);
err0:
	return status;
}


static PDI_status_t array_datatype_destroy(PDI_array_type_t *type)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_datatype_destroy(&type->type), err0);
	// don't free subsizes in case it was a copy of sizes
	if (type->subsizes != type->sizes) free(type->subsizes);
	
	free(type->sizes);
	free(type->starts);
	
	return status;
err0:
	return status;
}


static PDI_status_t array_datatype_is_dense(const PDI_array_type_t *type, int *is_dense)
{
	PDI_status_t status = PDI_OK;
	
	int result = 1;
	
	for (int dim = 0; dim < type->ndims; ++dim) {
		long size; PDI_handle_err(PDI_value_int(&(type->sizes[dim]), &size), err0);
		long subsize; PDI_handle_err(PDI_value_int(&(type->subsizes[dim]), &subsize), err0);
		if (size != subsize) {
			result = 0;
			break;
		}
	}
	
	*is_dense = result; // late copy so as not to modify arguments in case of error
	return status;
	
err0:
	return status;
}


static PDI_status_t datatype_bufdesc(const PDI_datatype_t *type, buffer_descriptor_t *result);


static PDI_status_t scalar_datatype_bufdesc(PDI_scalar_type_t type, buffer_descriptor_t *result)
{
	PDI_status_t status = PDI_OK;
	
	result->ndims = 0;
	result->sizes = NULL;
	result->subsizes = NULL;
	result->starts = NULL;
	
	switch (type) {
	case PDI_T_INT8:
		result->dense_size = sizeof(int8_t);
		break;
	case PDI_T_INT16:
		result->dense_size = sizeof(int16_t);
		break;
	case PDI_T_INT32:
		result->dense_size = sizeof(int32_t);
		break;
	case PDI_T_INT64:
		result->dense_size = sizeof(int64_t);
		break;
	case PDI_T_FLOAT:
		result->dense_size = sizeof(float);
		break;
	case PDI_T_DOUBLE:
		result->dense_size = sizeof(double);
		break;
	case PDI_T_LONG_DOUBLE:
		result->dense_size = sizeof(long double);
		break;
	default:
		PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Unknown PDI type: #%d", type), err0);
	}
	
	return status;
	
err0:
	return status;
}


static PDI_status_t array_datatype_bufdesc(const PDI_array_type_t *type, buffer_descriptor_t *result)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(datatype_bufdesc(&type->type, result), err0);
	for (int dim = 0; dim < type->ndims; ++dim) {
		long size; PDI_handle_err(PDI_value_int(&(type->sizes[dim]), &size), err1);
		long subsize; PDI_handle_err(PDI_value_int(&(type->subsizes[dim]), &subsize), err1);
		if ((size == subsize) && (result->ndims == 0)) {   // dense case
			result->dense_size *= size;
		} else { // sparse case
			long start; PDI_handle_err(PDI_value_int(&type->starts[dim], &start), err1);
			++result->ndims;
			result->sizes    = (size_t *) realloc(result->sizes, result->ndims * sizeof(size_t));
			result->subsizes = (size_t *) realloc(result->subsizes, result->ndims * sizeof(size_t));
			result->starts   = (size_t *) realloc(result->starts, result->ndims * sizeof(size_t));
			result->sizes[result->ndims - 1] = size;
			result->subsizes[result->ndims - 1] = subsize;
			result->starts[result->ndims - 1] = start;
		}
	}
	
	return status;
err1:
	bufdesc_destroy(result);
err0:
	return status;
}


static PDI_status_t datatype_bufdesc(const PDI_datatype_t *type, buffer_descriptor_t *result)
{
	PDI_status_t status = PDI_OK;
	
	switch (type->kind) {
	case PDI_K_SCALAR: {
		PDI_handle_err(scalar_datatype_bufdesc(type->c.scalar, result), err0);
	} break;
	case PDI_K_ARRAY: {
		PDI_handle_err(array_datatype_bufdesc(type->c.array, result), err0);
	} break;
	case PDI_K_STRUCT: {
		PDI_handle_err(PDI_make_err(PDI_ERR_CONFIG, "Not implemented yet"), err0);
	} break;
	}
	
	return status;
	
err0:
	return status;
}


/** Copies a subpart of from content into to
 *
 * \param dim the number of the subdimension to copy
 * \param from buffer holding the from data
 * \param from_size size of the data in from
 * \param to buffer holding the to data
 * \param to_size size of the data in to
 */
static PDI_status_t buffer_do_copy(void *to, const buffer_descriptor_t *to_desc, const void *from, const buffer_descriptor_t *from_desc)
{
	PDI_status_t status = PDI_OK;
	
	
	size_t from_size; PDI_handle_err(bufdesc_datasize(from_desc, &from_size), err0);
	size_t to_size; PDI_handle_err(bufdesc_datasize(to_desc, &to_size), err0);
	
	// and now for a little defensive programming
	if (from_size != to_size) {
		PDI_handle_err(PDI_make_err(PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size), err0);
	}
	
	
	if (0 == from_desc->ndims && 0 == to_desc->ndims) {   // dense to dense
		if (from_desc->dense_size != to_desc->dense_size) {
			PDI_handle_err(PDI_make_err(PDI_ERR_TYPE, "Incompatible type size for dense copy: %ld vs. %ld .", (long)from_desc->dense_size, (long)to_desc->dense_size), err0);
		}
		memcpy(to, from, from_desc->dense_size);
		
	} else { // sparse involved, explicit loop
		size_t nblocks;
		buffer_descriptor_t from_blockdesc;
		size_t from_start;
		buffer_descriptor_t to_blockdesc;
		size_t to_start;
		
		if (0 == from_desc->ndims) {   // dense to sparse
			nblocks = to_desc->subsizes[0];
			
			from_blockdesc = *from_desc;
			from_blockdesc.dense_size /= to_desc->subsizes[0];
			from_start = 0;
			
			to_blockdesc = *to_desc;
			// remove slowest dim
			--to_blockdesc.ndims;
			++to_blockdesc.sizes;
			++to_blockdesc.starts;
			++to_blockdesc.subsizes;
			to_start = to_desc->starts[0];
			
		} else if (0 == to_desc->ndims) {   // sparse to dense
			nblocks = from_desc->subsizes[0];
			
			from_blockdesc = *from_desc;
			// remove slowest dim
			--from_blockdesc.ndims;
			++from_blockdesc.sizes;
			++from_blockdesc.starts;
			++from_blockdesc.subsizes;
			from_start = from_desc->starts[0];
			
			to_blockdesc = *to_desc;
			to_blockdesc.dense_size /= from_desc->subsizes[0];
			to_start = 0;
			
		} else { // sparse to sparse
			if (from_desc->subsizes[0] != to_desc->subsizes[0]) {
				PDI_handle_err(PDI_make_err(PDI_ERR_TYPE, "Incompatible array type size for copy."), err0);
			}
			nblocks = from_desc->subsizes[0];
			
			from_blockdesc = *from_desc;
			// remove slowest dim
			--from_blockdesc.ndims;
			++from_blockdesc.sizes;
			++from_blockdesc.starts;
			++from_blockdesc.subsizes;
			from_start = from_desc->starts[0];
			
			to_blockdesc = *to_desc;
			// remove slowest dim
			--to_blockdesc.ndims;
			++to_blockdesc.sizes;
			++to_blockdesc.starts;
			++to_blockdesc.subsizes;
			to_start = to_desc->starts[0];
			
		}
		size_t to_stride; bufdesc_buffersize(&to_blockdesc, &to_stride);
		size_t from_stride; bufdesc_buffersize(&from_blockdesc, &from_stride);
		from = (char *)from + from_start * from_stride;
		to = (char *)to + to_start * to_stride;
		for (size_t ii = 0; ii < nblocks; ++ii) {
			buffer_do_copy(to, &to_blockdesc, from, &from_blockdesc);
			from = (char *)from + from_stride;
			to = (char *)to + to_stride;
		}
	}
	
	return status;
	
err0:
	return status;
}


// public functions


PDI_status_t PDI_datatype_init_scalar(PDI_datatype_t *dest, PDI_scalar_type_t value)
{
	dest->kind = PDI_K_SCALAR;
	dest->c.scalar = value;
	return PDI_OK;
}


PDI_status_t PDI_datatype_init_array(PDI_datatype_t *dest, const PDI_datatype_t *type, int ndims,
                                     const PDI_value_t *sizes, const PDI_value_t *subsizes, const PDI_value_t *starts)
{
	PDI_status_t status = PDI_OK;
	
	PDI_array_type_t *array = (PDI_array_type_t *) malloc(sizeof(PDI_array_type_t));
	
	array->ndims = ndims;
	
	array->sizes = (PDI_value_t *) malloc(ndims * sizeof(PDI_value_t));
	for (int ii = 0; ii < ndims; ++ii) {
		PDI_handle_err(PDI_value_copy(&(sizes[ii]), &(array->sizes[ii])), err0);
	}
	
	if (sizes == subsizes) {
		array->subsizes = array->sizes;
	} else {
		array->subsizes = (PDI_value_t *)malloc(ndims * sizeof(PDI_value_t));
		for (int ii = 0; ii < ndims; ++ii) {
			PDI_handle_err(PDI_value_copy(&(subsizes[ii]), &(array->subsizes[ii])), err1);
		}
	}
	
	array->starts = (PDI_value_t *)malloc(ndims * sizeof(PDI_value_t));
	if (!starts) {
		for (int ii = 0; ii < ndims; ++ii) {
			array->starts[ii] = Value{"0"};
		}
	} else {
		for (int ii = 0; ii < ndims; ++ii) {
			PDI_handle_err(PDI_value_copy(&(starts[ii]), &(array->starts[ii])), err2);
		}
	}
	
	PDI_handle_err(PDI_datatype_copy(&array->type, type), err3);
	
	dest->kind = PDI_K_ARRAY;
	dest->c.array = array;
	
	return status;
	
err3:
	for (int ii = 0; ii < ndims; ++ii) PDI_value_destroy(&array->starts[ii]);
err2:
	//TODO: free already built starts
	free(array->starts);
	for (int ii = 0; ii < ndims; ++ii) PDI_value_destroy(&array->subsizes[ii]);
err1:
	//TODO: free already built subsizes
	if (array->subsizes != array->sizes) free(array->subsizes);
	for (int ii = 0; ii < ndims; ++ii) PDI_value_destroy(&array->sizes[ii]);
err0:
	//TODO: free already built sizes
	free(array->sizes);
	free(array);
	return status;
}


PDI_status_t PDI_datatype_copy(PDI_datatype_t *copy, const PDI_datatype_t *from)
{
	PDI_status_t status = PDI_OK;
	
	switch (from->kind) {
	case PDI_K_SCALAR:
		PDI_handle_err(PDI_datatype_init_scalar(copy, from->c.scalar), err0);
		break;
	case PDI_K_ARRAY:
		PDI_handle_err(PDI_datatype_init_array(copy, &from->c.array->type,
		                                       from->c.array->ndims, from->c.array->sizes, from->c.array->subsizes,
		                                       from->c.array->starts),
		               err0);
		break;
	case PDI_K_STRUCT:
		PDI_handle_err(PDI_make_err(PDI_UNAVAILABLE, "Structure support not implemented"), err0);
		break;
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_datatype_densify(PDI_datatype_t *result, const PDI_datatype_t *oldtype)
{
	PDI_status_t status = PDI_OK;
	
	switch (oldtype->kind) {
	case PDI_K_SCALAR:
		PDI_datatype_init_scalar(result, oldtype->c.scalar);
		break;
		
	case PDI_K_ARRAY: ;
		PDI_datatype_t subtype; PDI_handle_err(PDI_datatype_densify(&subtype, &oldtype->c.array->type), err0);
		PDI_handle_err(PDI_datatype_init_array(result, &subtype, oldtype->c.array->ndims,
		                                       oldtype->c.array->subsizes, oldtype->c.array->subsizes, NULL),
		               err0);
		break;
		
	case PDI_K_STRUCT:
		PDI_handle_err(PDI_make_err(PDI_ERR_VALUE,
		                            "In %s, line %d, Structure cannot be copied (not implemented).", __func__, __LINE__), err0);
		break;
	}
	
	return status;
err0:
	return status;
}


PDI_status_t PDI_datatype_load(PDI_datatype_t *type, PC_tree_t node)
{
	PDI_status_t status = PDI_OK;
	
	// load as a scalar by default
	PDI_type_kind_t kind = PDI_K_SCALAR;
	
	// size or sizes => array
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		if (!PC_status(PC_get(node, ".size"))) kind = PDI_K_ARRAY;
		if (!PC_status(PC_get(node, ".sizes"))) kind = PDI_K_ARRAY;
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	switch (kind) {
	case PDI_K_ARRAY: {// load the type as an array
		PDI_array_type_t *array = (PDI_array_type_t *) malloc(sizeof(PDI_array_type_t));
		PDI_handle_err(array_datatype_load(node, array), err1);
		
		type->c.array = array;
		break;
err1:
		free(array);
		PDI_handle_err(status, err0);
		break;
	}
	case PDI_K_STRUCT: { // load the type as a structure
		PDI_struct_type_t *struct_ = (PDI_struct_type_t *)malloc(sizeof(PDI_struct_type_t));
		PDI_handle_err(struct_datatype_load(node, struct_), err2);
		type->c.struct_ = struct_;
		break;
err2:
		free(struct_);
		PDI_handle_err(status, err0);
		break;
	}
	case PDI_K_SCALAR: { // load the type as a scalar
		PDI_handle_err(scalar_datatype_load(node, &(type->c.scalar)), err0);
		break;
	}
	}
	type->kind = kind;
err0:
	return status;
}


PDI_status_t PDI_datatype_destroy(PDI_datatype_t *type)
{
	PDI_status_t status = PDI_OK;
	
	switch (type->kind) {
	case PDI_K_ARRAY: {
		array_datatype_destroy(type->c.array);
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


PDI_status_t PDI_datatype_is_dense(const PDI_datatype_t *type, int *is_dense)
{
	switch (type->kind) {
	case PDI_K_SCALAR:
		*is_dense = 1;
		return PDI_OK;
		
	case PDI_K_ARRAY:
		return array_datatype_is_dense(type->c.array, is_dense);
		
	case PDI_K_STRUCT:
		return PDI_UNAVAILABLE;
	}
	return PDI_OK;
}


PDI_status_t PDI_datatype_datasize(const PDI_datatype_t *type, size_t *result)
{
	PDI_status_t status = PDI_OK;
	
	buffer_descriptor_t valsz; PDI_handle_err(datatype_bufdesc(type, &valsz), err0);
	PDI_handle_err(bufdesc_datasize(&valsz, result), err1);
	bufdesc_destroy(&valsz);
	
	return status;
	
err1:
	bufdesc_destroy(&valsz);
err0:
	return status;
}


PDI_status_t PDI_datatype_buffersize(const PDI_datatype_t *type, size_t *result)
{
	PDI_status_t status = PDI_OK;
	
	buffer_descriptor_t valsz; PDI_handle_err(datatype_bufdesc(type, &valsz), err0);
	PDI_handle_err(bufdesc_buffersize(&valsz, result), err1);
	bufdesc_destroy(&valsz);
	
	return status;
	
err1:
	bufdesc_destroy(&valsz);
err0:
	return status;
}


PDI_status_t PDI_buffer_copy(void *to, const PDI_datatype_t *to_type, const void *from, const PDI_datatype_t *from_type)
{
	PDI_status_t status = PDI_OK;
	
	buffer_descriptor_t from_desc; PDI_handle_err(datatype_bufdesc(from_type, &from_desc), err0);
	buffer_descriptor_t to_desc; PDI_handle_err(datatype_bufdesc(to_type, &to_desc), err1);
	
	size_t from_size; PDI_handle_err(bufdesc_datasize(&from_desc, &from_size), err2);
	size_t to_size; PDI_handle_err(bufdesc_datasize(&to_desc, &to_size), err2);
	
	// and now for a little defensive programming
	if (from_size != to_size) {
		PDI_handle_err(PDI_make_err(PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size), err2);
	}
	
	PDI_handle_err(buffer_do_copy(to, &to_desc, from, &from_desc), err2);
	bufdesc_destroy(&from_desc);
	bufdesc_destroy(&to_desc);
	
	return status;
	
err2:
	bufdesc_destroy(&to_desc);
err1:
	bufdesc_destroy(&from_desc);
err0:
	return status;
}
