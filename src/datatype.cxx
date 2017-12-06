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

#include "config.h"

#include <cstdint>
#include <memory>
#include <string>

#include <paraconf.h>

#include "pdi.h"

#include "pdi/value.h"
#include "pdi/status.h"

#include "pdi/datatype.h"

#define IDX_BUF_SIZE 256
#define EXPR_BUF_SIZE 256

using std::string;
using std::unique_ptr;

namespace PDI {

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


Datatype PDI_UNDEF_TYPE = { PDI_K_SCALAR, { PDI_T_UNDEF } };


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


static PDI_status_t array_datatype_load(PC_tree_t node, Array_datatype *type)
{
	Array_datatype res_type;
	
	// Order: C or fortran ordering, default is C
	PDI_order_t order = PDI_ORDER_C;
	{
		string order_str;
		try {
			char *tmp;
			PC_string(PC_get(node, ".order"), &tmp);
			order_str = tmp;
			free(tmp);
		} catch ( const Error& ) {
			order_str = "c";
		}
		if ( order_str == "c" || order_str == "C" ) {
			order = PDI_ORDER_C;
		} else if ( order_str == "fortran" || order_str == "Fortran" ) {
			order = PDI_ORDER_FORTRAN;
		} else {
			throw Error{PDI_ERR_CONFIG, "Incorrect array ordering: `%s'", order_str.c_str()};
		}
	}
	
	// sizes for a multidim array
	PC_status_t invalid_sizes;
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		invalid_sizes = PC_len(PC_get(node, ".sizes"), &res_type.ndims);
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	if (!invalid_sizes) {   // if multi dim array
		res_type.sizes = (Value *) malloc(res_type.ndims * sizeof(Value));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr;
			PC_string(PC_get(node, ".sizes[%d]", ii), &expr);
			string expr2 = expr;
			free(expr);
			new (&res_type.sizes[ri]) Value{Value::parse(expr2.c_str())};
		}
	} else { // else single dim array
		res_type.ndims = 1;
		res_type.sizes = (Value *) malloc(res_type.ndims * sizeof(Value));
		char *expr; PC_string(PC_get(node, ".size"), &expr);
		string expr2 = expr;
		free(expr);
		new (res_type.sizes) Value{Value::parse(expr2.c_str())};
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
			throw Error{PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", len, res_type.ndims};
		}
	}
	
	if (!invalid_subsizes) {
		res_type.subsizes = (Value *) malloc(res_type.ndims * sizeof(Value));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr; PC_string(PC_get(node, ".subsizes[%d]", ii), &expr);
			string expr2 = expr;
			free(expr);
			new (&res_type.subsizes[ri]) Value{Value::parse(expr2.c_str())};
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
			throw Error{PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", len, res_type.ndims};
		}
	}
	
	res_type.starts = (Value *)malloc(res_type.ndims * sizeof(Value));
	if (!invalid_starts) {
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			int ri = ridx(ii, order, res_type.ndims);
			char *expr; PC_string(PC_get(node, ".starts[%d]", ii), &expr);
			string expr2 = expr;
			free(expr);
			new (&res_type.starts[ri]) Value{Value::parse(expr2.c_str())};
		}
	} else { // no start, start at 0 everywhere
		res_type.starts = (Value *) malloc(res_type.ndims * sizeof(Value));
		for (int ii = 0; ii < res_type.ndims; ++ii) {
			new (&res_type.starts[ii]) Value{Value::parse("0")};
		}
	}
	
	PDI_datatype_load(&res_type.type, PC_get(node, ".type"));
	
	*type = res_type;
	return PDI_OK;
}


static PDI_status_t struct_datatype_load(PC_tree_t, Record_datatype *)
{
	throw Error{PDI_ERR_IMPL, "Structure support not implemented yet"};
}


static PDI_status_t scalar_datatype_load(PC_tree_t node, Scalar_datatype *type)
{
	string tname;
	long kind = 0;
	try {
		char * ctname;
		PC_string(PC_get(node, ".type"), &ctname);
		tname = ctname;
		free(ctname);
		PC_int(PC_get(node, ".kind"), &kind);
	} catch ( const Error& ) {
		char * ctname;
		PC_string(node, &ctname);
		tname = ctname;
		free(ctname);
	}
	
	// For Fortran, we assume kind means number of bytes... TODO: autodetect
	if ( tname == "char" && kind == 0) { // C char
		*type = PDI_T_INT8;
	} else if ( tname == "int" && kind == 0) { // C int
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
			throw Error{PDI_ERR_VALUE, "Unsupported int size: %d", sizeof(int) * 8};
		}
	} else if ( tname == "int8"  && kind == 0) { // C int8
		*type = PDI_T_INT8;
	} else if ( tname == "int16"  && kind == 0) { // C int16
		*type = PDI_T_INT16;
	} else if ( tname == "int32"  && kind == 0) { // C int32
		*type = PDI_T_INT32;
	} else if ( tname == "int64"  && kind == 0) { // C int64
		*type = PDI_T_INT64;
	} else if ( tname == "float"  && kind == 0) { // C float
		*type = PDI_T_FLOAT;
	} else if ( tname == "double"  && kind == 0) { // C double
		*type = PDI_T_DOUBLE;
	} else if ( tname == "character" ) {   // Fortran character
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
			throw Error{PDI_ERR_VALUE, "Invalid kind for character: `%s'", tname.c_str()};
		}
	} else if ( tname == "integer" ) {   // Fortran integer
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
			throw Error{PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname};
		}
	} else if ( tname == "logical" ) {   // Fortran integer
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
			throw Error{PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname.c_str()};
		}
	} else if ( tname == "real" ) {   // Fortran real
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
			throw Error{PDI_ERR_VALUE, "Invalid kind for real: `%s'", tname.c_str()};
		}
	} else {
		throw Error{PDI_ERR_VALUE, "Invalid scalar type: `%s'", tname.c_str()};
	}
	
	return PDI_OK;
}


static PDI_status_t array_datatype_destroy(Array_datatype *type)
{
	PDI_datatype_destroy(&type->type);
	// don't free subsizes in case it was a copy of sizes
	if (type->subsizes != type->sizes) free(type->subsizes);
	
	free(type->sizes);
	free(type->starts);
	
	return PDI_OK;
}


static PDI_status_t array_datatype_is_dense(const Array_datatype *type, int *is_dense)
{
	for (int dim = 0; dim < type->ndims; ++dim) {
		if (type->sizes[dim].to_long() != type->subsizes[dim].to_long()) {
			*is_dense = 0;
			return PDI_OK;
		}
	}
	
	*is_dense = 1;
	return PDI_OK;
}


static PDI_status_t datatype_bufdesc(const Datatype *type, buffer_descriptor_t *result);


static PDI_status_t scalar_datatype_bufdesc(Scalar_datatype type, buffer_descriptor_t *result)
{
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
		throw Error{PDI_ERR_CONFIG, "Unknown PDI type: #%d", type};
	}
	
	return PDI_OK;
}


static PDI_status_t array_datatype_bufdesc(const Array_datatype *type, buffer_descriptor_t *result)
{
	datatype_bufdesc(&type->type, result);
	for (int dim = 0; dim < type->ndims; ++dim) {
		long size = type->sizes[dim];
		long subsize = type->subsizes[dim];
		if ((size == subsize) && (result->ndims == 0)) {   // dense case
			result->dense_size *= size;
		} else { // sparse case
			long start = type->starts[dim];
			++result->ndims;
			result->sizes    = (size_t *) realloc(result->sizes, result->ndims * sizeof(size_t));
			result->subsizes = (size_t *) realloc(result->subsizes, result->ndims * sizeof(size_t));
			result->starts   = (size_t *) realloc(result->starts, result->ndims * sizeof(size_t));
			result->sizes[result->ndims - 1] = size;
			result->subsizes[result->ndims - 1] = subsize;
			result->starts[result->ndims - 1] = start;
		}
	}
	
	return PDI_OK;
}


static PDI_status_t datatype_bufdesc(const Datatype *type, buffer_descriptor_t *result)
{
	switch (type->kind) {
	case PDI_K_SCALAR: {
		scalar_datatype_bufdesc(type->c.scalar, result);
	} break;
	case PDI_K_ARRAY: {
		array_datatype_bufdesc(type->c.array, result);
	} break;
	case PDI_K_STRUCT: {
		throw Error{PDI_ERR_IMPL, "Not implemented yet"};
	} break;
	}
	
	return PDI_OK;
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
	size_t from_size; bufdesc_datasize(from_desc, &from_size);
	size_t to_size; bufdesc_datasize(to_desc, &to_size);
	
	// and now for a little defensive programming
	if (from_size != to_size) {
		throw Error{PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size};
	}
	
	
	if (0 == from_desc->ndims && 0 == to_desc->ndims) {   // dense to dense
		if (from_desc->dense_size != to_desc->dense_size) {
			throw Error{PDI_ERR_TYPE, "Incompatible type size for dense copy: %ld vs. %ld .", (long)from_desc->dense_size, (long)to_desc->dense_size};
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
				throw Error{PDI_ERR_TYPE, "Incompatible array type size for copy."};
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
	
	return PDI_OK;
}


// public functions


PDI_status_t PDI_datatype_init_scalar(Datatype *dest, Scalar_datatype value)
{
	dest->kind = PDI_K_SCALAR;
	dest->c.scalar = value;
	return PDI_OK;
}


PDI_status_t PDI_datatype_init_array(Datatype *dest, const Datatype *type, int ndims,
                                     const Value *sizes, const Value *subsizes, const Value *starts)
{
	Array_datatype *array = (Array_datatype *) malloc(sizeof(Array_datatype));
	
	array->ndims = ndims;
	
	array->sizes = (Value *) malloc(ndims * sizeof(Value));
	for (int ii = 0; ii < ndims; ++ii) {
		new (&array->sizes[ii]) Value{sizes[ii]};
	}
	
	if (sizes == subsizes) {
		array->subsizes = array->sizes;
	} else {
		array->subsizes = (Value *)malloc(ndims * sizeof(Value));
		for (int ii = 0; ii < ndims; ++ii) {
			new (&array->subsizes[ii]) Value{subsizes[ii]};
		}
	}
	
	array->starts = (Value *)malloc(ndims * sizeof(Value));
	if (!starts) {
		for (int ii = 0; ii < ndims; ++ii) {
			new (&array->starts[ii]) Value{Value::parse("0")};
		}
	} else {
		for (int ii = 0; ii < ndims; ++ii) {
			new (&array->starts[ii]) Value{starts[ii]};
		}
	}
	
	PDI_datatype_copy(&array->type, type);
	
	dest->kind = PDI_K_ARRAY;
	dest->c.array = array;
	
	return PDI_OK;
}


PDI_status_t PDI_datatype_copy(Datatype *copy, const Datatype *from)
{
	switch (from->kind) {
	case PDI_K_SCALAR:
		PDI_datatype_init_scalar(copy, from->c.scalar);
		break;
	case PDI_K_ARRAY:
		PDI_datatype_init_array(copy, &from->c.array->type, from->c.array->ndims,
		                        from->c.array->sizes, from->c.array->subsizes,
		                        from->c.array->starts);
		break;
	case PDI_K_STRUCT:
		throw Error{PDI_UNAVAILABLE, "Structure support not implemented"};
		break;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_datatype_densify(Datatype *result, const Datatype *oldtype)
{
	switch (oldtype->kind) {
	case PDI_K_SCALAR:
		PDI_datatype_init_scalar(result, oldtype->c.scalar);
		break;
		
	case PDI_K_ARRAY: ;
		Datatype subtype; PDI_datatype_densify(&subtype, &oldtype->c.array->type);
		PDI_datatype_init_array(result, &subtype, oldtype->c.array->ndims,
		                        oldtype->c.array->subsizes,
		                        oldtype->c.array->subsizes, NULL);
		break;
	case PDI_K_STRUCT:
		throw Error{PDI_ERR_IMPL, "Structure support not implemented"};
		break;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_datatype_load(Datatype *type, PC_tree_t node)
{
	// load as a scalar by default
	Datatype_kind kind = PDI_K_SCALAR;
	
	// size or sizes => array
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		if (!PC_status(PC_get(node, ".size"))) kind = PDI_K_ARRAY;
		if (!PC_status(PC_get(node, ".sizes"))) kind = PDI_K_ARRAY;
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	switch (kind) {
	case PDI_K_ARRAY: {// load the type as an array
		unique_ptr<Array_datatype, decltype(&free)> array {static_cast<Array_datatype *>(malloc(sizeof(Array_datatype))), free};
		array_datatype_load(node, array.get());
		type->c.array = array.release();
		break;
	}
	case PDI_K_STRUCT: { // load the type as a structure
		unique_ptr<Record_datatype, decltype(&free)> struct_ {static_cast<Record_datatype *>(malloc(sizeof(Record_datatype))), free};
		struct_datatype_load(node, struct_.get());
		type->c.struct_ = struct_.release();
		break;
	}
	case PDI_K_SCALAR: { // load the type as a scalar
		scalar_datatype_load(node, &(type->c.scalar));
		break;
	}
	}
	type->kind = kind;
	return PDI_OK;
}


PDI_status_t PDI_datatype_destroy(Datatype *type)
{
	switch (type->kind) {
	case PDI_K_ARRAY: {
		array_datatype_destroy(type->c.array);
		free(type->c.array);
	} break;
	case PDI_K_STRUCT: {
		throw Error{PDI_ERR_IMPL, "Structs not implemented yet"};
		free(type->c.struct_);
	} break;
	case PDI_K_SCALAR: {
		// nothing to do here
	} break;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_datatype_is_dense(const Datatype *type, int *is_dense)
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


PDI_status_t PDI_datatype_datasize(const Datatype *type, size_t *result)
{
	buffer_descriptor_t valsz; datatype_bufdesc(type, &valsz);
	try {
		bufdesc_datasize(&valsz, result);
	} catch(...) {
		bufdesc_destroy(&valsz);
		throw;
	}
	bufdesc_destroy(&valsz);
	
	return PDI_OK;
}


PDI_status_t PDI_datatype_buffersize(const Datatype *type, size_t *result)
{
	buffer_descriptor_t valsz; datatype_bufdesc(type, &valsz);
	try {
		bufdesc_buffersize(&valsz, result);
	} catch(...) {
		bufdesc_destroy(&valsz);
		throw;
	}
	bufdesc_destroy(&valsz);
	
	return PDI_OK;
}


PDI_status_t PDI_buffer_copy(void *to, const Datatype *to_type, const void *from, const Datatype *from_type)
{
	buffer_descriptor_t from_desc; datatype_bufdesc(from_type, &from_desc);
	try {
		buffer_descriptor_t to_desc; datatype_bufdesc(to_type, &to_desc);
		try {
			size_t from_size; bufdesc_datasize(&from_desc, &from_size);
			size_t to_size; bufdesc_datasize(&to_desc, &to_size);
			
			// and now for a little defensive programming
			if (from_size != to_size) {
				throw Error{PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size};
			}
			
			buffer_do_copy(to, &to_desc, from, &from_desc);
			
			bufdesc_destroy(&from_desc);
			bufdesc_destroy(&to_desc);
		} catch ( ... ) {
			bufdesc_destroy(&to_desc);
			throw;
		}
	} catch ( ... ) {
		bufdesc_destroy(&from_desc);
		throw;
	}
	return PDI_OK;
}

}
