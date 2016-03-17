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

#include <stdlib.h>
#include <stdint.h>

#include "pdi.h"
#include "pdi/datatype.h"
#include "pdi/value.h"

#include "status.h"
#include "utils.h"

#define PRINTF_BUFFER_SIZE 256

char *vmsprintf(const char *fmt, va_list ap)
{
	int index_size = PRINTF_BUFFER_SIZE;
	char *index = malloc(index_size);
	while ( vsnprintf(index, index_size, fmt, ap) > index_size ) {
		index_size *= 2;
		index = realloc(index, index_size);
	}
	return index;
}

char *msprintf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char *res = vmsprintf(fmt, ap);
	va_end(ap);
	return res;
}

char *mstrcat(char *dest, size_t dlen, const char *src, size_t slen)
{
	char *result = realloc(dest, dlen+slen+1);
	memcpy(result+dlen, src, slen);
	result[dlen+slen] = 0;
	return result;
}

typedef struct val_size_s
{
	int ndims;
	
	int *sizes;
	
	int *subsizes;
	
	int *starts;
	
	int type;
	
} val_size_t;

PDI_status_t size(const PDI_type_t *type, val_size_t *result);

PDI_status_t size_destroy(val_size_t *result)
{
	free(result->sizes);
	free(result->subsizes);
	free(result->starts);
	return PDI_OK;
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
		handle_err(handle_error(PDI_ERR_CONFIG, "Unknown PDI type: #%d", type), err0);
	}
	
	return status;
err0:
	return status;
}

PDI_status_t array_size(const PDI_array_type_t *type, val_size_t *result)
{
	PDI_status_t status = PDI_OK;
	
	handle_err(size(&type->type, result), err0);
	for ( int dim=0; dim<type->ndims; ++dim ) {
		int size; handle_err(PDI_value_int(type->sizes, &size), err1);
		int subsize; handle_err(PDI_value_int(type->subsizes, &subsize), err1);
		if ( (size == subsize) && (result->ndims == 0) ) {
			result->type *= size;
		} else {
			int start; handle_err(PDI_value_int(type->starts, &start), err1);
			++result->ndims;
			result->sizes = realloc(result->sizes, result->ndims*sizeof(int));
			result->subsizes = realloc(result->subsizes, result->ndims*sizeof(int));
			result->starts = realloc(result->starts, result->ndims*sizeof(int));
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
		handle_err(scal_size(type->c.scalar, result), err0);
	} break;
	case PDI_K_ARRAY: {
		handle_err(array_size(type->c.array, result), err0);
	} break;
	case PDI_K_STRUCT: {
		handle_err(handle_error(PDI_ERR_CONFIG, "Not implemented yet"), err0);
	} break;
	}
	
	return status;
err0:
	return status;
}

PDI_status_t dat_size(const PDI_type_t *type, int *result)
{
	PDI_status_t status = PDI_OK;
	
	val_size_t valsz; handle_err(size(type, &valsz), err0);
	*result = valsz.type;
	for ( int dim=0; dim<valsz.ndims; ++dim ) {
		*result *= valsz.subsizes[dim];
	}
	size_destroy(&valsz);
	
	return status;
err0:
	return status;
}

void do_copy(val_size_t *tsize, int dim, void *to, void *from)
{
	if ( dim == tsize->ndims ) {
		memcpy(to, from, tsize->type);
	} else {
		int blocksize = tsize->type;
		for (int ii=dim+1; ii<tsize->ndims; ++ii) blocksize*= tsize->sizes[ii];
		from = (char*)from + tsize->starts[dim]*blocksize;
		for (int ii=0; ii<tsize->subsizes[dim]; ++ii) {
			do_copy(tsize, dim+1, to, from);
			from = (char*)from + blocksize;
			to = (char*)to + blocksize;
		}
		int end = tsize->sizes[dim]-tsize->starts[dim]-tsize->subsizes[dim];
		from = (char*)from + end*blocksize;
	}
}

PDI_status_t tcopy(const PDI_type_t *type, void *to, void *from)
{
	PDI_status_t status = PDI_OK;
	
	val_size_t tsize; handle_err(size(type, &tsize), err0);
	do_copy(&tsize, 0, to, from);
	size_destroy(&tsize);
	
	return status;
err0:
	return status;
}