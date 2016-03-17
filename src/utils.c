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

#include "pdi.h"
#include "pdi/datatype.h"
#include "pdi/value.h"

#include "utils.h"

#define PRINTF_BUFFER_SIZE 256

unsigned long scal_size(PDI_scalar_type_t type)
{
	switch ( type ) {
	case PDI_T_INT8: return sizeof(int8_t);
	case PDI_T_INT16: return sizeof(int16_t);
	case PDI_T_INT32: return sizeof(int32_t);
	case PDI_T_INT64: return sizeof(int64_t);
	case PDI_T_FLOAT: return sizeof(float);
	case PDI_T_DOUBLE: return sizeof(double);
	case PDI_T_LONG_DOUBLE: return sizeof(long double);
	}
	abort();
}


unsigned long array_dat_size(PDI_array_type_t *type)
{
	unsigned long result = PDI_dat_size(&type->type);
	int dim;
	for ( dim=0; dim<type->ndims; ++dim ) {
		int subsize;
		//TODO: handle error here
		PDI_value_int(type->subsizes, &subsize);
		result *= subsize;
	}
	return result;
}

unsigned long PDI_dat_size(PDI_type_t *type)
{
	switch( type->kind ) {
		case PDI_K_SCALAR: return scal_size(type->c.scalar);
		case PDI_K_ARRAY: return array_dat_size(type->c.array);
		//TODO: implement structs
	}
	abort();
}

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

PDI_status_t PDI_copy(PDI_type_t *type, void **to, void *from)
{
	//TODO: implement
	*to = from;
}