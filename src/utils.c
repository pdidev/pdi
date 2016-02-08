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
		PDI_value_eval(type->subsizes, subsize);
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


PC_status_t PC_copy(PDI_type_t type, void *to, void *from)
{
	
}