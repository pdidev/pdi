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

#ifndef PDI_DATATYPE_H__
#define PDI_DATATYPE_H__

#include <string.h>

typedef enum type_kind_e {
	SCALAR,
	ARRAY,
	STRUCT
} type_kind_t;

typedef enum scalar_type_e {
	INT8,
	INT16,
	INT32,
	INT64,
	UINT8,
	UINT16,
	UINT32,
	UINT64,
	FLOAT,
	DOUBLE,
	LONG_DOUBLE
} scalar_type_t;

typedef struct array_type_s array_type_t;

typedef struct struct_type_s struct_type_t;

typedef struct PDI_type_s
{
	type_kind_t kind;
	
	union
	{
		scalar_type_t scalar;
		
		array_type_t *array;
		
		struct_type_t *struct_;
		
	};
	
} PDI_type_t;

typedef enum order_e {
	ORDER_C,
	ORDER_FORTRAN
} order_t;

typedef struct array_type_s
{
	int ndims;
	
	//TODO: change to be exprs
	int *array_of_sizes;
	
	//TODO: change to be exprs
	int *array_of_subsizes;
	
	//TODO: change to be exprs
	int *array_of_starts;
	
	order_t order;
	
	PDI_type_t type;
	
} array_type_t;

typedef struct member_s
{
	//TODO: change to be an expr
	int displacement;
	
	PDI_type_t type;
	
} member_t;

typedef struct struct_type_s
{
	size_t nb_member;
	
	member_t *members;
	
} struct_type_t;

#endif // PDI_DATATYPE_H__
