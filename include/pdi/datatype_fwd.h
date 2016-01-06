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

#ifndef PDI_DATATYPE_FWD_H__
#define PDI_DATATYPE_FWD_H__

typedef struct PDI_type_s PDI_type_t;

typedef enum PDI_type_kind_e {
	SCALAR,
	ARRAY,
	STRUCT
} PDI_type_kind_t;

typedef struct PDI_array_type_s PDI_array_type_t;

typedef enum PDI_order_e {
	ORDER_C,
	ORDER_FORTRAN
} PDI_order_t;

typedef struct PDI_struct_type_s PDI_struct_type_t;

typedef struct PDI_member_s PDI_member_t;

typedef enum PDI_scalar_type_e {
	PDI_T_INT8,
	PDI_T_INT16,
	PDI_T_INT32,
	PDI_T_INT64,
	PDI_T_UINT8,
	PDI_T_UINT16,
	PDI_T_UINT32,
	PDI_T_UINT64,
	PDI_T_FLOAT,
	PDI_T_DOUBLE,
	PDI_T_LONG_DOUBLE
} PDI_scalar_type_t;

#endif // PDI_DATATYPE_FWD_H__
