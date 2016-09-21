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
 * \file datatype_fwd.h
 * \brief public PDI type declaration
 * \details
 * PDI type (PDI_type_t) handles 3 datatypes that are either scalars, arrays, or structures 
 * (respectively PDI_K_SCALAR, PDI_K_ARRAY, PDI_K_STRUCT).
 * Scalar are currently restricted to int8 to int64 or float to long double.
 * Other scalars cannot be used (real16 for instance)
 * \author J. Bigot (CEA)
 */

#ifndef PDI_DATATYPE_FWD_H__
#define PDI_DATATYPE_FWD_H__

/// A PDI type descriptor
typedef struct PDI_type_s PDI_type_t;

/// The possible kinds of type
typedef enum PDI_type_kind_e {
	/// A scalar
	PDI_K_SCALAR,
	/// An array
	PDI_K_ARRAY,
	/// A record (a.k.a. C-style structure)
	PDI_K_STRUCT
} PDI_type_kind_t;

/// A PDI array type descriptor
typedef struct PDI_array_type_s PDI_array_type_t;

//TODO: ordering of array, implement 
// typedef enum PDI_order_e {
// 	PDI_ORDER_C,
// 	PDI_ORDER_FORTRAN
// } PDI_order_t;

/// A PDI record type descriptor
typedef struct PDI_struct_type_s PDI_struct_type_t;

/// A record member descriptor
typedef struct PDI_member_s PDI_member_t;

/// A PDI scalar type descriptor
typedef enum PDI_scalar_type_e {
	/// A 8 bits integer
	PDI_T_INT8,
	/// A 16 bits integer
	PDI_T_INT16,
	/// A 32 bits integer
	PDI_T_INT32,
	/// A 64 bits integer
	PDI_T_INT64,
	/// A 32 bits IEEE ??? floating point value
	PDI_T_FLOAT,
	/// A 64 bits IEEE ??? floating point value
	PDI_T_DOUBLE,
	/// A 128 bits IEEE ??? floating point value
	PDI_T_LONG_DOUBLE,
	/// undefined type
	PDI_T_UNDEF
} PDI_scalar_type_t;

#endif // PDI_DATATYPE_FWD_H__
