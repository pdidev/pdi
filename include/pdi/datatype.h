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
 * \file datatype.h
 * \brief private PDI type declaration
 * \details
 * PDI type (PDI_type_t) handles 3 datatypes that are either scalars, arrays, or structures.
 * Structures are composed of one or mutiple PDI_type_t.
 * Data are managed differently in memory: scalars are copied whereas a pointer is used to access arrays/structures.
 * \author J. Bigot (CEA)
 */

#ifndef PDI_DATATYPE_H__
#define PDI_DATATYPE_H__

#include <pdi.h>

#include <pdi/datatype_fwd.h>
#include <pdi/value.h>

struct PDI_type_s
{
	/// The kind of type this describes
	PDI_type_kind_t kind;
	
	union
	{
		/// The actual descriptor in case of a scalar
		PDI_scalar_type_t scalar;
		
		/// The actual descriptor in case of an array
		PDI_array_type_t *array;
		
		/// The actual descriptor in case of a record
		PDI_struct_type_t *struct_;
		
	} c;
	
};

struct PDI_array_type_s
{
	int ndims;
	
	PDI_value_t *sizes;
	
	PDI_value_t *subsizes;
	
	PDI_value_t *starts;
	
	PDI_type_t type;
	
	PDI_order_t order;
};

struct PDI_member_s
{
	/// Offset or distance in octet between the beginning of PDI_struct_type 
	PDI_value_t displacement;
	 
	PDI_type_t type;
	
	char *name;
	
};

struct PDI_struct_type_s
{
	int nb_member;
	
	PDI_member_t *members;
	
};

/** Computes the data size of a type, excluding potentially unused memory from
 * a sparse type
 * \param type the type whose size to compute
 * \param result the size in bytes
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_data_size(const PDI_type_t* type, size_t* result);

/** Loads a type definition from a paraconf-style config
 * \param node the configuration to read
 * \param type the type to generate
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_load(PC_tree_t node, PDI_type_t* type);

/** Destroys a previously constructed type and frees the associated memory
 * 
 * This does not free the memory required for the actual PDI_type_t structure
 * 
 * \param type the type to destroy
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_destroy(PDI_type_t *type);

/** Copies a buffer into an other 
 * 
 * Content of <from> is copied into <to> if both type are compatible.
 * No allocation is performed by this routine.
 * Please note that if one array is sparse and not the other, only the dense parts
 * are copied. A current limitation is that both arrays can't be sparse.
 * 
 * \param from the buffer that is copied
 * \param from_type the type of the original data
 * \param to the destination of the copy
 * \param to_type the type of the destination data
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_buffer_copy(const void *from, const PDI_type_t *from_type, void *to, const PDI_type_t *to_type);

/** Allocate and create a dense type.
 *
 * Allocate and create a dense type whose subsizes and sizes are equals to the subsizes of a given datatype.
 * Starts values (offset) are set to 0.
 *
 * \param type the type of the (possibly sparse) data whose attributes are used to produce a corresponding dense type.
 * \param dense the dense type that is produced using type attributes.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_densify(const PDI_type_t *type, PDI_type_t *dense);

/** Indicate if a given datatype is dense or not 
 * 
 * \param array_type the type that is checked
 * \param is_dense an integer that stores 1 for scalars and dense arrays and 0 otherwise.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_is_dense(const PDI_type_t *type, int *is_dense);

#endif // PDI_DATATYPE_H__
