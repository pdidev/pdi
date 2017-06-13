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

struct PDI_type_s {
	/// The kind of type this describes
	PDI_type_kind_t kind;
	
	union {
		/// The actual descriptor in case of a scalar
		PDI_scalar_type_t scalar;
		
		/// The actual descriptor in case of an array
		PDI_array_type_t *array;
		
		/// The actual descriptor in case of a record
		PDI_struct_type_t *struct_;
		
	} c;
	
};

struct PDI_array_type_s {
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
	PDI_value_t *sizes;
	
	/* Array of subsizes of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is subsizes[0], the fastest subsizes[ndims-1]
	 *
	 * The subsize describes the part of the array actually containing data.
	 */
	PDI_value_t *subsizes;
	
	/* Array of start of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is starts[0], the fastest starts[ndims-1]
	 *
	 * The start is the first index in a given dimension containing data.
	 */
	PDI_value_t *starts;
	
	/// Type of the elements contained in the array.
	PDI_type_t type;
};

struct PDI_member_s {
	/// Offset or distance in octet between the beginning of PDI_struct_type
	PDI_value_t displacement;
	
	PDI_type_t type;
	
	char *name;
	
};

struct PDI_struct_type_s {
	int nb_member;
	
	PDI_member_t *members;
	
};

/** Creates a new datatype to represent a scalar
 * \param result the datatype to be constructed
 * \param scalar_type the scalar the datatype should represent
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_init_scalar(PDI_type_t *result, PDI_scalar_type_t scalar_type);

/** Creates a new datatype to represent a scalar
 * \param result the datatype to be constructed
 * \param elem_type type of an element of the array
 * \param ndims number of dimensions of the array
 * \param sizes size of the buffer in each dimension
 * \param subsizes size of the data inside the buffer in each dimension
 * \param starts first index containing data in each dimension
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_init_array(PDI_type_t *result, const PDI_type_t *elem_type, int ndims,
        const PDI_value_t *sizes, const PDI_value_t *subsizes, const PDI_value_t *starts);

/** Creates a new datatype as the exact copy of an existing datatype
 * \param result the datatype to be constructed
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_copy(PDI_type_t *result, const PDI_type_t *from);

/** Creates a new datatype as the dense copy of an existing type.
 *
 * \param oldtype the type of the (possibly sparse) data whose attributes are used to produce a corresponding dense type.
 * \param result the dense type that is produced using type attributes.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_densify(PDI_type_t *result, const PDI_type_t *oldtype);

/** Creates a new datatype from a paraconf-style config
 * \param node the configuration to read
 * \param result the type to generate
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_load(PDI_type_t *result, PC_tree_t node);

/** Destroys a previously constructed type and frees the associated memory
 *
 * This does not free the memory required for the actual PDI_type_t structure
 *
 * \param type the type to destroy
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_destroy(PDI_type_t *type);

/** Indicate if a given datatype is dense or not
 *
 * \param array_type the type that is checked
 * \param is_dense an integer that stores 1 for scalars and dense arrays and 0 otherwise.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_is_dense(const PDI_type_t *type, int *is_dense);

/** Computes the data size of a type, excluding potentially unused memory from
 * a sparse type
 * \param type the type whose size to compute
 * \param result the size in bytes
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_datasize(const PDI_type_t *type, size_t *result);

/** Computes the data size of a type, including potentially unused memory from
 * a sparse type
 * \param type the type whose size to compute
 * \param result the size in bytes
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_buffersize(const PDI_type_t *type, size_t *result);

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
PDI_status_t PDI_EXPORT PDI_buffer_copy(void *to, const PDI_type_t *to_type, const void *from, const PDI_type_t *from_type);

/** Indicate if a given datatype is dense or not
 *
 * \param array_type the type that is checked
 * \param is_dense an integer that stores 1 for scalars and dense arrays and 0 otherwise.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_is_dense(const PDI_type_t *type, int *is_dense);

#endif // PDI_DATATYPE_H__
