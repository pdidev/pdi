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

#include <pdi.h>

#include <pdi/datatype_fwd.h>
#include <pdi/value.h>

namespace PDI
{

class Datatype
{
public:
	/** Checks whether the datatype is fully defined (no PDI_T_UNDEF)
	 *
	 * \return whether this is a fully defined type
	 */
	bool is_defined() const;
	
	/// The kind of type this describes
	Datatype_kind kind;
	
	union {
		/// The actual descriptor in case of a scalar
		Scalar_datatype scalar;
		
		/// The actual descriptor in case of an array
		Array_datatype *array;
		
		/// The actual descriptor in case of a record
		Record_datatype *struct_;
		
	} c;
};

struct Array_datatype {

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
	PDI::Value *sizes;
	
	/* Array of subsizes of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is subsizes[0], the fastest subsizes[ndims-1]
	 *
	 * The subsize describes the part of the array actually containing data.
	 */
	PDI::Value *subsizes;
	
	/* Array of start of the array from outer to inner dim
	 *
	 * The array size is ndims.
	 * The slowest dimension is starts[0], the fastest starts[ndims-1]
	 *
	 * The start is the first index in a given dimension containing data.
	 */
	PDI::Value *starts;
	
	/// Type of the elements contained in the array.
	Datatype type;
};

struct Record_datatype {
	struct Member {
		/// Offset or distance in octet between the beginning of PDI_struct_type
		PDI::Value displacement;
		
		Datatype type;
		
		char *name;
		
	};
	
	int nb_member;
	
	Member *members;
	
};

/** Creates a new datatype to represent a scalar
 * \param result the datatype to be constructed
 * \param scalar_type the scalar the datatype should represent
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_init_scalar(Datatype *result, Scalar_datatype scalar_type);

/** Creates a new datatype to represent a scalar
 * \param result the datatype to be constructed
 * \param elem_type type of an element of the array
 * \param ndims number of dimensions of the array
 * \param sizes size of the buffer in each dimension
 * \param subsizes size of the data inside the buffer in each dimension
 * \param starts first index containing data in each dimension
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_init_array(Datatype *result, const Datatype *elem_type, int ndims,
        const PDI::Value *sizes, const PDI::Value *subsizes, const PDI::Value *starts);

/** Creates a new datatype as the exact copy of an existing datatype
 * \param result the datatype to be constructed
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_copy(Datatype *result, const Datatype *from);

/** Creates a new datatype as the dense copy of an existing type.
 *
 * \param oldtype the type of the (possibly sparse) data whose attributes are used to produce a corresponding dense type.
 * \param result the dense type that is produced using type attributes.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_densify(Datatype *result, const Datatype *oldtype);

/** Creates a new datatype from a paraconf-style config
 * \param node the configuration to read
 * \param result the type to generate
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_load(Datatype *result, PC_tree_t node);

/** Destroys a previously constructed type and frees the associated memory
 *
 * This does not free the memory required for the actual PDI_datatype_t structure
 *
 * \param type the type to destroy
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_destroy(Datatype *type);

/** Indicate if a given datatype is dense or not
 *
 * \param array_type the type that is checked
 * \param is_dense an integer that stores 1 for scalars and dense arrays and 0 otherwise.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_is_dense(const Datatype *type, int *is_dense);

/** Computes the data size of a type, excluding potentially unused memory from
 * a sparse type
 * \param type the type whose size to compute
 * \param result the size in bytes
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_datasize(const Datatype *type, size_t *result);

/** Computes the data size of a type, including potentially unused memory from
 * a sparse type
 * \param type the type whose size to compute
 * \param result the size in bytes
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_buffersize(const Datatype *type, size_t *result);

/** Copies a buffer into an other
 *
 * Content of <from> is copied into <to> if both type are compatible.
 * No allocation is performed by this routine.
 * Please note that if one array is sparse and not the other, only the dense parts
 * are copied. A current limitation is that both arrays can't be sparse.
 *
 * \param to the destination of the copy
 * \param to_type the type of the destination data
 * \param from the buffer that is copied
 * \param from_type the type of the original data
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_buffer_copy(void *to, const Datatype *to_type, const void *from, const Datatype *from_type);

/** Indicate if a given datatype is dense or not
 *
 * \param[in] type the type that is checked
 * \param[out] is_dense an integer that stores 1 for scalars and dense arrays and 0 otherwise.
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_is_dense(const Datatype *type, int *is_dense);

extern Datatype PDI_EXPORT PDI_UNDEF_TYPE;

} // namespace PDI

#endif // PDI_DATATYPE_H__
