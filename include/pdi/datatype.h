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

#ifndef PDI_DATATYPE_H_
#define PDI_DATATYPE_H_

#include <memory>
#include <vector>

#include <pdi.h>

#include <pdi/datatype_fwd.h>
#include <pdi/value.h>

namespace PDI
{

class Datatype
{
public:
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
	
	/** Creates a undefined datatype
	 */
	constexpr Datatype(): kind{PDI_K_SCALAR}, c{PDI_T_UNDEF} {}
	
	/** Creates a new datatype as the dense copy of this one
	 *
	 * \return the dense type that is produced
	 */
	Datatype densify() const;
	
	/** Creates a new datatype as the exact copy of an existing datatype
	 */
	Datatype(const Datatype &from);
	
	/** Creates a new datatype as the exact copy of an existing datatype
	 */
	Datatype(Datatype &&from);
	
	/** Copy of an existing datatype
	 * \return *this
	 */
	Datatype &operator = (const Datatype &from);
	
	/** Move of an existing datatype
	 * \return *this
	 */
	Datatype &operator = (Datatype &&from);
	
	/** Destroys a Datatype
	 */
	~Datatype();
	
	/** Checks whether the datatype is fully defined (no PDI_T_UNDEF)
	 *
	 * \return whether this is a fully defined type
	 */
	bool is_defined() const;
	
	/** Indicate if the datatype is dense or not
	 *
	 * \return whether the datatype is dense
	 */
	bool is_dense() const;
	
	/** Computes the data size of a type, excluding potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	size_t datasize() const;
	
	/** Computes the data size of a type, including potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	size_t buffersize() const;
	
};

struct Array_datatype {
	struct Dimension {
		Dimension(const Value &size): m_size{size}, m_start{Value::parse("0")}, m_subsize{size} {}
		
		Dimension(const Value &size, const Value &start, const Value &subsize): m_size{size}, m_start{start}, m_subsize{subsize} {}
		
		/* Size of the array in this dimension in term of sub-elements
		 *
		 * The stride for a given dimension is the product of the element size by
		 * all size of dimensions with a lower index.
		 */
		Value m_size;
		
		/* Index of the first sub-element that is actually part of the array in
		 * this dimension
		 */
		Value m_start;
		
		/// Number of elements contained in the array in this dimension,
		Value m_subsize;
		
	};
	
	/* Array of dimensions of the array from outer to inner dim
	 *
	 * The slowest (outer) dimension is m_dimensions[0], the fastest
	 * m_dimensions[m_dimensions.size()-1]
	 */
	std::vector<Dimension> m_dimensions;
	
	/// Type of the elements contained in the array.
	Datatype type;
	
	/** Indicate if the datatype is dense or not
	 *
	 * \return whether the datatype is dense
	 */
	bool is_dense() const;
	
	Array_datatype densify() const;
	
};

struct Record_datatype {
	struct Member {
		/// Offset or distance in octet between the beginning of PDI_struct_type
		Value m_displacement;
		
		Datatype m_type;
		
		std::string m_name;
		
	};
	
	std::vector<Member> m_members;
	
};

/** Creates a new datatype to represent a scalar
 * \param result the datatype to be constructed
 * \param scalar_type the scalar the datatype should represent
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_init_scalar(Datatype *result, Scalar_datatype scalar_type);

/** Creates a new datatype from a paraconf-style config
 * \param node the configuration to read
 * \param result the type to generate
 * \return an exit status code
 */
PDI_status_t PDI_EXPORT PDI_datatype_load(Datatype *result, PC_tree_t node);

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

extern const Datatype PDI_EXPORT PDI_UNDEF_TYPE;

} // namespace PDI

#endif // PDI_DATATYPE_H_
