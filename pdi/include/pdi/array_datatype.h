/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_ARRAY_DATATYPE_H_
#define PDI_ARRAY_DATATYPE_H_

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>

namespace PDI {

/**
 * an Array_datatype is a Datatype that represents an array: i.e storage
 * of multiple elements of the same type continuously in memory.
 */
class PDI_EXPORT Array_datatype:
	public Datatype
{
	/// Type of the elements contained in the array.
	Datatype_uptr m_subtype;
	
	/// Number of elements the array can store
	size_t m_size;
	
	/// id of the first actual element of the array
	size_t m_start;
	
	/// Number of actual elements in the array
	size_t m_subsize;
	
public:
	/** Accessor to get single element from array
	 */
	class Index_accessor : public Datatype::Accessor_base
	{
		/// Index of element that will be returned
		size_t m_index;
		
		std::string access_kind() const override;
	public:
		/** Construct a new index accessor
		 *
		 * \param index index of element that will be returned
		 */
		Index_accessor(size_t index);
		
		std::pair<void*, Datatype_uptr> access(const Array_datatype& array_type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	};
	
	/** Accessor to get a slice of an array, returns array of the same subtype
	 */
	class Slice_accessor : public Datatype::Accessor_base
	{
		/// Start index of the slice
		size_t m_start;
		
		/// End index of the slice
		size_t m_end;
		
		/** Returns size of the slice
		 *  \return size of the slice
		 */
		size_t size() const;
		
		std::string access_kind() const override;
	public:
		/** Construct a new slice accessor
		 *
		 * \param start start index of the slice
		 * \param end end index of the slice
		 */
		Slice_accessor(size_t start, size_t end);
		
		std::pair<void*, Datatype_uptr> access(const Array_datatype& array_type,
		    void* from,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
		    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	};
	
	/** Construct a new partially filled Array_datatype
	 *
	 * \param[in] subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 * \param start the id of the first actual element of the array
	 * \param subsize the number of actual elements in the array
	 */
	Array_datatype(Datatype_uptr subtype, size_t size, size_t start, size_t subsize);
	
	/** Construct a new completely filled Array_datatype
	 *
	 * \param subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 */
	Array_datatype(Datatype_uptr subtype, size_t size);
	
	/** Type of the elements contained in the array
	 *
	 * \return the type of the elements contained in the array
	 */
	const Datatype& subtype() const;
	
	/** Number of elements the array can store
	 * \return the number of elements the array can store
	 */
	size_t size() const;
	
	/** id of the first actual element of the array
	 *
	 * \return the id of the first actual element of the array
	 */
	size_t start() const;
	
	/** Number of actual elements in the array
	 *
	 * \return the number of actual elements in the array
	 */
	size_t subsize() const;
	
	Datatype_template_uptr clone() const override;
	
	Datatype_uptr clone_type() const override;
	
	Datatype_uptr densify() const override;
	
	Datatype_uptr evaluate(Context&) const override;
	
	bool dense() const override;
	
	size_t datasize() const override;
	
	size_t buffersize() const override;
	
	size_t alignment() const override;
	
	bool simple() const override;
	
	void* data_to_dense_copy(void*, const void*) const override;
	
	void* data_from_dense_copy(void*, const void*) const override;
	
	std::pair<void*, Datatype_uptr> subaccess_by_iterators(void* from,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_begin,
	    std::vector<std::unique_ptr<Accessor_base>>::const_iterator remaining_end) const override;
	    
	void destroy_data(void*) const override;
	
	std::string debug_string() const override;
	
	bool operator== (const Datatype&) const override;
	
};

} // namespace PDI

#endif // PDI_ARRAY_DATATYPE_H_
