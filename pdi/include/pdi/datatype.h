/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <string>
#include <utility>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>

namespace PDI {

/** A Datatype is a Datatype_template that accepts no argument.
 *
 * It represents the memory layout of data and supports some simple operations
 * on it:
 * * accessing its content
 * * data copy and destruction
 */
class PDI_EXPORT Datatype: public Datatype_template
{
public:
	/** Creates a new datatype
	 *
	 * \param[in] attributes attributes of the datatype
	 */
	Datatype(const Attributes_map& attributes = {});

	~Datatype() override;

	/** Test for equality
	 *
	 * \param other the Datatype to compare
	 * \return true if the Datatype's are equal
	 */
	virtual bool operator== (const Datatype& other) const = 0;

	/** Test for inequality
	 *
	 * \param other the Datatype to compare
	 * \return true if the Datatype's are different
	 */
	bool operator!= (const Datatype& other) const;

	/** Creates a new datatype as the dense copy of this one
	 *
	 * \return the type that is produced
	 */
	virtual Datatype_sptr densify() const = 0;

	/** Indicate if the datatype is dense or not
	 *
	 * \return whether the datatype is dense
	 */
	virtual bool dense() const = 0;

	/** Computes the data size of a type, excluding potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t datasize() const = 0;

	/** Computes the data size of a type, including potentially unused memory
	 *  from a sparse type
	 *
	 * \return the size in bytes
	 */
	virtual size_t buffersize() const = 0;

	/** Returns the required alignment for a type
	 *
	 * \return the size in bytes
	 */
	virtual size_t alignment() const = 0;

	/** Tells if data can be copied as bytes (if type is dense) and doesn't need a destroyer
	 *
	 * \return true if data has trivial copier and destroyer, false otherwise
	 */
	virtual bool simple() const = 0;

	/** Creates a dense deep copy of data
	 *
	 * \param[in] to the pointer to the allocated memory to fill (dense data)
	 * \param[in] from the pointer to the copied data (size of buffersize)
	 * \return updated `to' pointer
	 */
	virtual void* data_to_dense_copy(void* to, const void* from) const = 0;

	/** Creates a sparse deep copy of dense data
	 *
	 * \param[in] to the pointer to the allocated memory to fill (size of buffersize)
	 * \param[in] from the pointer to the copied data (dense data)
	 * \return updated `to' pointer
	 */
	virtual void* data_from_dense_copy(void* to, const void* from) const = 0;

	/** \anchor Datatype_index_size_t
	 * Access the type of the element at the provided index
	 *
	 * \param index the index where to look
	 * \return the Datatype of the indexed sub-element
	 */
	virtual Datatype_sptr index(size_t index) const;

	/** Access the type and value of the element at the provided index
	 *
	 * \param index the index where to look
	 * \param data the address of the element to index
	 * \return the Datatype and address of the indexed sub-element
	 */
	virtual std::pair<void*, Datatype_sptr> index(size_t index, void* data) const;

	/** Access the type of the elements slice between the provided indices
	 *
	 * \param start_index the index where to start
	 * \param end_index the index where to end
	 * \return the Datatype of the slice
	 */
	virtual Datatype_sptr slice(size_t start_index, size_t end_index) const;

	/** Access the type and value of the elements slice between the provided indices
	 *
	 * \param start_index the index where to start
	 * \param end_index the index where to end
	 * \param data the address of the element to slice
	 * \return the Datatype and address of the slice
	 */
	virtual std::pair<void*, Datatype_sptr> slice(size_t start_index, size_t end_index, void* data) const;

	/** Access the type of the member with the provided name
	 *
	 * \param name the name of the member to access
	 * \return the Datatype of the member
	 */
	virtual Datatype_sptr member(const char* name) const;

	/** Access the type and value of the member with the provided name
	 *
	 * \param name the name of the member to access
	 * \param data the address of the element whose member to access
	 * \return the Datatype and address of the member
	 */
	virtual std::pair<void*, Datatype_sptr> member(const char* name, void* data) const;

	/** Access the type referenced by this
	 *
	 * \return the Datatype referenced
	 */
	virtual Datatype_sptr dereference() const;

	/** Access the type and value referenced by this
	 *
	 * \param data the address of the reference
	 * \return the Datatype and address referenced
	 */
	virtual std::pair<void*, Datatype_sptr> dereference(void* data) const;

	/** Delete data whose type is described by the Datatype.
	 *
	 * This does not deallocate the buffer used to store the data.
	 *
	 * \param[in] ptr to the data to free
	 */
	virtual void destroy_data(void* ptr) const = 0;

	/** Returns the datatype yaml representation as a string
	 *
	 * \return the datatype yaml representation as a string
	 */
	virtual std::string debug_string() const = 0;
};

} // namespace PDI

#endif // PDI_DATATYPE_H_
