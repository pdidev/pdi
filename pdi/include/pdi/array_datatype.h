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

#ifndef PDI_ARRAY_DATATYPE_H_
#define PDI_ARRAY_DATATYPE_H_

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>
#include <pdi/tuple_datatype.h>

namespace PDI {

/**
 * an Array_datatype is a Datatype that represents an array: i.e storage
 * of multiple elements of the same type continuously in memory.
 */
class PDI_EXPORT Array_datatype: public Datatype
{
	// Required to make_shared due to private ctor
	struct Shared_enabler;

	/// Type of the elements contained in the array.
	Datatype_sptr m_subtype;

	/// Number of elements the array can store
	size_t m_size;

	/// id of the first actual element of the array
	size_t m_start;

	/// Number of actual elements in the array
	size_t m_subsize;

public:
	/** Type of the elements contained in the array
	 *
	 * \return the type of the elements contained in the array
	 */
	Datatype_sptr subtype() const;

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

	Datatype_sptr densify() const override;

	Datatype_sptr evaluate(Context&) const override;

	bool dense() const override;

	size_t datasize() const override;

	size_t buffersize() const override;

	size_t alignment() const override;

	bool simple() const override;

	void* data_to_dense_copy(void*, const void*) const override;

	void* data_from_dense_copy(void*, const void*) const override;

	/// \copydoc PDI::Datatype::index(size_t) const
	Datatype_sptr index(size_t index) const override;

	std::pair<void*, Datatype_sptr> index(size_t index, void* data) const override;

	Datatype_sptr slice(size_t start_index, size_t end_index) const override;

	std::pair<void*, Datatype_sptr> slice(size_t start_index, size_t end_index, void* data) const override;

	void destroy_data(void*) const override;

	std::string debug_string() const override;

	bool operator== (const Datatype&) const override;

	/** Construct a new partially filled Array_datatype
	 *
	 * \param[in] subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 * \param start the id of the first actual element of the array
	 * \param subsize the number of actual elements in the array
	 * \param attributes attributes of the array
	 */
	static std::shared_ptr<Array_datatype>
	make(Datatype_sptr subtype, size_t size, size_t start, size_t subsize, const Attributes_map& attributes = {});

	/** Construct a new completely filled Array_datatype
	 *
	 * \param subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 * \param attributes attributes of the array
	 */
	static std::shared_ptr<Array_datatype> make(Datatype_sptr subtype, size_t size, const Attributes_map& attributes = {});

private:
	/** Construct a new partially filled Array_datatype
	 *
	 * \param[in] subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 * \param start the id of the first actual element of the array
	 * \param subsize the number of actual elements in the array
	 * \param attributes attributes of the array
	 */
	Array_datatype(Datatype_sptr subtype, size_t size, size_t start, size_t subsize, const Attributes_map& attributes = {});

	/** Construct a new completely filled Array_datatype
	 *
	 * \param subtype the type of the elements contained in the array
	 * \param size the number of elements the array can store
	 * \param attributes attributes of the array
	 */
	Array_datatype(Datatype_sptr subtype, size_t size, const Attributes_map& attributes = {});
};

} // namespace PDI

#endif // PDI_ARRAY_DATATYPE_H_
