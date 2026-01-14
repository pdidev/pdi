/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_TUPLE_DATATYPE_H_
#define PDI_TUPLE_DATATYPE_H_

#include <string>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>

namespace PDI {

/** A Tuple_datatype is a Datatype that represents a fixed number of elements of
 * potentially different types layed out in a specific way in memory.
 * Each element is given a index to access it.
 */
class PDI_EXPORT Tuple_datatype: public Datatype
{
	// Required to make_shared due to private ctor
	struct Shared_enabler;

public:
	/** A Element is one of the elements inside a Tuple_datatype
	 */
	class Element
	{
		/// Offset in byte from the Tuple_datatype start
		size_t m_offset;

		/// Type of the contained element
		Datatype_sptr m_type;

	public:
		/** Construct a new element
		 *
		 * \param offset offset in byte from the Tuple_datatype start
		 * \param type type of the contained element
		 */
		Element(size_t offset, Datatype_sptr type);

		/** Construct a new element by copy
		 *
		 * \param o the element to copy
		 */
		Element(const Element& o);

		/** Access the offset in byte from the Tuple_datatype start
		 *
		 * \return the offset in byte from the Tuple_datatype start
		 */
		size_t offset() const;

		/** Access the type of the contained element
		 *
		 * \return the type of the contained element
		 */
		Datatype_sptr type() const;

		/** Tests another element for equality
		 *
		 * \param rhs the other element to compare
		 * \return true if the elements are equal
		 */
		bool operator== (const Element& rhs) const;

		/** Tests another element for inequality
		 *
		 * \param rhs the other element to compare
		 * \return true if the elements are different
		 */
		bool operator!= (const Element& rhs) const;
	};

private:
	/// All elements in increasing offset order
	std::vector<Element> m_elements;

	/// The total size of the buffer containing all elementrs
	size_t m_buffersize;

public:
	/** Accesses the elements in increasing offset order
	 */
	const std::vector<Element>& elements() const;

	/** Number of elements of the tuple
	 * \return the number of elements of the tuple
	 */
	size_t size() const;

	Datatype_sptr densify() const override;

	Datatype_sptr evaluate(Context&) const override;

	bool dense() const override;

	size_t datasize() const override;

	size_t buffersize() const override;

	size_t alignment() const override;

	bool simple() const override;

	void* data_to_dense_copy(void*, const void*) const override;

	void* data_from_dense_copy(void*, const void*) const override;

	/// \copydoc PDI::Datatype::Datatype_index_size_t
	Datatype_sptr index(size_t) const override;

	std::pair<void*, Datatype_sptr> index(size_t, void*) const override;

	Datatype_sptr slice(size_t, size_t) const override;

	std::pair<void*, Datatype_sptr> slice(size_t, size_t, void*) const override;

	void destroy_data(void*) const override;

	std::string debug_string() const override;

	bool operator== (const Datatype&) const override;

private:
	/** Constructs a new Tuple_datatype
	 *
	 * \param elements the elements for the newly created Tuple_datatype in
	 *        increasing offset order
	 * \param buffersize the total size of the buffer containing all elements
	 * \param attributes attributes of the tuple datatype
	 */
	Tuple_datatype(std::vector<Element> elements, size_t buffersize, const Attributes_map& attributes = {});

public:
	/** Constructs a new Tuple_datatype
	 *
	 * \param elements the elements for the newly created Tuple_datatype in
	 *        increasing offset order
	 * \param buffersize the total size of the buffer containing all elements
	 * \param attributes attributes of the tuple datatype
	 */
	static std::shared_ptr<Tuple_datatype> make(std::vector<Element> elements, size_t buffersize, const Attributes_map& attributes = {});
};

} // namespace PDI

#endif // PDI_TUPLE_DATATYPE_H_
