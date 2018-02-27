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

#include "config.h"

#include <cassert>
#include <cstring>
#include <cstdint>
#include <map>
#include <memory>
#include <vector>

#include "pdi/data_type.h"

#include "pdi/data_reference.h"


namespace PDI
{

using std::map;
using std::make_pair;
using std::memcpy;
using std::move;
using std::unique_ptr;
using std::vector;


namespace
{

struct Data_layout {
	/// number of time to repeat the content
	size_t m_repeat;
	
	/// offset=>subcontent or 1 byte if empty
	map<size_t, Data_layout> m_subcontent;
	
	/// Size of the repeated content including empty space
	size_t m_size;
	
	/// the bits that should be null in an address for alignment
	uintptr_t m_align_mask;
	
	/** Copy a buffer whose layout is described by this into a sparse equivalent
	 * \param to the adress at which to write the dense copy
	 * \param from the adress from which to copy
	 * \return the address following the just copied data
	 */
	uint8_t *copy(uint8_t *to, const uint8_t *from)
	{
		to = reinterpret_cast<uint8_t *>(reinterpret_cast<uintptr_t>(to + m_align_mask) & ~m_align_mask);
		if (m_subcontent.empty() && 1 == m_size) { // optimize for dense blocks
			memcpy(to, from, m_repeat);
			return static_cast<uint8_t *>(to) + m_repeat;
		}
		for (size_t ii = 0; ii < m_repeat; ++ii) {
			if (m_subcontent.empty()) {   // copy one byte
				*to++ = *from;
			}
			for (auto &&mem : m_subcontent) {
				to = mem.second.copy(to, from + mem.first);
			}
			from += m_size;
		}
		return to;
	}
};

Data_layout desc(const Data_type &type)
{
	if (auto &&scalar_type = dynamic_cast<const Scalar_datatype *>(&type)) {
		return {scalar_type->datasize(), {}, 1, static_cast<uintptr_t>((1 << scalar_type->alignment()) - 1)};
	}
	if (auto &&array_type = dynamic_cast<const Array_datatype *>(&type)) {
		Data_layout inner = desc(array_type->subtype());
		long subsize = array_type->subsize();
		long size = array_type->size();
		if (subsize == size) {   // dense array
			inner.m_repeat *= subsize;
			return inner;
		} else { // sparse array
			size_t buffer_size = inner.m_size * inner.m_repeat * size; // the contained buffer size is one element times the buffer size
			inner.m_repeat *= subsize; // repeat the content as required
			return {1, {make_pair(inner.m_size *inner.m_repeat * array_type->start(), move(inner))}, buffer_size, 0};
		}
	}
	if (auto &&record_type = dynamic_cast<const Record_datatype *>(&type)) {
		if (record_type->members().size() == 1) {   // spurious optim when we have just a potentially sparsified version of the content
			Data_layout inner = desc(record_type->members().front().type());
			size_t offset = record_type->members().front().displacement();
			size_t buffersize = record_type->buffersize();
			if (inner.m_repeat == 1) {   // we can sparsify by adding the offset to all subcontent
				Data_layout result = {1, {}, record_type->buffersize(), 0};
				for (auto &&sub : inner.m_subcontent) {
					result.m_subcontent.emplace(sub.first + offset, move(sub.second)); // add the offset to all members
				}
				return result;
			}
			return {1, {make_pair(offset, move(inner))}, buffersize, 0};
		}
		Data_layout result = {1, {}, record_type->buffersize(), 0 };
		for (auto &&mem : record_type->members()) {
			Data_layout mem_layout = desc(mem.type());
			if (mem_layout.m_repeat > 1) {
				result.m_subcontent.emplace(mem.displacement(), move(mem_layout));
			} else {
				for (auto &&mem_subctnt : mem_layout.m_subcontent) {
					mem_layout.m_subcontent.emplace(make_pair(mem.displacement() + mem_subctnt.first, move(mem_subctnt.second)));
				}
			}
		}
		return result;
	}
	assert(false && "Unsupported data type");
}

} // namespace <anonymous>

Data_ref Data_ref_base::do_copy(Data_r_ref ref)
{
	Data_layout ref_desc = desc(ref.type());
	void *newbuffer{operator new (ref_desc.m_size * ref_desc.m_repeat)};
	try {
		ref_desc.copy(static_cast<uint8_t *>(newbuffer), static_cast<const uint8_t *>(ref.get()));
		return Data_ref{newbuffer,  [](void *d){::operator delete (d);}, ref.type().densify(), true, true};
	} catch (...) {
		::operator delete (newbuffer);
		throw;
	}
}

const Data_type &Data_ref_base::type() const
{
	if (!m_content || !m_content->m_buffer) return UNDEF_TYPE;
	return *m_content->m_type;
}

} // namespace PDI
