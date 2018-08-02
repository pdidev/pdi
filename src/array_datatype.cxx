/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/array_datatype.h"


namespace PDI {

using std::max;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

Array_datatype::Array_datatype(Datatype_uptr subtype, size_t size, size_t start, size_t subsize):
	m_subtype {std::move(subtype)},
	m_size{std::move(size)},
	m_start{std::move(start)},
	m_subsize{std::move(subsize)}
{}

Array_datatype::Array_datatype(Datatype_uptr subtype, size_t size):
	Array_datatype{std::move(subtype), size, 0, std::move(size)}
{}

const Datatype& Array_datatype::subtype() const
{
	return *m_subtype;
}

size_t Array_datatype::size() const
{
	return m_size;
}

size_t Array_datatype::start() const
{
	return m_start;
}

size_t Array_datatype::subsize() const
{
	return m_subsize;
}

Datatype_template_uptr Array_datatype::clone() const
{
	return clone_type();
}

Datatype_uptr Array_datatype::clone_type() const
{
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->clone_type(), m_size, m_start, m_subsize}};
}

Datatype_uptr Array_datatype::densify() const
{
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->densify(), m_subsize}};
}

Datatype_uptr Array_datatype::evaluate(Context&) const
{
	return Array_datatype::clone_type();
}

bool Array_datatype::dense() const
{
	if (m_size != m_subsize) return false;
	return m_subtype->dense();
}

size_t Array_datatype::datasize() const
{
	return m_subsize * m_subtype->datasize();
}

size_t Array_datatype::buffersize() const
{
	return m_size * m_subtype->buffersize();
}

size_t Array_datatype::alignment() const
{
	return m_subtype->alignment();
}

bool Array_datatype::is_POD() const
{
	return dense() && m_subtype->is_POD();
}

void Array_datatype::copy_data(void*& to, const void* from) const
{
	//need to align to the aligment of subtype (at some point it will be scalar or record)
	auto space_to_align = alignment();
	//size = 0, because we know that 'to' points to allocated memory
	to = std::align(alignment(), 0, to, space_to_align);
	if (to == nullptr) {
		throw Error{PDI_ERR_IMPL, "Could not align the array datatype"};
	}
	
	if (is_POD()) {
		//dense copy
		memcpy(to, from, buffersize());
		to = reinterpret_cast<uint8_t*>(to) + buffersize();
		return;
	}
	
	auto subtype_buffersize = subtype().buffersize();
	const uint8_t* updated_from_ptr = (start() * subtype_buffersize) + reinterpret_cast<const uint8_t*>(from);
	
	if (subtype().is_POD()) {
		//make a dense copy of subarray
		memcpy(to, updated_from_ptr, datasize());
		to = reinterpret_cast<uint8_t*>(to) + datasize();
	} else {
		//not POD copy
		for (int subtype_no = 0; subtype_no < subsize(); subtype_no++) {
			subtype().copy_data(to, updated_from_ptr);
			updated_from_ptr += subtype_buffersize;
		}
	}
}

void Array_datatype::delete_data(void* ptr) const
{
	if (!subtype().is_POD()) {
		auto subtype_buffersize = subtype().buffersize();
		for (int subtype_no = 0; subtype_no < subsize(); subtype_no++) {
			subtype().delete_data(reinterpret_cast<uint8_t*>(ptr) + subtype_no*subtype_buffersize);
		}
	}
}

} // namespace PDI
