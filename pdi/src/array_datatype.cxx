/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "config.h"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <regex>
#include <string>
#include <sstream>
#include <vector>

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/array_datatype.h"


namespace PDI {

using std::align;
using std::endl;
using std::max;
using std::move;
using std::pair;
using std::regex;
using std::regex_replace;
using std::string;
using std::stringstream;
using std::to_string;
using std::transform;
using std::unique_ptr;
using std::vector;

Array_datatype::Index_accessor::Index_accessor(size_t index):
	m_index{index}
{}

string Array_datatype::Index_accessor::access_kind() const 
{
	return "array index access [" + to_string(m_index) + "]";
}

unique_ptr<Datatype::Accessor_base> Array_datatype::Index_accessor::clone() const
{
	return unique_ptr<Accessor_base>{new Index_accessor{m_index}};
}

pair<void*, Datatype_uptr> Array_datatype::Index_accessor::access(const Array_datatype& array_type,
										void* from,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (m_index < array_type.subsize()) {
		from = reinterpret_cast<uint8_t*>(from) + array_type.subtype().buffersize() * (array_type.start() + m_index);
		if (remaining_begin == remaining_end) {
			return {from, array_type.subtype().clone_type()};
		} else {
			return array_type.subtype().subaccess_by_iterators(from, remaining_begin, remaining_end);	
		}
	} else {
		throw Value_error {"Subaccess array index out of range: {} >= {}", m_index, array_type.subsize()};
	}
}

pair<void*, Datatype_uptr> Array_datatype::Index_accessor::access(const Tuple_datatype& tuple_type,
										void* from,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (m_index < tuple_type.size()) {
		from = reinterpret_cast<uint8_t*>(from) + tuple_type.elements()[m_index].offset();
		if (remaining_begin == remaining_end) {
			return pair<void*, Datatype_uptr>{from, tuple_type.elements()[m_index].type().clone_type()};
		} else {
			return tuple_type.elements()[m_index].type().subaccess_by_iterators(from, remaining_begin, remaining_end);	
		}
	} else {
		throw Value_error {"Subaccess tuple index out of range: {} >= {}", m_index, tuple_type.size()};
	}
}

Array_datatype::Slice_accessor::Slice_accessor(size_t start, size_t end):
	m_start{start},
	m_end{end}
{}

string Array_datatype::Slice_accessor::access_kind() const 
{
	return "array slice access [" + to_string(m_start) + ":" + to_string(m_end) + "]";
}

pair<void*, Datatype_uptr> Array_datatype::Slice_accessor::access(const Array_datatype& array_type,
										void* from,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (m_end <= array_type.subsize()) {
		Datatype_uptr new_type {new Array_datatype{array_type.subtype().clone_type(), this->size()}};
		from = reinterpret_cast<uint8_t*>(from) + array_type.subtype().buffersize() * (array_type.start() + m_start);
		if (remaining_begin == remaining_end) {
			return {from, move(new_type)};
		} else {
			return new_type->subaccess_by_iterators(from, remaining_begin, remaining_end);
		}
	} else {
		throw Value_error {"Subaccess array slice out of range: [{}:{}] > {}", m_start, 
																				m_end,
																				array_type.subsize()};
	}
}


pair<void*, Datatype_uptr> Array_datatype::Slice_accessor::access(const Tuple_datatype& tuple_type,
										void* from,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
										vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (m_end <= tuple_type.size()) {
		vector<Tuple_datatype::Element> new_elements;
		for (size_t i = m_start; i < m_end; i++) {
			new_elements.emplace_back(tuple_type.elements()[i].offset() - tuple_type.elements()[m_start].offset(),
				tuple_type.elements()[i].type().clone_type());
		}
		size_t new_buffersize = 0UL;
		if (m_end == tuple_type.size()) {
			// m_end is the last element
			new_buffersize = tuple_type.buffersize() - tuple_type.elements()[m_start].offset();
		} else {
			// m_end is not the last element
			new_buffersize = tuple_type.elements()[m_end + 1].offset() - tuple_type.elements()[m_start].offset();
		}

		Datatype_uptr new_tuple {new Tuple_datatype{move(new_elements), new_buffersize}};
		from = reinterpret_cast<uint8_t*>(from) + tuple_type.elements()[m_start].offset();
		if (remaining_begin == remaining_end) {
			return {from, move(new_tuple)};
		} else {
			return new_tuple->subaccess_by_iterators(from, remaining_begin, remaining_end);
		}
	} else {
		throw Value_error {"Subaccess tuple slice out of range: [{}:{}] > {}", m_start, 
																				m_end,
																				tuple_type.size()};
	}
}

unique_ptr<Datatype::Accessor_base> Array_datatype::Slice_accessor::clone() const
{
	return unique_ptr<Accessor_base>{new Slice_accessor{m_start, m_end}};
}

size_t Array_datatype::Slice_accessor::size() const
{
	return m_end - m_start;
}

Array_datatype::Array_datatype(Datatype_uptr subtype, size_t size, size_t start, size_t subsize, const Attributes_map& attributes):
	Datatype(attributes),
	m_subtype {move(subtype)},
	m_size{move(size)},
	m_start{move(start)},
	m_subsize{move(subsize)}
{}

Array_datatype::Array_datatype(Datatype_uptr subtype, size_t size, const Attributes_map& attributes):
	Array_datatype{move(subtype), size, 0, move(size), attributes}
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
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->clone_type(), m_size, m_start, m_subsize, m_attributes}};
}

Datatype_uptr Array_datatype::densify() const
{
	return unique_ptr<Array_datatype> {new Array_datatype{m_subtype->densify(), m_subsize, m_attributes}};
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

bool Array_datatype::simple() const
{
	return m_subtype->simple();
}

void* Array_datatype::data_to_dense_copy(void* to, const void* from) const
{
	size_t subtype_buffersize = subtype().buffersize();
	from = static_cast<const uint8_t*>(from) + (start() * subtype_buffersize);
	
	if (subtype().simple() && subtype().dense()) {
		//dense copy
		memcpy(to, from, datasize());
		return reinterpret_cast<uint8_t*>(to) + datasize();
	}
	
	size_t subtype_alignment = subtype().alignment();
	for (size_t subtype_no = 0; subtype_no < subsize(); subtype_no++) {
		//space_to_align is set to alignment(), because we always find the alignment in the size of alignment
		size_t space_to_align = subtype_alignment;
		//size = 0, because we know that to points to allocated memory
		to = align(subtype_alignment, 0, to, space_to_align);
		
		to = subtype().data_to_dense_copy(to, from);
		from = reinterpret_cast<const uint8_t*>(from) + subtype_buffersize;
	}
	return to;
}

void* Array_datatype::data_from_dense_copy(void* to, const void* from) const
{
	uint8_t* original_to = reinterpret_cast<uint8_t*>(to);
	size_t subtype_buffersize = subtype().buffersize();
	to = static_cast<uint8_t*>(to) + (start() * subtype_buffersize);
	
	if (subtype().simple() && subtype().dense()) {
		//dense copy
		memcpy(to, from, datasize());
		to = original_to + buffersize();
		return to;
	}
	
	size_t subtype_alignment = subtype().alignment();
	for (int subtype_no = 0; subtype_no < subsize(); subtype_no++) {
		//space_to_align is set to alignment(), because we always find the alignment in the size of alignment
		size_t space_to_align = subtype_alignment;
		//size = 0, because we know that to points to allocated memory
		to = align(subtype_alignment, 0, to, space_to_align);
		
		//cannot use std::align, becasue `from' is const
		auto subtype_align = subtype().alignment();
		int padding = (subtype_align - (reinterpret_cast<const uintptr_t>(from) % subtype_align)) % subtype_align;
		from = reinterpret_cast<const uint8_t*>(from) + padding;
		
		to = subtype().data_from_dense_copy(to, from);
		from = reinterpret_cast<const uint8_t*>(from) + subtype().datasize();
	}
	to = original_to + buffersize();
	return to;
}

pair<void*, Datatype_uptr> Array_datatype::subaccess_by_iterators(void* from,
												vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
												vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	return remaining_begin->get()->access(*this, from, ++remaining_begin, remaining_end);
}

void Array_datatype::destroy_data(void* ptr) const
{
	if (!subtype().simple()) {
		size_t subtype_buffersize = subtype().buffersize();
		for (size_t subtype_no = start(); subtype_no < start()+subsize(); ++subtype_no) {
			subtype().destroy_data(reinterpret_cast<uint8_t*>(ptr) + subtype_no*subtype_buffersize);
		}
	}
}


string Array_datatype::debug_string() const
{
	stringstream ss;
	auto subtype_str = subtype().debug_string();
	subtype_str = regex_replace(subtype_str, regex("\n"), "\n\t");
	subtype_str.insert(subtype_str.begin(), '\t');
	ss << "type: array" << endl
	    << "dense: " << (dense() ? "true" : "false") << endl
	    << "buffersize: " << buffersize() << endl
	    << "datasize: " << datasize() << endl
	    << "alignment: " << alignment() << endl
	    << "size: " << size() << endl
	    << "start: " << start() << endl
	    << "subsize: " << subsize() << endl
	    << "subtype: " << endl << subtype_str;
	if (!m_attributes.empty()) {
		ss << endl << "attributes: " << endl;
		auto it = m_attributes.begin();
		for (; next(it) != m_attributes.end(); it++) {
			ss << "\t" << it->first << ", ";
		}
		ss << "\t" << it->first;
	}
	return ss.str();
}

bool Array_datatype::operator==(const Datatype& other) const
{
	const Array_datatype* rhs = dynamic_cast<const Array_datatype*>(&other);
	return rhs
	    && *m_subtype == *rhs->m_subtype
	    && m_size == rhs->m_size
	    && m_start == rhs->m_start
	    && m_subsize == rhs->m_subsize;
}

} // namespace PDI
