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

#include "config.h"

#include <algorithm>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/ref_any.h"

#include "pdi/tuple_datatype.h"

namespace PDI {

using std::align;
using std::endl;
using std::make_shared;
using std::max;
using std::move;
using std::pair;
using std::regex;
using std::regex_replace;
using std::shared_ptr;
using std::static_pointer_cast;
using std::string;
using std::stringstream;
using std::to_string;
using std::unique_ptr;
using std::vector;

Tuple_datatype::Element::Element(size_t displacement, Datatype_sptr type)
	: m_offset{displacement}
	, m_type{std::move(type)}
{}

Tuple_datatype::Element::Element(const Element& o)
	: m_offset{o.m_offset}
	, m_type{o.m_type}
{}

size_t Tuple_datatype::Element::offset() const
{
	return m_offset;
}

Datatype_sptr Tuple_datatype::Element::type() const
{
	return m_type;
}

bool Tuple_datatype::Element::operator== (const Element& rhs) const
{
	return m_offset == rhs.m_offset && *m_type == *rhs.m_type;
}

bool Tuple_datatype::Element::operator!= (const Element& rhs) const
{
	return !(*this == rhs);
}

Tuple_datatype::Tuple_datatype(vector<Element> elements, size_t buffersize, std::unordered_map<std::string, Ref> attributes)
	: m_elements{std::move(elements)}
	, m_buffersize{buffersize}
{}

const vector<Tuple_datatype::Element>& Tuple_datatype::elements() const
{
	return m_elements;
}

size_t Tuple_datatype::size() const
{
	return m_elements.size();
}

Datatype_sptr Tuple_datatype::densify() const
{
	size_t displacement = 0;
	vector<Tuple_datatype::Element> densified_elements;
	for (auto&& element: m_elements) {
		Datatype_sptr densified_type = element.type()->densify();
		size_t alignment = densified_type->alignment();
		// align the next element as requested
		displacement += (alignment - (displacement % alignment)) % alignment;
		densified_elements.emplace_back(displacement, std::move(densified_type));
		displacement += densified_elements.back().type()->buffersize();
	}
	//add padding at the end of tuple
	size_t tuple_alignment = alignment();
	displacement += (tuple_alignment - (displacement % tuple_alignment)) % tuple_alignment;

	// ensure the tuple size is at least 1 to have a unique address
	displacement = max<size_t>(1, displacement);
	return unique_ptr<Tuple_datatype>{new Tuple_datatype{std::move(densified_elements), displacement}};
}

bool Tuple_datatype::dense() const
{
	size_t displacement = 0;
	for (auto&& element: m_elements) {
		if (!element.type()->dense()) return false;
		size_t alignment = element.type()->alignment();
		// add space for alignment
		displacement += (alignment - (displacement % alignment)) % alignment;
		if (element.offset() > displacement) return false;
		displacement += element.type()->buffersize();
	}
	//add padding at the end of tuple
	size_t tuple_alignment = alignment();
	displacement += (tuple_alignment - (displacement % tuple_alignment)) % tuple_alignment;

	// accept 1 extra byte for unique address
	displacement = max<size_t>(1, displacement);
	if (buffersize() > displacement) return false;
	return true;
}

size_t Tuple_datatype::datasize() const
{
	size_t result = 0;
	for (auto&& element: m_elements) {
		result += element.type()->datasize();
	}
	return result;
}

size_t Tuple_datatype::buffersize() const
{
	return m_buffersize;
}

size_t Tuple_datatype::alignment() const
{
	size_t result = 1;
	for (auto&& element: m_elements) {
		result = max(result, element.type()->alignment());
	}
	return result;
}

bool Tuple_datatype::simple() const
{
	for (auto&& element: m_elements) {
		if (!element.type()->simple()) return false;
	}
	return true;
}

void* Tuple_datatype::data_to_dense_copy(void* to, const void* from) const
{
	if (simple() && dense()) {
		//dense copy
		memcpy(to, from, buffersize());
		to = reinterpret_cast<uint8_t*>(to) + buffersize();
		return to;
	}

	for (auto&& element: elements()) {
		//space_to_align is set to alignment(), because we always find the alignment in the size of alignment
		auto space_to_align = element.type()->alignment();
		//size = 0, because we know that to points to allocated memory
		to = align(element.type()->alignment(), 0, to, space_to_align);
		const uint8_t* element_from = reinterpret_cast<const uint8_t*>(from) + element.offset();
		to = element.type()->data_to_dense_copy(to, element_from);
	}
	return to;
}

void* Tuple_datatype::data_from_dense_copy(void* to, const void* from) const
{
	uint8_t* original_to = reinterpret_cast<uint8_t*>(to);

	if (simple() && dense()) {
		//dense copy
		memcpy(to, from, buffersize());
		to = original_to + buffersize();
		return to;
	}

	for (auto&& element: elements()) {
		auto element_align = element.type()->alignment();
		int padding = (element_align - (reinterpret_cast<const uintptr_t>(from) % element_align)) % element_align;
		from = reinterpret_cast<const uint8_t*>(from) + padding;
		to = original_to + element.offset();

		element.type()->data_from_dense_copy(to, from);
		from = reinterpret_cast<const uint8_t*>(from) + element.type()->datasize();
	}
	to = original_to + buffersize();
	return to;
}

Datatype_sptr Tuple_datatype::index(size_t index) const
{
	if (index < size()) {
		return elements()[index].type();
	} else {
		throw Value_error{"Subaccess tuple index out of range: {} >= {}", index, size()};
	}
}

std::pair<void*, Datatype_sptr> Tuple_datatype::index(size_t index, void* data) const
{
	if (index < size()) {
		data = reinterpret_cast<uint8_t*>(data) + elements()[index].offset();
		return pair<void*, Datatype_sptr>{data, elements()[index].type()};
	} else {
		throw Value_error{"Subaccess tuple index out of range: {} >= {}", index, size()};
	}
}

Datatype_sptr Tuple_datatype::slice(size_t start_index, size_t end_index) const
{
	if (end_index <= size()) {
		vector<Tuple_datatype::Element> new_elements;
		for (size_t i = start_index; i < end_index; i++) {
			new_elements.emplace_back(elements()[i].offset() - elements()[start_index].offset(), elements()[i].type());
		}
		size_t new_buffersize = 0UL;
		if (end_index == size()) {
			// end_index is the last element
			new_buffersize = buffersize() - elements()[start_index].offset();
		} else {
			// end_index is not the last element
			new_buffersize = elements()[end_index + 1].offset() - elements()[start_index].offset();
		}

		auto&& new_tuple = Tuple_datatype::make(std::move(new_elements), new_buffersize);
		return std::move(new_tuple);
	} else {
		throw Value_error{"Subaccess tuple slice out of range: [{}:{}] > {}", start_index, end_index, size()};
	}
}

std::pair<void*, Datatype_sptr> Tuple_datatype::slice(size_t start_index, size_t end_index, void* data) const
{
	return {reinterpret_cast<uint8_t*>(data) + elements()[start_index].offset(), slice(start_index, end_index)};
}

void Tuple_datatype::destroy_data(void* ptr) const
{
	if (simple()) return;
	for (auto&& element: elements()) {
		element.type()->destroy_data(reinterpret_cast<uint8_t*>(ptr) + element.offset());
	}
}

string Tuple_datatype::debug_string() const
{
	stringstream ss;
	ss << "type: tuple" << endl
	   << "dense: " << (dense() ? "true" : "false") << endl
	   << "buffersize: " << buffersize() << endl
	   << "datasize: " << datasize() << endl
	   << "alignment: " << alignment() << endl
	   << "elements: ";
	for (auto&& element: elements()) {
		auto type_str = element.type()->debug_string();
		type_str = regex_replace(type_str, regex("\n"), "\n\t\t");
		type_str.insert(0, "\t\t");
		ss << endl << "\t   displacement: " << element.offset() << endl << "\t   type: " << endl << type_str;
	}
	if (!attributes().empty()) {
		ss << endl << "attributes: " << endl;
		auto it = attributes().begin();
		for (; next(it) != attributes().end(); it++) {
			ss << "\t" << it->first << ", ";
		}
		ss << "\t" << it->first;
	}
	return ss.str();
}

bool Tuple_datatype::operator== (const Datatype& other) const
{
	const Tuple_datatype* rhs = dynamic_cast<const Tuple_datatype*>(&other);
	return rhs && m_buffersize == rhs->m_buffersize && m_elements == rhs->m_elements;
}

struct Tuple_datatype::Shared_enabler: public Tuple_datatype {
	template <class... Args>
	Shared_enabler(Args... args)
		: Tuple_datatype(std::forward<Args>(args)...)
	{}
};

shared_ptr<Tuple_datatype> Tuple_datatype::make(vector<Element> elements, size_t buffersize, std::unordered_map<std::string, Ref> attributes)
{
	return make_shared<Shared_enabler>(std::move(elements), buffersize, std::move(attributes));
}

} // namespace PDI
