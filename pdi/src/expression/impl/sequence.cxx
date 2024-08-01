/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"
#include "pdi/tuple_datatype.h"

#include "sequence.h"

namespace PDI {

using std::dynamic_pointer_cast;
using std::max;
using std::move;
using std::string;
using std::unique_ptr;
using std::vector;

Expression::Impl::Sequence::Sequence(PC_tree_t value)
{
	size_t size = PDI::len(value);
	m_value.reserve(size);
	for (int i = 0; i < size; i++) {
		m_value.emplace_back(Expression{parse(PC_get(value, "[%d]", i))});
	}
}

Expression::Impl::Sequence::Sequence(const vector<Expression>& value)
	: m_value(value)
{}

unique_ptr<Expression::Impl> Expression::Impl::Sequence::clone() const
{
	return unique_ptr<Impl> {new Expression::Impl::Sequence(m_value)};
}

long Expression::Impl::Sequence::to_long(Context& ctx) const
{
	throw Value_error{"Cannot interpret Array_expression as a long value"};
}

double Expression::Impl::Sequence::to_double(Context& ctx) const
{
	throw Value_error{"Cannot interpret Array_expression as a double value"};
}

string Expression::Impl::Sequence::to_string(Context& ctx) const
{
	string result;
	for (const auto& element: m_value) {
		result += element.to_string(ctx);
	}
	return result;
}

Ref Expression::Impl::Sequence::to_ref(Context& ctx) const
{
	if (m_value.empty()) {
		Ref_rw result{nullptr, [](void*) {}, Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNKNOWN, 0), 0), true, true};
		return result;
	}
	
	// get subtypes and alignment
	std::vector<Datatype_sptr> subtypes;
	size_t result_alignment = 0;
	for (auto&& element: m_value) {
		subtypes.emplace_back(element.to_ref(ctx).type());
		result_alignment = max<size_t>(result_alignment, subtypes.back()->alignment());
	}
	
	// check if all elements are the same, if true then it is an array
	bool array_datatype = true;
	Datatype_sptr result_type;
	for (int i = 1; i < subtypes.size(); i++) {
		if (*subtypes[0] != *subtypes[i]) {
			array_datatype = false;
			break;
		}
	}
	
	// create the datatype
	if (array_datatype) {
		result_type = Array_datatype::make(std::move(subtypes[0]), m_value.size());
	} else {
		//tuple datatype
		size_t displacement = 0;
		vector<Tuple_datatype::Element> tuple_elements;
		for (auto&& element_type: subtypes) {
			size_t alignment = element_type->alignment();
			// align the next element as requested
			displacement += (alignment - (displacement % alignment)) % alignment;
			tuple_elements.emplace_back(displacement, element_type);
			displacement += tuple_elements.back().type()->buffersize();
		}
		//add padding at the end of tuple
		displacement += (result_alignment - (displacement % result_alignment)) % result_alignment;
		
		// ensure the tuple size is at least 1 to have a unique address
		displacement = max<size_t>(1, displacement);
		result_type = Tuple_datatype::make(move(tuple_elements), displacement);
	}
	
	return Impl::to_ref(ctx, result_type);
}

size_t Expression::Impl::Sequence::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	if (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(type)) {
		size_t offset = 0;
		for (int i = 0; i < m_value.size(); i++) {
			void* to = static_cast<uint8_t*>(buffer) + offset;
			offset += m_value[i].m_impl->copy_value(ctx, to, array_type->subtype());
		}
		if (offset != array_type->buffersize()) {
			throw Value_error{"Array literal copy incomplete: copied {} B of {} B", offset, array_type->buffersize()};
		}
		return offset;
	} else if (auto&& tuple_type = dynamic_pointer_cast<const Tuple_datatype>(type)) {
		size_t bytes_copied = 0;
		for (int i = 0; i < m_value.size(); i++) {
			bytes_copied += m_value[i].m_impl->copy_value(
			    ctx,
			    static_cast<uint8_t*>(buffer) + tuple_type->elements()[i].offset(),
			    tuple_type->elements()[i].type()
			    );
		}
		return tuple_type->buffersize();
	} else {
		throw Value_error{"Sequence literal cannot copy a value whose type is neither array nor tuple"};
	}
}

} // namespace PDI
