/*******************************************************************************
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

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "mapping.h"

namespace PDI {

using std::max;
using std::move;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::vector;

Expression::Impl::Mapping::Mapping(PC_tree_t value)
{
	size_t size = len(value);
	for (int i = 0; i < size; i++) {
		m_value.emplace(PDI::to_string(PC_get(value, "{%d}", i)), Expression{parse(PC_get(value, "<%d>", i))});
	}
}

Expression::Impl::Mapping::Mapping(const unordered_map< string, Expression >& value)
{
	for (const auto& element : value) {
		m_value.emplace(element.first, element.second);
	}
}

unique_ptr<Expression::Impl> Expression::Impl::Mapping::clone() const
{
	return unique_ptr<Impl> {new Expression::Impl::Mapping(m_value)};
}

long Expression::Impl::Mapping::to_long(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Cannot interpret Map_expression as a long value"};
}

double Expression::Impl::Mapping::to_double(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Cannot interpret Map_expression as a double value"};
}

string Expression::Impl::Mapping::to_string(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Cannot interpret Map_expression as a string value"};
}

Ref Expression::Impl::Mapping::to_ref(Context& ctx) const
{
	vector<Record_datatype::Member> members;
	size_t displacement = 0;
	size_t record_alignment = 1;
	for (const auto& element: m_value) {
		Ref_rw element_ref {element.second.to_ref(ctx)};
		
		size_t alignment = element_ref.type().alignment();
		record_alignment = max(record_alignment, alignment);
		
		// align the next member
		displacement += (alignment - (displacement % alignment)) % alignment;
		members.emplace_back(displacement, element_ref.type().clone_type(), element.first);
		displacement += element_ref.type().buffersize();
	}
	//add padding at the end of record
	displacement += (record_alignment - (displacement % record_alignment)) % record_alignment;
	
	Ref_rw result {
		aligned_alloc(record_alignment, displacement),
		[](void* v){free(v);},
		Datatype_uptr(new Record_datatype{move(members), displacement}),
		true,
		true
	};
	copy_value(ctx, result.get(), result.type());
	return result;
}

Ref Expression::Impl::Mapping::to_ref(Context& ctx, const Datatype& type) const
{
	Ref_rw result {
		aligned_alloc(type.alignment(), type.buffersize()),
		[](void* v){free(v);},
		type.clone_type(),
		true,
		true
	};
	copy_value(ctx, result, result.type());
	return result;
}

size_t Expression::Impl::Mapping::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Record_datatype* record_type = dynamic_cast<const Record_datatype*>(&type)) {
		for (const auto& element : m_value) {
			auto member_it = find_if(record_type->members().begin(), record_type->members().end(), [&element](const Record_datatype::Member m) {
				return m.name() == element.first;
			});
			if (member_it != record_type->members().end()) {
				void* to = static_cast<uint8_t*>(buffer) + member_it->displacement();
				element.second.m_impl->copy_value(ctx, to, member_it->type());
			} else {
				throw Error {PDI_ERR_VALUE, "Trying to reference non-existing member: {}", element.first};
			}
		}
		return type.buffersize();
	} else {
		throw Error {PDI_ERR_VALUE, "Map literal cannot copy value of not record datatype"};
	}
}

} // namespace PDI
