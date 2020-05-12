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

#include "sequence.h"

namespace PDI {

Expression::Impl::Sequence::Sequence(PC_tree_t value)
{
	size_t size = PDI::len(value);
	m_value.reserve(size);
	for (int i = 0; i < size; i++) {
		m_value.emplace_back(new Expression(PC_get(value, "[%d]", i)));
	}
}

Expression::Impl::Sequence::Sequence(const std::vector<std::unique_ptr<Expression>>& value)
{
	m_value.reserve(value.size());
	for (int i = 0; i < value.size(); i++) {
		m_value.emplace_back(new Expression{*value[i]});
	}
}

std::unique_ptr<Expression::Impl> Expression::Impl::Sequence::clone() const
{
	return std::unique_ptr<Expression::Impl> {new Expression::Impl::Sequence(m_value)};
}

long Expression::Impl::Sequence::to_long(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Cannot interpret Array_expression as a long value"};
}

double Expression::Impl::Sequence::to_double(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Cannot interpret Array_expression as a double value"};
}

std::string Expression::Impl::Sequence::to_string(Context& ctx) const
{
	std::string result;
	for (const auto& element : m_value) {
		result += element->to_string(ctx);
	}
	return result;
}

Ref Expression::Impl::Sequence::to_ref(Context& ctx) const
{
	if (m_value.empty()) {
		Ref_rw result {
			nullptr,
			[](void* v){},
			Datatype_uptr(new Array_datatype{Datatype_uptr{new Scalar_datatype{Scalar_kind::UNKNOWN, 0}}, 0}),
			true,
			true
		};
		return result;
	}
	Datatype_uptr subtype = m_value[0]->to_ref(ctx).type().clone_type();
	Ref_rw result {
		aligned_alloc(subtype->alignment(), subtype->buffersize() * m_value.size()),
		[](void* v){free(v);},
		Datatype_uptr(new Array_datatype{std::move(subtype), m_value.size()}),
		true,
		true
	};
	
	copy_value(ctx, result.get(), result.type());
	return result;
}

Ref Expression::Impl::Sequence::to_ref(Context& ctx, const Datatype& type) const
{
	Ref_rw result {
		aligned_alloc(type.alignment(), type.buffersize()),
		[](void* v){free(v);},
		type.clone_type(),
		true,
		true
	};
	copy_value(ctx, result.get(), result.type());
	return result;
}

size_t Expression::Impl::Sequence::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Array_datatype* array_type = dynamic_cast<const Array_datatype*>(&type)) {
		size_t offset = 0;
		for (int i = 0; i < m_value.size(); i++) {
			void* to = static_cast<uint8_t*>(buffer) + offset;
			offset += m_value[i]->m_impl->copy_value(ctx, to, array_type->subtype());
		}
		if (offset != array_type->buffersize()) {
			throw Error {PDI_ERR_VALUE, "Array literal copy incomplete: copied {} B of {} B", offset, array_type->buffersize()};
		}
		return offset;
	} else {
		throw Error {PDI_ERR_VALUE, "Array literal cannot copy value of not array datatype"};
	}
}

} // namespace PDI
