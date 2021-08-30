/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <memory>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "int_literal.h"


namespace PDI {

using std::unique_ptr;

Expression::Impl::Int_literal::Int_literal(long value) : m_value(value) {}

long Expression::Impl::Int_literal::to_long(Context&) const
{
	return m_value;
}

double Expression::Impl::Int_literal::to_double(Context&) const
{
	return static_cast<double>(m_value);
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::clone() const
{
	return unique_ptr<Int_literal> {new Int_literal{*this}};
}

Ref Expression::Impl::Int_literal::to_ref(Context& ctx) const
{
	return Impl::to_ref(ctx, Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)});
}

template<class T>
size_t from_long_cpy(void* buffer, long value_long)
{
	T value = static_cast<T>(value_long);
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Int_literal::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
		if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED) {
			switch (scalar_type->buffersize()) {
			case 1L:
				return from_long_cpy<uint8_t>(buffer, m_value);
			case 2L:
				return from_long_cpy<uint16_t>(buffer, m_value);
			case 4L:
				return from_long_cpy<uint32_t>(buffer, m_value);
			case 8L:
				return from_long_cpy<uint64_t>(buffer, m_value);
			default:
				throw Type_error{"Unknown size of integer datatype"};
			}
		} else if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
			switch (type.buffersize()) {
			case 1L:
				return from_long_cpy<int8_t>(buffer, m_value);
			case 2L:
				return from_long_cpy<int16_t>(buffer, m_value);
			case 4L:
				return from_long_cpy<int32_t>(buffer, m_value);
			case 8L:
				return from_long_cpy<int64_t>(buffer, m_value);
			default:
				break;
			}
		}
	}
	throw Value_error{"Cannot copy Int_literal as a non integer datatype."};
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::parse(char const** val_str)
{
	const char* constval = *val_str;
	
	unique_ptr<Int_literal> result {new Int_literal{strtol(constval, const_cast<char**>(&constval), 0)}};
	if (*val_str == constval) {
		throw Value_error{"Expected integer, found `{}'", constval};
	}
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
	return result;
}

} // namespace PDI
