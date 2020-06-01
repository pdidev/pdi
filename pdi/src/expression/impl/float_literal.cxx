/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "float_literal.h"

namespace PDI {

using std::unique_ptr;

Expression::Impl::Float_literal::Float_literal(double value) : m_value(value) {}

unique_ptr<Expression::Impl> Expression::Impl::Float_literal::clone() const
{
	return unique_ptr<Float_literal> {new Float_literal{*this}};
}

long Expression::Impl::Float_literal::to_long(Context&) const
{
	return static_cast<long>(m_value);
}

double Expression::Impl::Float_literal::to_double(Context& ctx) const
{
	return m_value;
}

Ref Expression::Impl::Float_literal::to_ref(Context& ctx) const
{
	// create double datatype as default
	Ref_rw result {
		aligned_alloc(alignof(double), sizeof(double)),
		[](void* v){free(v);},
		unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}},
		true,
		true
	};
	*static_cast<double*>(result.get()) = m_value;
	return result;
}

Ref Expression::Impl::Float_literal::to_ref(Context& ctx, const Datatype& type) const
{
	if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
		Ref_rw result {
			aligned_alloc(scalar_type->alignment(), scalar_type->buffersize()),
			[](void* v){free(v);},
			scalar_type->clone_type(),
			true,
			true
		};
		copy_value(ctx, result.get(), result.type());
		return result;
	} else if (const Array_datatype* array_type = dynamic_cast<const Array_datatype*>(&type)) {
		throw Error{PDI_ERR_VALUE, "Cannot interpret Float_literal as an array datatype."};
	} else if (const Record_datatype* record_type = dynamic_cast<const Record_datatype*>(&type)) {
		throw Error{PDI_ERR_VALUE, "Cannot interpret Float_literal as a record datatype."};
	} else {
		throw Error{PDI_ERR_VALUE, "Cannot interpret Float_literal as given datatype."};
	}
}

size_t Expression::Impl::Float_literal::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
		if (scalar_type->kind() == PDI::Scalar_kind::FLOAT) {
			switch (type.buffersize()) {
			case 4L: {
				float value = static_cast<float>(m_value);
				memcpy(buffer, &value, sizeof(float));
				return sizeof(float);
			}
			case 8L: {
				memcpy(buffer, &m_value, sizeof(double));
				return sizeof(double);
			}
			default:
				break;
			}
		}
	}
	throw Error{PDI_ERR_VALUE, "Cannot copy Float_literal as a non float datatype."};
}

unique_ptr<Expression::Impl> Expression::Impl::Float_literal::parse(char const** val_str)
{
	const char* constval = *val_str;
	
	unique_ptr<Float_literal> result {new Float_literal{strtod(constval, const_cast<char**>(&constval))}};
	if (*val_str == constval) {
		throw Error {PDI_ERR_VALUE, "Expected double, found `{}'", constval};
	}
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
	return result;
}

} //namespace PDI
