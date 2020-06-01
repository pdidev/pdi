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
#include <string>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "reference_expression.h"

#include "string_literal.h"

namespace PDI {

using std::string;
using std::unique_ptr;

unique_ptr<Expression::Impl> Expression::Impl::String_literal::clone() const
{
	return unique_ptr<String_literal> {new String_literal(*this)};
}

string Expression::Impl::String_literal::to_string(Context& ctx) const
{
	string result = m_start;
	for (auto&& subval : m_values) {
		result += subval.first.to_string(ctx);
		result += subval.second;
	}
	return result;
}

long Expression::Impl::String_literal::to_long(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Can not interpret `{}' as an integer value", to_string(ctx)};
}

double Expression::Impl::String_literal::to_double(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Can not interpret `{}' as an double value", to_string(ctx)};
}

Ref Expression::Impl::String_literal::to_ref(Context& ctx) const
{
	string value = to_string(ctx);
	// copy because string does not provide a release call
	void* str = malloc(sizeof(char) * (value.length() + 1));
	memcpy(str, value.c_str(), value.length() + 1);
	
	return Ref {
		str,
		[](void* v){free(v);},
		unique_ptr<Array_datatype>{
			new Array_datatype{
				unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}},
				value.length() + 1
			}
		},
		true,
		true
	};
}

Ref Expression::Impl::String_literal::to_ref(Context& ctx, const Datatype& type) const
{
	if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
		throw Error{PDI_ERR_VALUE, "Cannot interpret String_literal as a scalar datatype."};
	} else if (const Array_datatype* array_type = dynamic_cast<const Array_datatype*>(&type)) {
		Ref_rw result {
			aligned_alloc(array_type->alignment(), array_type->buffersize()),
			[](void* v){free(v);},
			array_type->clone_type(),
			true,
			true
		};
		copy_value(ctx, result.get(), result.type());
		return result;
	} else if (const Record_datatype* record_type = dynamic_cast<const Record_datatype*>(&type)) {
		throw Error{PDI_ERR_VALUE, "Cannot interpret String_literal as a record datatype."};
	} else {
		throw Error{PDI_ERR_VALUE, "Cannot interpret String_literal as given datatype."};
	}
}

size_t Expression::Impl::String_literal::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Array_datatype* array_type = dynamic_cast<const Array_datatype*>(&type)) {
		if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&array_type->subtype())) {
			if (scalar_type->buffersize() == sizeof(char)) {
				string value = to_string(ctx);
				memcpy(buffer, value.c_str(), value.size()+1);
				return type.buffersize();
			}
		}
	}
	throw Error{PDI_ERR_VALUE, "Cannot copy String_literal as a non chars array datatype."};
}

unique_ptr<Expression::Impl> Expression::Impl::String_literal::parse(char const** val_str)
{
	const char* str = *val_str;
	
	unique_ptr<String_literal> result{new String_literal};
	
	string* curstr = &result->m_start;
	while (*str) {
		size_t sz = 0;
		while (str[sz] != '\\' && str[sz] != '$' && str[sz]) ++sz;
		curstr->append(str, sz);
		str += sz;
		switch (*str) {
		case '\\': {
			str += 2;
			curstr->push_back('\\');
		} break;
		case '$': {
			switch (str[1]) {
			case '(': { // remove the dollar, parse the term starting with the parenthesis (the operation)
				++str;
				result->m_values.emplace_back(Expression{parse_term(&str)}, "");
				curstr = &result->m_values.back().second;
			} break;
			default: { // parse the term starting with the dollar (the ref)
				result->m_values.emplace_back(Expression{Reference_expression::parse(&str)}, "");
				curstr = &result->m_values.back().second;
			} break;
			}
		} break;
		case 0: {} break;
		default: {
			throw Error {PDI_ERR_IMPL, "Unexpected error!!!"};
		}
		}
	}
	
	*val_str = str;
	return result;
}

} // namespace PDI
