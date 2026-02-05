// SPDX-FileCopyrightText: 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <memory>
#include <string>
#include <unordered_set>

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

using std::dynamic_pointer_cast;
using std::make_shared;
using std::string;
using std::unique_ptr;
using std::unordered_set;

unique_ptr<Expression::Impl> Expression::Impl::String_literal::clone() const
{
	return unique_ptr<String_literal>{new String_literal(*this)};
}

string Expression::Impl::String_literal::to_string(Context& ctx) const
{
	string result = m_start;
	for (auto&& subval: m_values) {
		result += subval.first.to_string(ctx);
		result += subval.second;
	}
	return result;
}

long Expression::Impl::String_literal::to_long(Context& ctx) const
{
	static const unordered_set<string> true_values{"y", "Y", "yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"};
	static const unordered_set<string> false_values{"n", "N", "no", "No", "NO", "false", "False", "FALSE", "Off", "Off", "OFF"};
	string src_string = to_string(ctx);
	if (true_values.find(src_string) != true_values.end()) {
		return 1L;
	}
	if (false_values.find(src_string) != false_values.end()) {
		return 0L;
	}
	throw Value_error{"Can not interpret `{}' as an integer value", src_string};
}

double Expression::Impl::String_literal::to_double(Context& ctx) const
{
	throw Value_error{"Can not interpret `{}' as an double value", to_string(ctx)};
}

Ref Expression::Impl::String_literal::to_ref(Context& ctx) const
{
	string value = to_string(ctx);
	return Impl::to_ref(ctx, Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), value.length() + 1));
}

size_t Expression::Impl::String_literal::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	if (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(type)) {
		if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(array_type->subtype())) {
			if (scalar_type->buffersize() == sizeof(char)) {
				string value = to_string(ctx);
				memcpy(buffer, value.c_str(), value.size() + 1);
				return type->buffersize();
			}
		}
	}
	throw Value_error{"Cannot copy String_literal as a non chars array datatype."};
}

unique_ptr<Expression::Impl> Expression::Impl::String_literal::parse(char const ** val_str)
{
	const char* str = *val_str;

	unique_ptr<String_literal> result{new String_literal};

	string* curstr = &result->m_start;
	while (*str) {
		size_t sz = 0;
		while (str[sz] != '\\' && str[sz] != '$' && str[sz]) {
			++sz;
		}
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
		case 0: {
		} break;
		default: {
			throw Impl_error{"Unexpected error!!!"};
		}
		}
	}

	*val_str = str;
	return result;
}

} // namespace PDI
