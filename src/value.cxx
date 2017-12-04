/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

/**
 * \file value.c
 * \brief Parsing functions
 * \details The data obtained from paraconf consist of arrays of chars.
 * Those string may contain variable name, operations... and are evaluated/parse using the above functions.
 * \author J. Bigot (CEA)
 */

#include <cassert>
#include <cstdint>
#include <sstream>
#include <variant>
#include <vector>

#include <ctype.h>
#include <stdlib.h>

#include "pdi/state.h"
#include "pdi/data_reference.h"

#include "status.h"
#include "utils.h"

#include "pdi/value.h"

namespace PDI {

using std::cout;
using std::endl;
using std::move;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

namespace {

/** The binary operators that can be used in values
 */
enum PDI_exprop_t
{
	PDI_OP_PLUS = '+',
	PDI_OP_MINUS = '-',
	PDI_OP_MULT = '*',
	PDI_OP_DIV = '/',
	PDI_OP_MOD = '%',
	PDI_OP_EQUAL = '=',
	PDI_OP_AND = '&',
	PDI_OP_OR = '|',
	PDI_OP_GT = '>',
	PDI_OP_LT = '<'
};

/** A constant integer value 
 */
struct Constval:
		public Value::Impl
{
	long m_value;
	
	Constval ( long value ) : m_value(value) {}
	
	long to_long() const override { return m_value; }
	
	unique_ptr<Impl> clone() const override
	{
		return unique_ptr<Constval>{new Constval{*this}};
	}
	
};

/** A value in case this is a string (potentially with dollar refs inside)
 */
struct Stringval:
		public Value::Impl
{
	/// a char string containing the constant part of the str_value
	string str;
	
	struct Subvalue {
		Value value;
		/// position in str where to insert the value
		int pos;
	};
	
	/// array of subvalues
	vector<Subvalue> values;
	
	long to_long() const override
	{
		throw Error{PDI_ERR_VALUE, "Can not interpret `%s' as an integer value", to_string().c_str()};
	}
	
	string to_string() const override
	{
		stringstream result;
		
		size_t from_idx = 0;
		for (size_t ii = 0; ii < values.size(); ++ii) {
			size_t blk_sz = values[ii].pos - from_idx;
			result << string{str.c_str() + from_idx, blk_sz};
			from_idx += blk_sz;
			
			result << values[ii].value.to_string();
		}
		result << str.c_str() + from_idx;
		
		return result.str();
	}
	
	unique_ptr<Impl> clone() const override { return unique_ptr<Stringval>{new Stringval{*this}}; }
	
};

/** A value in case this is an expression
 */
struct Exprval:
		public Value::Impl
{
	vector<Value> values;
	
	vector<PDI_exprop_t> ops;
	
	long to_long() const override
	{
		long computed_value = values[0].to_long();
		for (size_t ii = 1; ii < values.size(); ++ii) {
			long operand = values[ii].to_long();
			switch (ops[ii - 1]) {
			case PDI_OP_PLUS: {
				computed_value += operand;
			} break;
			case PDI_OP_MINUS: {
				computed_value -= operand;
			} break;
			case PDI_OP_MULT: {
				computed_value *= operand;
			} break;
			case PDI_OP_DIV: {
				computed_value /= operand;
			} break;
			case PDI_OP_MOD: {
				computed_value %= operand;
			} break;
			case PDI_OP_EQUAL: {
				computed_value = (computed_value == operand);
			} break;
			case PDI_OP_AND: {
				computed_value = computed_value && operand;
			} break;
			case PDI_OP_OR: {
				computed_value = computed_value || operand;
			} break;
			case PDI_OP_GT: {
				computed_value = (computed_value > operand);
			} break;
			case PDI_OP_LT: {
				computed_value = (computed_value < operand);
			} break;
			default: {
				PDI_make_err(PDI_ERR_VALUE, "Unknown operator: `%c'", ops[ii - 1]);
			}
			}
		}
		
		return computed_value;
	}
	
	unique_ptr<Impl> clone() const override { return unique_ptr<Exprval>{new Exprval{*this}}; }
	
};

/** A value in case this is a reference to another value
 */
struct Refval:
		public Value::Impl
{
	/// The referenced data
	Data_descriptor *m_referenced;
	
	/// Indexes in case the referenced data is an array
	vector<Value> m_idx;
	
	Refval(Data_descriptor * desc): m_referenced(desc) {}
	
	long to_long() const override
	{
		const PDI_datatype_t &ref_type = m_referenced->get_type();
		PDI_scalar_type_t type = ref_type.c.scalar;
		Data_ref cref;
		
		if (ref_type.kind == PDI_K_ARRAY) {
			if (m_idx.size() != static_cast<size_t>(ref_type.c.array->ndims)) {
				PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, %d expected", m_idx.size(), ref_type.c.array->ndims);
			}
			if (ref_type.c.array->type.kind != PDI_K_SCALAR) {
				PDI_make_err(PDI_ERR_VALUE, "Invalid type accessed");
			}
			type = ref_type.c.array->type.c.scalar;
		} else if (ref_type.kind == PDI_K_SCALAR) {
			if (!m_idx.empty()) {
				PDI_make_err(PDI_ERR_VALUE, "Invalid number of index: %d, 0 expected", m_idx.size());
			}
		} else {
			PDI_make_err(PDI_ERR_VALUE, "Invalid access to a struct");
		}
		
		long idx = 0;
		long stride = 1;
		for (size_t ii = 0; ii < m_idx.size(); ++ii) {
			long start = ref_type.c.array->starts[ii].to_long();
			long index = m_idx[ii].to_long();
			idx += (start + index) * stride;
			long size = ref_type.c.array->sizes[ii].to_long();
			stride *= size;
		}
		
		if (!m_referenced->value()) {
			PDI_make_err(PDI_ERR_VALUE, "Referenced variable `%s' is not shared", m_referenced->name().c_str());
		}
		
		if (Data_r_ref ref = m_referenced->value()) {
			void *value = ref.get();
			switch (type) {
			case PDI_T_INT8:
				return static_cast<int8_t *>(value)[idx];
			case PDI_T_INT16:
				return static_cast<int16_t *>(value)[idx];
			case PDI_T_INT32:
				return static_cast<int32_t *>(value)[idx];
			case PDI_T_INT64:
				return static_cast<int64_t *>(value)[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Non-integer type accessed");
			}
		}
		throw Error(PDI_ERR_VALUE, "Referenced variable `%s' is not readable", m_referenced->name().c_str());
	}
	
	unique_ptr<Impl> clone() const override { return unique_ptr<Refval>{new Refval{*this}}; }
	
};

Value parse_intval(char const **val_str, int level);

string parse_id(char const **val_str)
{
	const char *id = *val_str;
	
	if (!(
	        (*id >= 'a' && *id <= 'z')
	        || (*id >= 'A' && *id <= 'Z')
	        || (*id == '_')
	    )) {
		throw Error {PDI_ERR_VALUE, "Invalid first ID character: %c", *id};
	}
	++id;
	size_t id_len = 1;
	
	while (
	    (*id >= 'a' && *id <= 'z')
	    || (*id >= 'A' && *id <= 'Z')
	    || (*id >= '0' && *id <= '9')
	    || (*id == '_')
	) {
		++(id_len);
		++id;
	}
	
	string result { *val_str, id_len };
	*val_str = id;
	return result;
}

unique_ptr<Refval> parse_ref(char const **val_str)
{
	const char *ref = *val_str;
	
	if (*ref != '$') throw Error{PDI_ERR_VALUE, "Expected '$', got %c", *ref};
	++ref;
	
	bool has_curly_brace = false;
	if (*ref == '{') {
		++ref;
		has_curly_brace = true;
	}
	
	unique_ptr<Refval> result{new Refval{ &PDI_state.desc(parse_id(&ref)) }};
	
	if (!result->m_referenced->is_metadata()) {
		throw Error{PDI_ERR_VALUE, "Invalid reference to non-metadata `%s'", result->m_referenced->name().c_str()};
	}
	
	assert(!result->m_referenced->name().empty());
	
	while (isspace(*ref)) ++ref;
	
	while (*ref == '[') {
		++ref;
		while (isspace(*ref)) ++ref;
		result->m_idx.push_back(parse_intval(&ref, 1));
		if (*ref != ']')  {
			throw Error{PDI_ERR_VALUE, "Expected ']', found %c", *ref};
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	if (has_curly_brace) {
		if (*ref != '}') {
			throw Error{PDI_ERR_VALUE, "Expected '}', found %c", *ref};
		}
		++ref;
	}
	
	*val_str = ref;
	return result;
}

unique_ptr<Constval> parse_const(char const **val_str)
{
	const char *constval = *val_str;
	
	long result = strtol(constval, (char **)&constval, 0);
	if (*val_str == constval) {
		throw Error{PDI_ERR_VALUE, "Expected integer, found `%s'", constval};
	}
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
	return unique_ptr<Constval>{new Constval{result}};
}

Value parse_term(char const **val_str)
{
	
	if ( **val_str == '(' ) {
		const char *term = *val_str;
		++term;
		while (isspace(*term)) ++term;
		Value result = parse_intval(&term, 1);
		if (*term != ')')  throw Error{PDI_ERR_VALUE, "Expected ')', found '%c'", *term};
		++term;
		while (isspace(*term)) ++term;
		*val_str = term;
		return result;
	} else if ( **val_str == '$' ) {
		return Value { parse_ref(val_str) };
	}
	
	return Value{parse_const(val_str)};
}

#define OP_LEVELS 6

int op_level(PDI_exprop_t op)
{
	switch (op) {
	case PDI_OP_OR: return 1;
	case PDI_OP_AND: return 2;
	case PDI_OP_EQUAL: return 3;
	case PDI_OP_GT: case PDI_OP_LT: return 4;
	case PDI_OP_PLUS: case PDI_OP_MINUS: return 5;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: return 6;
	}
	return 0;
}

PDI_exprop_t parse_op(char const **val_str, int level)
{
	const char *c_op = *val_str;
	PDI_exprop_t op = (PDI_exprop_t)(*c_op);
	
	int found_level = op_level(op);
	if (found_level == 0) {
		throw Error{PDI_ERR_VALUE, "Expected operator, found '%c'", *c_op};
	}
	if (found_level != level) {
		throw Error{PDI_ERR_VALUE, "Mixing operator priority"};
	}
	++c_op;
	
	while (isspace(*c_op)) ++c_op;
	
	*val_str = c_op;
	return op;
}

Value parse_intval(char const **val_str, int level)
{
	if ( level > OP_LEVELS ) return parse_term(val_str);
	const char *exprval = *val_str;
	
	Value result = parse_intval(&exprval, level + 1);
	
	/* little compression trick, we only build the Exprval if needed, otherwise
	   we return  the previous expression directly */
	unique_ptr<Exprval> expr = NULL;
	while ( op_level(static_cast<PDI_exprop_t>(*exprval)) == level ) {
		PDI_exprop_t op = parse_op(&exprval, level);
		if (!expr) {
			expr.reset(new Exprval);
			expr->values.push_back(move(result));
		}
		expr->ops.push_back(op);
		
		if (level >= OP_LEVELS) {
			expr->values.push_back(parse_term(&exprval));
		} else {
			expr->values.push_back(parse_intval(&exprval, level + 1));
		}
	}
	
	while (isspace(*exprval)) ++exprval;
	
	*val_str = exprval;
	if ( expr ) {
		return Value{move(expr)};
	} else {
		return result;
	}
}

Value parse_strval(char const **val_str)
{
	const char *str = *val_str;
	
	unique_ptr<Stringval> result{new Stringval};
	
	while (*str) {
		int sz = 0;
		while (str[sz] != '\\' && str[sz] != '$' && str[sz]) ++sz;
		result->str.append(str, sz);
		str += sz;
		switch (*str) {
		case '\\': {
			++str;
			result->str.append(str, 1);
			str += 1;
		} break;
		case '$': {
			switch (str[1]) {
			case '(': { // parse the term starting with the parenthesis (the intvl)
				++str;
				result->values.push_back({Value{parse_term(&str)}, static_cast<int>(result->str.size())});
			} break;
			default: { // parse the term starting with the dollar (the ref)
				result->values.push_back({Value{parse_term(&str)}, static_cast<int>(result->str.size())});
			} break;
			}
		} break;
		}
	}
	
	*val_str = str;
	return Value{move(result)};
}

} // namespace <anonymous>

string Value::Impl::to_string() const
{
	stringstream result;
	result << to_long();
	return result.str();
}

Value Value::parse(const char *val_str)
{
	const char *parse_val = val_str;
	
	try { // parse as a space enclosed intval
		while (isspace(*parse_val)) ++parse_val;
		Value result = parse_intval(&parse_val, 1);
		while (isspace(*parse_val)) ++parse_val;
		return result;
	} catch ( Error& e ) { // in case of error, parse as a string
		return parse_strval(&val_str);
	}
}

} // namespace PDI
