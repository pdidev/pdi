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

#include "config.h"

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <vector>

#include "pdi/data_reference.h"
#include "pdi/state.h"
#include "pdi/status.h"

#include "utils.h"

#include "pdi/value.h"

namespace PDI
{

using std::cout;
using std::endl;
using std::move;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

namespace
{

struct Value_parser:
	public Value {
	
	/** The binary operators that can be used in values
	 */
	enum Operator {
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
		public Value::Impl {
		long m_value;
		
		Constval(long value) : m_value(value) {}
		
		long to_long() const override
		{
			return m_value;
		}
		
		unique_ptr<Impl> clone() const override
		{
			return unique_ptr<Constval> {new Constval{*this}};
		}
		
	};
	
	/** A value in case this is a string (potentially with dollar refs inside)
	 */
	struct Stringval:
		public Impl {
		/// a char string containing the beginning str_value
		string start;
		
		/** A Subvalue contains another value to insert and the string following it
		 */
		struct Subvalue {
			Value value;
			string str;
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
			result << start;
			for (auto &&subval : values) {
				result << subval.value.to_string() << subval.str;
			}
			return result.str();
		}
		
		unique_ptr<Impl> clone() const override
		{
			return unique_ptr<Stringval> {new Stringval{*this}};
		}
		
	};
	
	/** A value in case this is an expression
	 */
	struct Exprval:
		public Impl {
		vector<Value> values;
		
		vector<Operator> ops;
		
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
					throw Error{PDI_ERR_VALUE, "Unknown operator: `%c'", ops[ii - 1]};
				}
				}
			}
			
			return computed_value;
		}
		
		unique_ptr<Impl> clone() const override
		{
			return unique_ptr<Exprval> {new Exprval{*this}};
		}
		
	};
	
	/** A value in case this is a reference to another value
	 */
	struct Refval:
		public Impl {
		/// The referenced data
		Data_descriptor *m_referenced;
		
		/// Indexes in case the referenced data is an array
		vector<Value> m_idx;
		
		Refval(Data_descriptor *desc): m_referenced(desc) {}
		
		long to_long() const override
		{
			Data_r_ref ref = m_referenced->value();
			
			Scalar_datatype type;
			long idx = 0;
			
			if (ref.type().kind == PDI_K_ARRAY) {
				const Array_datatype &array_type = *ref.type().c.array;
				if (m_idx.size() != array_type.m_dimensions.size()) {
					throw Error{PDI_ERR_VALUE, "Invalid number of index: %d, %d expected", m_idx.size(), static_cast<int>(array_type.m_dimensions.size()) };
				}
				long stride = 1;
				for (size_t ii = 0; ii < m_idx.size(); ++ii) {
					long start = array_type.m_dimensions[ii].m_start;
					long index = m_idx[ii];
					idx += (start + index) * stride;
					long size = array_type.m_dimensions[ii].m_size;
					stride *= size;
				}
				if (array_type.type.kind != PDI_K_SCALAR) {
					throw Error{PDI_ERR_VALUE, "Invalid type accessed"};
				}
				type = array_type.type.c.scalar;
			} else if (ref.type().kind == PDI_K_SCALAR) {
				if (!m_idx.empty()) {
					throw Error{PDI_ERR_VALUE, "Invalid number of index: %d, 0 expected", m_idx.size()};
				}
				type = ref.type().c.scalar;
			} else {
				throw Error{PDI_ERR_VALUE, "Invalid access to a struct"};
			}
			
			switch (type) {
			case PDI_T_INT8:
				return static_cast<int8_t *>(ref.get())[idx];
			case PDI_T_INT16:
				return static_cast<int16_t *>(ref.get())[idx];
			case PDI_T_INT32:
				return static_cast<int32_t *>(ref.get())[idx];
			case PDI_T_INT64:
				return static_cast<int64_t *>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Non-integer type accessed");
			}
			assert(false);
			return -1;
		}
		
		unique_ptr<Impl> clone() const override
		{
			return unique_ptr<Refval> {new Refval{*this}};
		}
		
	};
	
	static std::string parse_id(char const **val_str);
	
	static Value parse_ref(char const **val_str);
	
	static Value parse_const(char const **val_str);
	
	static Value parse_term(char const **val_str);
	
	static Value parse_intval(char const **val_str, int level);
	
	static Value parse_strval(char const **val_str);
	
	static Operator parse_op(char const **val_str, int level);
	
	static int op_level(const char *op);
	
};

string Value_parser::parse_id(char const **val_str)
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

Value Value_parser::parse_ref(char const **val_str)
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
	return make_value(move(result));
}

Value Value_parser::parse_const(char const **val_str)
{
	const char *constval = *val_str;
	
	long result = strtol(constval, (char **)&constval, 0);
	if (*val_str == constval) {
		throw Error{PDI_ERR_VALUE, "Expected integer, found `%s'", constval};
	}
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
	return make_value(unique_ptr<Constval> {new Constval{result}});
}

Value Value_parser::parse_term(char const **val_str)
{

	if (**val_str == '(') {
		const char *term = *val_str;
		++term;
		while (isspace(*term)) ++term;
		Value result = parse_intval(&term, 1);
		if (*term != ')')  throw Error{PDI_ERR_VALUE, "Expected ')', found '%c'", *term};
		++term;
		while (isspace(*term)) ++term;
		*val_str = term;
		return result;
	} else if (**val_str == '$') {
		return parse_ref(val_str);
	}
	
	return Value{parse_const(val_str)};
}

#define OP_LEVELS 6

int Value_parser::op_level(const char *op)
{
	switch (*op) {
	case PDI_OP_OR: return 1;
	case PDI_OP_AND: return 2;
	case PDI_OP_EQUAL: return 3;
	case PDI_OP_GT: case PDI_OP_LT: return 4;
	case PDI_OP_PLUS: case PDI_OP_MINUS: return 5;
	case PDI_OP_MULT: case PDI_OP_DIV: case PDI_OP_MOD: return 6;
	}
	return 0;
}

Value_parser::Operator Value_parser::parse_op(char const **val_str, int level)
{
	const char *c_op = *val_str;
	int found_level = op_level(c_op);
	if (found_level == 0) {
		throw Error{PDI_ERR_VALUE, "Expected operator, found '%c'", *c_op};
	}
	if (found_level != level) {
		throw Error{PDI_ERR_VALUE, "Mixing operator priority"};
	}
	Operator op = (Operator)(*c_op);
	++c_op;
	
	while (isspace(*c_op)) ++c_op;
	
	*val_str = c_op;
	return op;
}

Value Value_parser::parse_intval(char const **val_str, int level)
{
	if (level > OP_LEVELS) return parse_term(val_str);
	const char *exprval = *val_str;
	
	Value result = parse_intval(&exprval, level + 1);
	
	/* little compression trick, we only build the Exprval if needed, otherwise
	   we return  the previous expression directly */
	unique_ptr<Exprval> expr = NULL;
	while (op_level(exprval) == level) {
		Operator op = parse_op(&exprval, level);
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
	if (expr) {
		return make_value(move(expr));
	} else {
		return result;
	}
}

Value Value_parser::parse_strval(char const **val_str)
{
	const char *str = *val_str;
	
	unique_ptr<Stringval> result{new Stringval};
	
	string *curstr = &result->start;
	while (*str) {
		int sz = 0;
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
			case '(': { // remove the dollar, parse the term starting with the parenthesis (the intvl)
				++str;
				result->values.push_back({Value{parse_term(&str)}, ""});
				curstr = &result->values.back().str;
			} break;
			default: { // parse the term starting with the dollar (the ref)
				result->values.push_back({Value{parse_term(&str)}, ""});
				curstr = &result->values.back().str;
			} break;
			}
		} break;
		case 0: {} break;
		default: {
			throw Error{PDI_ERR_IMPL, "Unexpected error!!!"};
		}
		}
	}
	
	*val_str = str;
	return make_value(move(result));
}

};

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
		Value result = Value_parser::parse_intval(&parse_val, 1);
		while (isspace(*parse_val)) ++parse_val;
		if (!*parse_val) return result; // only return this is we parsed the whole string, otherwise, parse as a string
	} catch (Error &e) {   // in case of error, parse as a string
	}
	return Value_parser::parse_strval(&val_str);
}

} // namespace PDI
