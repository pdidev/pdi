/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <sstream>
#include <utility>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"
#include "pdi/error.h"
#include "pdi/datatype.h"

#include "pdi/expression.h"


namespace PDI {

using std::move;
using std::pair;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

struct Expression::Impl {

	/** An expression made of a integer literal
	 */
	struct Int_literal;
	
	/** An expression made of a string literal (with potential dollar refs)
	 */
	struct String_literal;
	
	/** An expression made of an operation
	 */
	struct Operation;
	
	/** An expression made of a reference to a data
	*/
	struct Reference_expression;
	
	
	virtual ~Impl();
	
	virtual unique_ptr<Impl> clone() const = 0;
	
	virtual long to_long(Context&) const = 0;
	
	virtual string to_string(Context&) const;
	
	virtual Ref to_ref(Context&) const = 0;
	
	virtual MPI_Comm to_mpi_comm(Context&) const;
	
	static unique_ptr<Impl> parse_term(char const** val_str);
	
	static string parse_id(char const** val_str);
	
};

struct Expression::Impl::Int_literal: Impl {

	long m_value;
	
	
	Int_literal(long value);
	
	unique_ptr<Impl> clone() const override;
	
	long to_long(Context&) const override;
	
	Ref to_ref(Context&) const override;
	
	static unique_ptr<Impl> parse(char const** val_str);
	
};

struct Expression::Impl::String_literal: Impl {

	/** A Subvalue contains another value to insert and the string following it
	 */
	using Subvalue = pair<Expression, string>;
	
	
	/// a char string containing the beginning str_value
	string start;
	
	/// array of subvalues
	vector<Subvalue> values;
	
	
	unique_ptr<Impl> clone() const override;
	
	long to_long(Context& ctx) const override;
	
	string to_string(Context& ctx) const override;
	
	Ref to_ref(Context& ctx) const override;
	
	static unique_ptr<Impl> parse(char const** val_str);
	
};

struct Expression::Impl::Reference_expression: Impl {

	/// The referenced data
	string m_referenced;
	
	/// Indexes in case the referenced data is an array
	vector<Expression> m_idx;
	
	
	unique_ptr<Impl> clone() const override;
	
	long to_long(Context& ctx) const override;
	
	Ref to_ref(Context& ctx) const override;
	
	static unique_ptr<Impl> parse(char const** val_str);
	
};

struct Expression::Impl::Operation: Impl {

	/** The binary operators that can be used in expressions
	 */
	enum Operator {
		PLUS = '+',
		MINUS = '-',
		MULT = '*',
		DIV = '/',
		MOD = '%',
		EQUAL = '=',
		AND = '&',
		OR = '|',
		GT = '>',
		LT = '<'
	};
	
	using Operand = pair<Operator, Expression>;
	
	
	Expression m_first_operand;
	
	vector<Operand> m_operands;
	
	
	unique_ptr<Impl> clone() const override;
	
	long to_long(Context& ctx) const override;
	
	Ref to_ref(Context& ctx) const override;
	
	static unique_ptr<Impl> parse(char const** val_str, int level);
	
	static int op_level(const char* op);
	
	static Operator parse_operator(char const** val_str, int level);
	
};


Expression::Expression(unique_ptr<Impl> impl):
	m_impl(move(impl))
{}

Expression::Expression() = default;

Expression::Expression(const Expression& value)
{
	if (value)
		m_impl = value.m_impl->clone();
		
}

Expression::Expression(Expression&&  value) = default;

Expression::Expression(const char* val_str)
{
	try { // parse as a space enclosed intval
		const char* parse_val = val_str;
		while (isspace(*parse_val)) ++parse_val;
		m_impl = Impl::Operation::parse(&parse_val, 1);
		while (isspace(*parse_val)) ++parse_val;
		if (!*parse_val) return; // take this if we parsed the whole string, otherwise, parse as a string
	} catch (Error&) {}
	// in case of error, parse as a string
	m_impl = Impl::String_literal::parse(&val_str);
}

Expression::Expression(const string& val_str):
	Expression {val_str.c_str()}
{
}

Expression::Expression(long value):
	m_impl{new Expression::Impl::Int_literal{value}}
{
}

Expression::~Expression() = default;

Expression& Expression::operator=(const Expression& value)
{
	m_impl.reset(nullptr);
	if (value)
		m_impl = value.m_impl->clone();
	return *this;
}

Expression& Expression::operator=(Expression&& value) = default;

long Expression::to_long(Context& ctx) const
{
	return m_impl->to_long(ctx);
}

Expression::operator bool () const
{
	return static_cast<bool>(m_impl);
}

string Expression::to_string(Context& ctx) const
{
	return m_impl->to_string(ctx);
}

Ref Expression::to_ref(Context& ctx) const
{
	return m_impl->to_ref(ctx);
}

MPI_Comm Expression::to_mpi_comm(Context& ctx) const
{
	return m_impl->to_mpi_comm(ctx);
}

Expression::Impl::~Impl() = default;

string Expression::Impl::to_string(Context& ctx) const
{
	long lres = to_long(ctx);
	stringstream result;
	result << lres;
	return result.str();
}


MPI_Comm Expression::Impl::to_mpi_comm(Context& ctx) const
{
	try {
		Ref_r comm_ref = to_ref(ctx);
		
		if (auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(&comm_ref.type())) {
			if (scalar_type->kind() == Scalar_kind::MPI_COMM) {
				const MPI_Comm* comm = static_cast<const MPI_Comm*>(comm_ref.get());
				return *comm;
			}
		}
	} catch (Error e) {
		throw Error{PDI_ERR_TYPE, "Defined communicator is not a reference to MPI_Comm"};
	}
	throw Error{PDI_ERR_TYPE, "Trying to cast to MPI_Comm something that is not a MPI_Comm"};
}

unique_ptr<Expression::Impl> Expression::Impl::parse_term(char const** val_str)
{
	if (**val_str == '(') {
		const char* term = *val_str;
		++term;
		while (isspace(*term)) ++term;
		unique_ptr<Expression::Impl> result = Operation::parse(&term, 1);
		if (*term != ')')  throw Error {PDI_ERR_VALUE, "Expected ')', found '%c'", *term};
		++term;
		while (isspace(*term)) ++term;
		*val_str = term;
		return result;
	} else if (**val_str == '$') {
		return Reference_expression::parse(val_str);
	} else {
		return Int_literal::parse(val_str);
	}
}

string Expression::Impl::parse_id(char const** val_str)
{
	const char* id = *val_str;
	
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

Expression::Impl::Int_literal::Int_literal(long value) : m_value(value) {}

long Expression::Impl::Int_literal::to_long(Context&) const
{
	return m_value;
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::clone() const
{
	return unique_ptr<Int_literal> {new Int_literal{*this}};
}

Ref Expression::Impl::Int_literal::to_ref(Context&) const
{
	return Ref {
		new long{m_value},
		[](void* v){delete static_cast<long*>(v);},
		unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},
		true,
		true
	};
}

unique_ptr<Expression::Impl> Expression::Impl::Int_literal::parse(char const** val_str)
{
	const char* constval = *val_str;
	
	unique_ptr<Int_literal> result {new Int_literal{strtol(constval, const_cast<char**>(&constval), 0)}};
	if (*val_str == constval) {
		throw Error {PDI_ERR_VALUE, "Expected integer, found `%s'", constval};
	}
	while (isspace(*constval)) ++constval;
	
	*val_str = constval;
	return result;
}

unique_ptr<Expression::Impl> Expression::Impl::String_literal::clone() const
{
	return unique_ptr<String_literal> {new String_literal{*this}};
}

string Expression::Impl::String_literal::to_string(Context& ctx) const
{
	string result = start;
	for (auto&& subval : values) {
		result += subval.first.to_string(ctx);
		result += subval.second;
	}
	return result;
}

long Expression::Impl::String_literal::to_long(Context& ctx) const
{
	throw Error {PDI_ERR_VALUE, "Can not interpret `%s' as an integer value", to_string(ctx).c_str()};
}

Ref Expression::Impl::String_literal::to_ref(Context& ctx) const
{
	string value = to_string(ctx);
	
	// copy because string does not provide a release call
	unique_ptr<char[]> str {new char[value.length() + 1]};
	memcpy(str.get(), value.c_str(), value.length() + 1);
	
	return Ref {
		str.release(),
		[](void* v){delete[] static_cast<char*>(v);},
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

unique_ptr<Expression::Impl> Expression::Impl::String_literal::parse(char const** val_str)
{
	const char* str = *val_str;
	
	unique_ptr<String_literal> result{new String_literal};
	
	string* curstr = &result->start;
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
				result->values.emplace_back(Expression{parse_term(&str)}, "");
				curstr = &result->values.back().second;
			} break;
			default: { // parse the term starting with the dollar (the ref)
				result->values.emplace_back(Expression{Reference_expression::parse(&str)}, "");
				curstr = &result->values.back().second;
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

unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::clone() const
{
	return unique_ptr<Reference_expression> {new Reference_expression{*this}};
}

long Expression::Impl::Reference_expression::to_long(Context& ctx) const
try
{
	if (Ref_r ref = ctx.desc(m_referenced.c_str()).ref()) {
		const Datatype* type = &ref.type();
		long stride = 1;
		long idx = 0;
		for (auto&& ii : m_idx) {
			auto&& array_type = dynamic_cast<const Array_datatype*>(type);
			if (!array_type) throw Error {PDI_ERR_VALUE, "Accessing non-array data with an index"};
			idx += (array_type->start() + ii.to_long(ctx)) * stride;
			stride *= array_type->size();
			type = &array_type->subtype();
		}
		
		auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(type);
		if (!scalar_type) throw Error {PDI_ERR_VALUE, "Expected scalar found invalid type instead"};
		
		if (scalar_type->kind() == Scalar_kind::SIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<const int8_t*>(ref.get())[idx];
			case 2:
				return static_cast<const int16_t*>(ref.get())[idx];
			case 4:
				return static_cast<const int32_t*>(ref.get())[idx];
			case 8:
				return static_cast<const int64_t*>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected int size: %ld", static_cast<long>(scalar_type->kind()));
			}
		} else if (scalar_type->kind() == Scalar_kind::UNSIGNED) {
			switch (scalar_type->datasize()) {
			case 1:
				return static_cast<const uint8_t*>(ref.get())[idx];
			case 2:
				return static_cast<const uint16_t*>(ref.get())[idx];
			case 4:
				return static_cast<const uint32_t*>(ref.get())[idx];
			case 8:
				return static_cast<const uint64_t*>(ref.get())[idx];
			default:
				throw Error(PDI_ERR_VALUE, "Unexpected uint size: %ld", static_cast<long>(scalar_type->kind()));
			}
		}
		throw Error {PDI_ERR_VALUE, "Expected integer scalar"};
	}
	throw Error {PDI_ERR_RIGHT, "Unable to grant access for value reference"};
} catch (const Error& e)
{
	throw Error {e.status(), "while referencing `%s': %s", m_referenced.c_str(), e.what()};
}

Ref Expression::Impl::Reference_expression::to_ref(Context& ctx) const
{
	if (!m_idx.empty()) {
		return Ref {
			new long{to_long(ctx)},
			[](void* v){delete static_cast<long*>(v);},
			unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},
			true,
			true
		};
	}
	return ctx.desc(m_referenced.c_str()).ref();
}

unique_ptr<Expression::Impl> Expression::Impl::Reference_expression::parse(char const** val_str)
{
	const char* ref = *val_str;
	unique_ptr<Reference_expression> result{new Reference_expression};
	
	if (*ref != '$') throw Error {PDI_ERR_VALUE, "Expected '$', got %c", *ref};
	++ref;
	
	bool has_curly_brace = false;
	if (*ref == '{') {
		++ref;
		has_curly_brace = true;
		while (isspace(*ref)) ++ref;
	}
	
	result->m_referenced = parse_id(&ref);
	
	while (isspace(*ref)) ++ref;
	
	while (*ref == '[') {
		++ref;
		while (isspace(*ref)) ++ref;
		result->m_idx.emplace_back(Expression{Operation::parse(&ref, 1)});
		if (*ref != ']')  {
			throw Error {PDI_ERR_VALUE, "Expected ']', found %c", *ref};
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	if (has_curly_brace) {
		if (*ref != '}') {
			throw Error {PDI_ERR_VALUE, "Expected '}', found %c", *ref};
		}
		++ref;
		while (isspace(*ref)) ++ref;
	}
	
	*val_str = ref;
	return result;
}

long Expression::Impl::Operation::to_long(Context& ctx) const
{
	long computed_value = m_first_operand.to_long(ctx);
	for (auto&& op: m_operands) {
		long operand = op.second.to_long(ctx);
		switch (op.first) {
		case PLUS: {
			computed_value += operand;
		} break;
		case MINUS: {
			computed_value -= operand;
		} break;
		case MULT: {
			computed_value *= operand;
		} break;
		case DIV: {
			computed_value /= operand;
		} break;
		case MOD: {
			computed_value %= operand;
		} break;
		case EQUAL: {
			computed_value = (computed_value == operand);
		} break;
		case AND: {
			computed_value = computed_value && operand;
		} break;
		case OR: {
			computed_value = computed_value || operand;
		} break;
		case GT: {
			computed_value = (computed_value > operand);
		} break;
		case LT: {
			computed_value = (computed_value < operand);
		} break;
		}
	}
	
	return computed_value;
}

Ref Expression::Impl::Operation::to_ref(Context& ctx) const
{
	return Ref {
		new long{to_long(ctx)},
		[](void* v){delete static_cast<long*>(v);},
		unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},
		true,
		true
	};
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::clone() const
{
	return unique_ptr<Operation> {new Operation{*this}};
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::parse(char const** val_str, int level)
{
	// a level 7 operation is a term
	if (level == 7) return parse_term(val_str);
	
	const char* exprval = *val_str;
	unique_ptr<Expression::Impl> result = parse(&exprval, level + 1);
	
	// we only build the Operation if needed, otherwise we return the previous
	// expression directly
	unique_ptr<Operation> expr = NULL;
	while (op_level(exprval) == level) {
		if (!expr) {
			expr.reset(new Operation);
			expr->m_first_operand = move(result);
		}
		Operator oper = parse_operator(&exprval, level);
		unique_ptr<Expression::Impl> operand = parse(&exprval, level + 1);
		expr->m_operands.emplace_back(oper, Expression{move(operand)});
	}
	
	*val_str = exprval;
	if (expr) {
		return expr;
	} else {
		return result;
	}
}

int Expression::Impl::Operation::op_level(const char* op)
{
	switch (*op) {
	case OR: return 1;
	case AND: return 2;
	case EQUAL: return 3;
	case GT: case LT: return 4;
	case PLUS: case MINUS: return 5;
	case MULT: case DIV: case MOD: return 6;
	}
	return 0;
}

Expression::Impl::Operation::Operator Expression::Impl::Operation::parse_operator(char const** val_str, int level)
{
	const char* c_op = *val_str;
	int found_level = op_level(c_op);
	if (found_level == 0) {
		throw Error {PDI_ERR_VALUE, "Expected operator, found '%c'", *c_op};
	}
	if (found_level != level) {
		throw Error {PDI_ERR_VALUE, "Mixing operator priority"};
	}
	Operator op = static_cast<Operator>(*c_op);
	++c_op;
	
	while (isspace(*c_op)) ++c_op;
	
	*val_str = c_op;
	return op;
}

} // namespace PDI
