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

#include "operation.h"

namespace PDI {

using std::unique_ptr;

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

double Expression::Impl::Operation::to_double(Context& ctx) const
{
	double computed_value = m_first_operand.to_double(ctx);
	for (auto&& op: m_operands) {
		double operand = op.second.to_double(ctx);
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
			throw Value_error("Cannot use modulus operator on float values");
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
	try {
		Ref_rw result {
			aligned_alloc(alignof(long), sizeof(long)),
			[](void* v){free(v);},
			unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},
			true,
			true
		};
		*static_cast<long*>(result.get()) = to_long(ctx);
		return result;
	} catch (const Value_error& e) {
		Ref_rw result {
			aligned_alloc(alignof(double), sizeof(double)),
			[](void* v){free(v);},
			unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_kind::FLOAT, sizeof(double)}},
			true,
			true
		};
		*static_cast<double*>(result.get()) = to_double(ctx);
		return result;
	}
}

Ref Expression::Impl::Operation::to_ref(Context& ctx, const Datatype& type) const
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

template<class T>
size_t from_long_cpy(void* buffer, long value_long)
{
	T value = static_cast<T>(value_long);
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Operation::copy_value(Context& ctx, void* buffer, const Datatype& type) const
{
	if (const Scalar_datatype* scalar_type = dynamic_cast<const Scalar_datatype*>(&type)) {
		if (scalar_type->kind() == Scalar_kind::UNSIGNED && type.buffersize() == (long)sizeof(char)) {
			return from_long_cpy<unsigned char>(buffer, to_long(ctx));
		} else if (scalar_type->kind() == Scalar_kind::SIGNED) {
			switch (type.buffersize()) {
			case 1L:
				return from_long_cpy<signed char>(buffer, to_long(ctx));
			case 2L:
				return from_long_cpy<short>(buffer, to_long(ctx));
			case 4L:
				return from_long_cpy<int>(buffer, to_long(ctx));
			case 8L:
				return from_long_cpy<long>(buffer, to_long(ctx));
			default:
				break;
			}
		} else if (scalar_type->kind() == Scalar_kind::FLOAT) {
			switch (type.buffersize()) {
			case 4L: {
				float value = static_cast<float>(to_double(ctx));
				memcpy(buffer, &value, sizeof(float));
				return sizeof(float);
			}
			case 8L: {
				double value = to_double(ctx);
				memcpy(buffer, &value, sizeof(double));
				return sizeof(double);
			}
			default:
				break;
			}
		}
	}
	throw Value_error{"Cannot copy operation expression value: non scalar datatype"};
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::clone() const
{
	unique_ptr<Operation> result {new Operation};
	result->m_first_operand = m_first_operand;
	for (const auto& element : m_operands) {
		result->m_operands.emplace_back(element.first, element.second);
	}
	return result;
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::parse(char const** val_str, int level)
{
	// a level 7 operation is a term
	if (level == 7) return parse_term(val_str);
	
	const char* exprval = *val_str;
	unique_ptr<Impl> result = parse(&exprval, level + 1);
	
	// we only build the Operation if needed, otherwise we return the previous
	// expression directly
	unique_ptr<Operation> expr = NULL;
	while (op_level(exprval) == level) {
		if (!expr) {
			expr.reset(new Operation);
			expr->m_first_operand = move(result);
		}
		Operator oper = parse_operator(&exprval, level);
		expr->m_operands.emplace_back(oper, Expression{parse(&exprval, level + 1)});
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
		throw Value_error{"Expected operator, found '{}'", *c_op};
	}
	if (found_level != level) {
		throw Value_error{"Mixing operator priority"};
	}
	Operator op = static_cast<Operator>(*c_op);
	++c_op;
	
	while (isspace(*c_op)) ++c_op;
	
	*val_str = c_op;
	return op;
}

} // namespace PDI
