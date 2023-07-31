/*******************************************************************************
 * Copyright (C) 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <type_traits>

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

using std::dynamic_pointer_cast;
using std::is_integral_v;
using std::make_shared;
using std::remove_cv_t;
using std::unique_ptr;

template <class O1, class O2>
Ref Expression::Impl::Operation::eval(O1 const computed_value, Operator const op, O2 const operand_value)
{
	switch (op) {
	case PLUS: {
		auto val = computed_value + operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case MINUS: {
		auto val = computed_value - operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case MULT: {
		auto val = computed_value * operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case DIV: {
		auto val = computed_value / operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case MOD: {
		if constexpr (is_integral_v<O1>&& is_integral_v<O2>) {
			auto val = computed_value % operand_value;
			return Ref(
			        new decltype(val)(val),
			[](void* p) {
				delete reinterpret_cast<decltype(val)*>(p);
			},
			Scalar_datatype::type_for_v<decltype(val)>,
			true,
			false
			    );
		} else {
			throw Type_error("Invalid operands to modulo operation");
		}
	} break;
	case EQUAL: {
		auto val = computed_value == operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case AND: {
		auto val = computed_value && operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case OR: {
		auto val = computed_value || operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case GT: {
		auto val = computed_value > operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case LT: {
		auto val = computed_value < operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case GET: {
		auto val = computed_value >= operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	case LET: {
		auto val = computed_value <= operand_value;
		return Ref(
		        new decltype(val)(val),
		[](void* p) {
			delete reinterpret_cast<decltype(val)*>(p);
		},
		Scalar_datatype::type_for_v<decltype(val)>,
		true,
		false
		    );
	} break;
	}
	throw Type_error("Unexpected type");
}

template <class O1>
Ref Expression::Impl::Operation::evalp(O1 const computed_value, Operator const op, Ref_r operand_ref)
{
	auto const operand_type = dynamic_pointer_cast<Scalar_datatype const>(operand_ref.type());
	if (!operand_type) {
		throw Type_error("Cannot apply operation on non-scalar value");
	}
	switch (operand_type->kind()) {
	case Scalar_kind::FLOAT: {
		switch (operand_type->datasize()) {
		case sizeof(float): {
			return eval(computed_value, op, operand_ref.scalar_value<float>());
		} break;
		case sizeof(double): {
			return eval(computed_value, op, operand_ref.scalar_value<double>());
		} break;
		default:
			throw Type_error("Unable to compute on floating point data of size {}", operand_type->datasize());
		}
	} break;
	case Scalar_kind::SIGNED: {
		switch (operand_type->datasize()) {
		case sizeof(int8_t): {
			return eval(computed_value, op, operand_ref.scalar_value<int8_t>());
		} break;
		case sizeof(int16_t): {
			return eval(computed_value, op, operand_ref.scalar_value<int16_t>());
		} break;
		case sizeof(int32_t): {
			return eval(computed_value, op, operand_ref.scalar_value<int32_t>());
		} break;
		case sizeof(int64_t): {
			return eval(computed_value, op, operand_ref.scalar_value<int64_t>());
		} break;
		default:
			throw Type_error("Unable to compute on integer data of size {}", operand_type->datasize());
		}
	} break;
	case Scalar_kind::UNSIGNED: {
		switch (operand_type->datasize()) {
		case sizeof(uint8_t): {
			return eval(computed_value, op, operand_ref.scalar_value<uint8_t>());
		} break;
		case sizeof(uint16_t): {
			return eval(computed_value, op, operand_ref.scalar_value<uint16_t>());
		} break;
		case sizeof(uint32_t): {
			return eval(computed_value, op, operand_ref.scalar_value<uint32_t>());
		} break;
		case sizeof(uint64_t): {
			return eval(computed_value, op, operand_ref.scalar_value<uint64_t>());
		} break;
		default:
			throw Type_error("Unable to compute on unsigned data of size {}", operand_type->datasize());
		}
	} break;
	default:
		throw Type_error("Unable to compute on data of unknown type");
	}
}

Expression::Impl::Operation::Operation() {}

Expression::Impl::Operation::Operation(Expression first_operand, Operator op, Expression secend_operand)
{
	if (!first_operand || !secend_operand) {
		throw Value_error{"Cannot call operation on empty expression with another expression"};
	}
	
	m_first_operand = first_operand;
	m_operands.emplace_back(op, secend_operand);
}

double Expression::Impl::Operation::to_double(Context& ctx) const
{
	Ref_r const ref_value = to_ref(ctx);
	if (!ref_value) {
		throw Value_error("Unexpected null value for operation");
	}
	return ref_value.scalar_value<double>();
}

long Expression::Impl::Operation::to_long(Context& ctx) const
{
	Ref_r const ref_value = to_ref(ctx);
	if (!ref_value) {
		throw Value_error("Unexpected null value for operation");
	}
	return ref_value.scalar_value<long>();
}

Ref Expression::Impl::Operation::to_ref(Context& ctx) const
{
	Ref_r computed_ref = m_first_operand.to_ref(ctx);
	for (auto&& op: m_operands) {
		auto computed_type = dynamic_pointer_cast<Scalar_datatype const>(computed_ref.type());
		if (!computed_type) {
			throw Type_error("Cannot apply operation on non-scalar value");
		}
		Ref_r const operand_ref = op.second.to_ref(ctx);
		switch (computed_type->kind()) {
		case Scalar_kind::FLOAT: {
			switch (computed_type->datasize()) {
			case sizeof(float): {
				computed_ref = evalp(computed_ref.scalar_value<float>(), op.first, operand_ref);
			} break;
			case sizeof(double): {
				computed_ref = evalp(computed_ref.scalar_value<double>(), op.first, operand_ref);
			} break;
			default:
				throw Type_error("Unable to compute on floating point data of size {}", computed_type->datasize());
			}
		} break;
		case Scalar_kind::SIGNED: {
			switch (computed_type->datasize()) {
			case sizeof(int8_t): {
				computed_ref = evalp(computed_ref.scalar_value<int8_t>(), op.first, operand_ref);
			} break;
			case sizeof(int16_t): {
				computed_ref = evalp(computed_ref.scalar_value<int16_t>(), op.first, operand_ref);
			} break;
			case sizeof(int32_t): {
				computed_ref = evalp(computed_ref.scalar_value<int32_t>(), op.first, operand_ref);
			} break;
			case sizeof(int64_t): {
				computed_ref = evalp(computed_ref.scalar_value<int64_t>(), op.first, operand_ref);
			} break;
			default:
				throw Type_error("Unable to compute on integer data of size {}", computed_type->datasize());
			}
		} break;
		case Scalar_kind::UNSIGNED: {
			switch (computed_type->datasize()) {
			case sizeof(uint8_t): {
				computed_ref = evalp(computed_ref.scalar_value<uint8_t>(), op.first, operand_ref);
			} break;
			case sizeof(uint16_t): {
				computed_ref = evalp(computed_ref.scalar_value<uint16_t>(), op.first, operand_ref);
			} break;
			case sizeof(uint32_t): {
				computed_ref = evalp(computed_ref.scalar_value<uint32_t>(), op.first, operand_ref);
			} break;
			case sizeof(uint64_t): {
				computed_ref = evalp(computed_ref.scalar_value<uint64_t>(), op.first, operand_ref);
			} break;
			default:
				throw Type_error("Unable to compute on unsigned data of size {}", computed_type->datasize());
			}
		} break;
		default:
			throw Type_error("Unable to compute on data of unknown type");
		}
	}
	return computed_ref;
}

template <class T>
size_t from_long_cpy(void* buffer, long value_long)
{
	T value = static_cast<T>(value_long);
	memcpy(buffer, &value, sizeof(T));
	return sizeof(T);
}

size_t Expression::Impl::Operation::copy_value(Context& ctx, void* buffer, Datatype_sptr type) const
{
	Ref_r value = to_ref(ctx);
	if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
		switch (scalar_type->kind()) {
		case Scalar_kind::FLOAT: {
			switch (scalar_type->datasize()) {
			case sizeof(float): {
				*reinterpret_cast<float*>(buffer) = value.scalar_value<float>();
			} break;
			case sizeof(double): {
				*reinterpret_cast<double*>(buffer) = value.scalar_value<double>();
			} break;
			default:
				throw Type_error("Cannot copy operation expression value to floating point data of size {}", scalar_type->datasize());
			}
		} break;
		case Scalar_kind::SIGNED: {
			switch (scalar_type->datasize()) {
			case sizeof(int8_t): {
				*reinterpret_cast<int8_t*>(buffer) = value.scalar_value<int8_t>();
			} break;
			case sizeof(int16_t): {
				*reinterpret_cast<int16_t*>(buffer) = value.scalar_value<int16_t>();
			} break;
			case sizeof(int32_t): {
				*reinterpret_cast<int32_t*>(buffer) = value.scalar_value<int32_t>();
			} break;
			case sizeof(int64_t): {
				*reinterpret_cast<int64_t*>(buffer) = value.scalar_value<int64_t>();
			} break;
			default:
				throw Type_error("Cannot copy operation expression value to integer data of size {}", scalar_type->datasize());
			}
		} break;
		case Scalar_kind::UNSIGNED: {
			switch (scalar_type->datasize()) {
			case sizeof(uint8_t): {
				*reinterpret_cast<uint8_t*>(buffer) = value.scalar_value<uint8_t>();
			} break;
			case sizeof(uint16_t): {
				*reinterpret_cast<uint16_t*>(buffer) = value.scalar_value<uint16_t>();
			} break;
			case sizeof(uint32_t): {
				*reinterpret_cast<uint32_t*>(buffer) = value.scalar_value<uint32_t>();
			} break;
			case sizeof(uint64_t): {
				*reinterpret_cast<uint64_t*>(buffer) = value.scalar_value<uint64_t>();
			} break;
			default:
				throw Type_error("Cannot copy operation expression value to unsigned data of size {}", scalar_type->datasize());
			}
		} break;
		default:
			throw Type_error{"Cannot copy operation expression value: unknown type"};
		}
		return scalar_type->datasize();
	}
	throw Value_error{"Cannot copy operation expression value to non scalar datatype"};
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::clone() const
{
	unique_ptr<Operation> result{new Operation};
	result->m_first_operand = m_first_operand;
	for (const auto& element: m_operands) {
		result->m_operands.emplace_back(element.first, element.second);
	}
	return result;
}

unique_ptr<Expression::Impl> Expression::Impl::Operation::parse(char const** val_str, int level)
{
	// a level 7 operation is a term
	if (level == 7) {
		return parse_term(val_str);
	}
	
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
	case OR:
		return 1;
	case AND:
		return 2;
	case EQUAL:
		return 3;
	case GT:
	case LT:
		return 4;
	case PLUS:
	case MINUS:
		return 5;
	case MULT:
	case DIV:
	case MOD:
		return 6;
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
	
	while (isspace(*c_op)) {
		++c_op;
	}
	
	*val_str = c_op;
	return op;
}

} // namespace PDI
