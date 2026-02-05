/*
 * SPDX-FileCopyrightText: 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_EXPRESSION_IMPL_OPERATION_H_
#define PDI_EXPRESSION_IMPL_OPERATION_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a an operation
 */
struct PDI_NO_EXPORT Expression::Impl::Operation: Expression::Impl {
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
		LT = '<',
		GET = ']',
		LET = '['
	};

	using Operand = std::pair<Operator, Expression>;

	Expression m_first_operand;

	std::vector<Operand> m_operands;

	template <class O1>
	static Ref evalp(O1, Operator, Ref_r);

	template <class O1, class O2>
	static Ref eval(O1, Operator, O2);

	Operation();

	Operation(Expression first_operand, Operator op, Expression secend_operand);

	std::unique_ptr<Impl> clone() const override;

	double to_double(Context& ctx) const override;

	long to_long(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;

	size_t copy_value(Context& ctx, void* buffer, Datatype_sptr type) const override;

	static std::unique_ptr<Impl> parse(char const ** val_str, int level);

	static int op_level(const char* op);

	static Operator parse_operator(char const ** val_str, int level);
};

} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_OPERATION_H_
