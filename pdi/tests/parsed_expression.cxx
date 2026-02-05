// SPDX-FileCopyrightText: 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <type_traits>

#include <gtest/gtest.h>

#include <pdi/expression.h>

#include "mocks/context_mock.h"

#include "operators.h"


// clang-format off
#cmakedefine OPERAND1_TYPE @OPERAND1_TYPE@
#cmakedefine OPERAND2_TYPE @OPERAND2_TYPE@
#cmakedefine CASE_NAME @CASE_NAME@
// clang-format on


using PDI::Expression;
using PDI::Ref_r;
using std::remove_cv_t;
using std::remove_reference_t;

using operand1_type = OPERAND1_TYPE;
using operand2_type = OPERAND2_TYPE;


// clang-format off
template <class T>
struct ParseableExpressionU@CASE_NAME@Test: public ::testing::Test{};
// clang-format on

// clang-format off
TYPED_TEST_CASE(ParseableExpressionU@CASE_NAME@Test, OperatorTypes);
// clang-format on


// clang-format off
TYPED_TEST(ParseableExpressionU@CASE_NAME@Test, toLong)
// clang-format on
{
	MockContext ctx;
	using operator_type = TypeParam;

	for (auto op1: PARSEABLE_VALS<operand1_type>) {
		for (auto op2: PARSEABLE_VALS<operand2_type>) {
			auto const & expr = fmt::format("{} {} {}", op1, operator_type::sign, op2);
			auto const & expected = operator_type::eval(op1, op2);
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<long>(expected), result.to_long(ctx)) << expr;
		}
	}
}

// clang-format off
TYPED_TEST(ParseableExpressionU@CASE_NAME@Test, toDouble)
// clang-format on
{
	MockContext ctx;

	using operator_type = TypeParam;

	for (auto op1: PARSEABLE_VALS<operand1_type>) {
		for (auto op2: PARSEABLE_VALS<operand2_type>) {
			auto const & expr = fmt::format("{} {} {}", op1, operator_type::sign, op2);
			auto const & expected = operator_type::eval(op1, op2);
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<double>(expected), result.to_double(ctx)) << expr;
		}
	}
}

// clang-format off
TYPED_TEST(ParseableExpressionU@CASE_NAME@Test, toRef)
// clang-format on
{
	MockContext ctx;

	using operator_type = TypeParam;

	for (auto op1: PARSEABLE_VALS<operand1_type>) {
		for (auto op2: PARSEABLE_VALS<operand2_type>) {
			auto const & expr = fmt::format("{} {} {}", op1, operator_type::sign, op2);
			auto const & expected = operator_type::eval(op1, op2);
			auto const & result = Ref_r(PDI::Expression(expr).to_ref(ctx)).scalar_value< remove_cv_t<remove_reference_t<decltype(expected)>>>();
			EXPECT_EQ(expected, result) << expr;
		}
	}
}
