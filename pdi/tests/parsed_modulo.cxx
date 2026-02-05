// SPDX-FileCopyrightText: 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <type_traits>

#include <gtest/gtest.h>

#include <pdi/expression.h>

#include "mocks/context_mock.h"
#include "mocks/data_descriptor_mock.h"

#include "operators.h"


using PDI::Expression;
using PDI::Ref_r;
using std::remove_cv_t;
using std::remove_reference_t;

TEST(ParsedModuloTest, toLong)
{
	MockContext ctx;

	for (auto op1: PARSEABLE_VALS<long>) {
		for (auto op2: PARSEABLE_VALS<long>) {
			auto const & expr = fmt::format("{} % {}", op1, op2);
			auto const & expected = op1 % op2;
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<long>(expected), result.to_long(ctx)) << expr;
		}
	}
}

TEST(ParsedModuloTest, toDouble)
{
	MockContext ctx;

	for (auto op1: PARSEABLE_VALS<long>) {
		for (auto op2: PARSEABLE_VALS<long>) {
			auto const & expr = fmt::format("{} % {}", op1, op2);
			auto const & expected = op1 % op2;
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<double>(expected), result.to_double(ctx)) << expr;
		}
	}
}

TEST(ParsedModuloTest, toRef)
{
	MockContext ctx;

	for (auto op1: PARSEABLE_VALS<long>) {
		for (auto op2: PARSEABLE_VALS<long>) {
			auto const & expr = fmt::format("{} % {}", op1, op2);
			auto const & expected = op1 % op2;
			auto const & result = Ref_r(PDI::Expression(expr).to_ref(ctx)).scalar_value< remove_cv_t<remove_reference_t<decltype(expected)>>>();
			EXPECT_EQ(expected, result) << expr;
		}
	}
}
