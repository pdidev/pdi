/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
