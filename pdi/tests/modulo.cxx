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


#cmakedefine OPERAND1_TYPE @OPERAND1_TYPE@
#cmakedefine OPERAND2_TYPE @OPERAND2_TYPE@


using PDI::Expression;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Scalar_datatype;
using std::remove_cv_t;
using std::remove_reference_t;

using operand1_type = OPERAND1_TYPE;
using operand2_type = OPERAND2_TYPE;

TEST(ModuloU@CASE_NAME@Test, toLong)
{
	MockContext ctx;

	MockDataDescriptor o1_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o1"))))
			.WillRepeatedly(testing::ReturnRef(o1_desc));

	MockDataDescriptor o2_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o2"))))
			.WillRepeatedly(testing::ReturnRef(o2_desc));

	for (auto op1 : ALL_VALS<operand1_type>) {
		for (auto op2 : ALL_VALS<operand2_type>) {
			EXPECT_CALL(o1_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op1,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op1)>,
							true,
							false
					)));
			EXPECT_CALL(o2_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op2,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op2)>,
							true,
							false
					)));

			auto const & expr = "${o1} % ${o2}";
			auto const & expected = op1 % op2;
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<long>(expected), result.to_long(ctx)) << expr;
		}
	}
}

TEST(ModuloU@CASE_NAME@Test, toDouble)
{
	MockContext ctx;

	MockDataDescriptor o1_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o1"))))
			.WillRepeatedly(testing::ReturnRef(o1_desc));

	MockDataDescriptor o2_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o2"))))
			.WillRepeatedly(testing::ReturnRef(o2_desc));

	for (auto op1 : ALL_VALS<operand1_type>) {
		for (auto op2 : ALL_VALS<operand2_type>) {
			EXPECT_CALL(o1_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op1,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op1)>,
							true,
							false
					)));
			EXPECT_CALL(o2_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op2,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op2)>,
							true,
							false
					)));

			auto const & expr = "${o1} % ${o2}";
			auto const & expected = op1 % op2;
			auto const & result = PDI::Expression(expr);
			EXPECT_EQ(static_cast<double>(expected), result.to_double(ctx)) << expr;
		}
	}
}

TEST(ModuloU@CASE_NAME@Test, toRef)
{
	MockContext ctx;

	MockDataDescriptor o1_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o1"))))
			.WillRepeatedly(testing::ReturnRef(o1_desc));

	MockDataDescriptor o2_desc;
	EXPECT_CALL(ctx, desc(testing::Matcher<const char*>(testing::StrEq("o2"))))
			.WillRepeatedly(testing::ReturnRef(o2_desc));

	for (auto op1 : ALL_VALS<operand1_type>) {
		for (auto op2 : ALL_VALS<operand2_type>) {
			EXPECT_CALL(o1_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op1,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op1)>,
							true,
							false
					)));
			EXPECT_CALL(o2_desc, ref())
					.WillOnce(testing::Return(Ref(
							&op2,
							[] (void*) {},
							Scalar_datatype::type_for_v<decltype(op2)>,
							true,
							false
					)));

			auto const & expr = "${o1} % ${o2}";
			auto const & expected = op1 % op2;
			auto const & result
					= Ref_r(PDI::Expression(expr).to_ref(ctx))
			                  .scalar_value< remove_cv_t<remove_reference_t<decltype(expected)>>>();
			EXPECT_EQ(expected, result) << expr;
		}
	}
}
