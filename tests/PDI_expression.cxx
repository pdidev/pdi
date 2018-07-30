/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <gtest/gtest.h>
#include <context_mock.h>
#include <data_descriptor_mock.h>

#include <pdi/expression.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

#include <limits>
#include <string>
#include <type_traits>
#include <utility>

using PDI::Expression;
using PDI::Datatype_uptr;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::Ref_r;
using std::numeric_limits;
using std::string;
using std::pair;
using std::vector;

/*
 * Structs prepared for ExpressionTests.
 */
struct EmptyExpressionTest : public testing::Test {
	Expression test_expression;
	MockContext context_mock;
};

struct LongExpressionTest : public testing::TestWithParam<long> {
	MockContext context_mock;
};

struct StringExpressionTest : public testing::TestWithParam<const char*> {
	MockContext context_mock;
};

struct AdvancedExpressionTest: public testing::Test {
	MockContext context_mock;
};

/*
 * Name:                EmptyExpressionTest.copy
 *
 * Tested functions:    PDI::Expression::Expression(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_F(EmptyExpressionTest, copy)
{
	Expression exp;
	Expression copy{exp};
}

/*
 * Name:                EmptyExpressionTest.copy_operator
 *
 * Tested functions:    PDI::Expression::operator =(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_F(EmptyExpressionTest, copy_operator)
{
	Expression exp;
	Expression copy;
	copy = exp;
}

/*
 * Name:                EmptyExpressionTest.move
 *
 * Tested functions:    PDI::Expression::Expression(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_F(EmptyExpressionTest, move)
{
	Expression exp;
	Expression copy{std::move(exp)};
}

/*
 * Name:                EmptyExpressionTest.move_operator
 *
 * Tested functions:    PDI::Expression::operator =(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_F(EmptyExpressionTest, move_operator)
{
	Expression exp;
	Expression copy;
	copy = std::move(exp);
}

/*
 * Name:                EmptyExpressionTest.bool_operator
 *
 * Tested functions:    PDI::Expression::operator bool()
 *
 * Description:         Checks if empty expression evaluates to false.
 */
TEST_F(EmptyExpressionTest, bool_operator)
{
	Expression exp;
	ASSERT_FALSE(exp);
}

/*
 * Name:                EmptyExpressionTest.to_long
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_F(EmptyExpressionTest, to_long)
{
	Expression exp;
	ASSERT_EXIT(exp.to_long(context_mock), ::testing::KilledBySignal(SIGSEGV), ".*");
}

/*
 * Name:                EmptyExpressionTest.to_string
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to string.
 */
TEST_F(EmptyExpressionTest, to_string)
{
	Expression exp;
	ASSERT_EXIT(exp.to_string(context_mock), ::testing::KilledBySignal(SIGSEGV), ".*");
}

/*
 * Name:                EmptyExpressionTest.to_ref
 *
 * Tested functions:    PDI::Expression::to_ref(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to ref.
 */
TEST_F(EmptyExpressionTest, to_ref)
{
	Expression exp;
	ASSERT_EXIT(exp.to_ref(context_mock), ::testing::KilledBySignal(SIGSEGV), ".*");
}

/*
 * Name:                LongExpressionTest.copy
 *
 * Tested functions:    PDI::Expression::Expression(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(LongExpressionTest, copy)
{
	long param = GetParam();
	Expression exp{param};
	Expression copy{exp};
	
	ASSERT_EQ(exp.to_long(context_mock), copy.to_long(context_mock));
}

/*
 * Name:                LongExpressionTest.copy_operator
 *
 * Tested functions:    PDI::Expression::operator =(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(LongExpressionTest, copy_operator)
{
	long param = GetParam();
	Expression exp{param};
	Expression copy;
	copy = exp;
	
	ASSERT_EQ(exp.to_long(context_mock), copy.to_long(context_mock));
}

/*
 * Name:                LongExpressionTest.move
 *
 * Tested functions:    PDI::Expression::Expression(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(LongExpressionTest, move)
{
	long param = GetParam();
	Expression exp{param};
	long result = exp.to_long(context_mock);
	Expression copy{std::move(exp)};
	
	ASSERT_EQ(result, copy.to_long(context_mock));
}

/*
 * Name:                LongExpressionTest.move_operator
 *
 * Tested functions:    PDI::Expression::operator =(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(LongExpressionTest, move_operator)
{
	long param = GetParam();
	Expression exp{param};
	long result = exp.to_long(context_mock);
	Expression copy;
	copy = std::move(exp);
	
	ASSERT_EQ(result, copy.to_long(context_mock));
}

/*
 * Name:                LongExpressionTest.bool_operator
 *
 * Tested functions:    PDI::Expression::operator bool()
 *
 * Description:         Checks if non-empty expression evaluates to true.
 */
TEST_P(LongExpressionTest, bool_operator)
{
	long param = GetParam();
	Expression exp{param};
	ASSERT_TRUE(exp);
}

/*
 * Name:                LongExpressionTest.to_long
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_P(LongExpressionTest, to_long)
{
	long param = GetParam();
	Expression exp{param};
	
	ASSERT_EQ(param, exp.to_long(context_mock));
}

/*
 * Name:                LongExpressionTest.to_string
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to string.
 */
TEST_P(LongExpressionTest, to_string)
{
	long param = GetParam();
	Expression exp{param};
	ASSERT_EQ(std::to_string(param), exp.to_string(context_mock));
}

/*
 * Name:                LongExpressionTest.to_ref
 *
 * Tested functions:    PDI::Expression::to_ref(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to ref.
 */
TEST_P(LongExpressionTest, to_ref)
{
	long param = GetParam();
	Expression exp{param};
	Ref_r ref {exp.to_ref(context_mock)};
	ASSERT_EQ(param, *static_cast<const long*>(ref.get()));
}

/*
 * Name:                StringExpressionTest.copy_cstr
 *
 * Tested functions:    PDI::Expression::Expression(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(StringExpressionTest, copy_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	Expression copy{exp};
	
	ASSERT_EQ(exp.to_string(context_mock), copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.copy_string
 *
 * Tested functions:    PDI::Expression::Expression(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(StringExpressionTest, copy_string)
{
	string param = GetParam();
	Expression exp{param};
	Expression copy{exp};
	
	ASSERT_EQ(exp.to_string(context_mock), copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.copy_operator_cstr
 *
 * Tested functions:    PDI::Expression::operator =(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(StringExpressionTest, copy_operator_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	Expression copy;
	copy = exp;
	
	ASSERT_EQ(exp.to_string(context_mock), copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.copy_operator_string
 *
 * Tested functions:    PDI::Expression::operator =(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(StringExpressionTest, copy_operator_string)
{
	string param = GetParam();
	Expression exp{param};
	Expression copy;
	copy = exp;
	
	ASSERT_EQ(exp.to_string(context_mock), copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.move_cstr
 *
 * Tested functions:    PDI::Expression::Expression(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(StringExpressionTest, move_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	string result = exp.to_string(context_mock);
	Expression copy{std::move(exp)};
	
	ASSERT_EQ(result, copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.move_string
 *
 * Tested functions:    PDI::Expression::Expression(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(StringExpressionTest, move_string)
{
	string param = GetParam();
	Expression exp{param};
	string result = exp.to_string(context_mock);
	Expression copy{std::move(exp)};
	
	ASSERT_EQ(result, copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.move_operator_cstr
 *
 * Tested functions:    PDI::Expression::operator =(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(StringExpressionTest, move_operator_cstr)
{

	const char* param = GetParam();
	Expression exp{param};
	string result = exp.to_string(context_mock);
	Expression copy;
	copy = std::move(exp);
	
	ASSERT_EQ(result, copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.move_operator_string
 *
 * Tested functions:    PDI::Expression::operator =(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(StringExpressionTest, move_operator_string)
{

	string param = GetParam();
	Expression exp{param};
	string result = exp.to_string(context_mock);
	Expression copy;
	copy = std::move(exp);
	
	ASSERT_EQ(result, copy.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.bool_operator_cstr
 *
 * Tested functions:    PDI::Expression::operator bool()
 *
 * Description:         Checks if non-empty expression evaluates to true.
 */
TEST_P(StringExpressionTest, bool_operator_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	ASSERT_TRUE(exp);
}

/*
 * Name:                StringExpressionTest.bool_operator_string
 *
 * Tested functions:    PDI::Expression::operator bool()
 *
 * Description:         Checks if non-empty expression evaluates to true.
 */
TEST_P(StringExpressionTest, bool_operator_string)
{
	string param = GetParam();
	Expression exp{param};
	ASSERT_TRUE(exp);
}

/*
 * Name:                StringExpressionTest.to_long_cstr
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_P(StringExpressionTest, to_long_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	try {
		long res = std::stol(param);
		ASSERT_EQ(res, exp.to_long(context_mock));
	} catch (std::invalid_argument) {
		ASSERT_THROW(exp.to_long(context_mock), PDI::Error);
	}
}

/*
 * Name:                StringExpressionTest.to_long_string
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_P(StringExpressionTest, to_long_string)
{
	string param = GetParam();
	Expression exp{param};
	try {
		long res = std::stol(param);
		ASSERT_EQ(res, exp.to_long(context_mock));
	} catch (std::invalid_argument) {
		ASSERT_THROW(exp.to_long(context_mock), PDI::Error);
	}
}

/*
 * Name:                StringExpressionTest.to_string_cstr
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to string.
 */
TEST_P(StringExpressionTest, to_string_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	ASSERT_STREQ(param, exp.to_string(context_mock).c_str());
}

/*
 * Name:                StringExpressionTest.to_string_string
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to string.
 */
TEST_P(StringExpressionTest, to_string_string)
{
	string param = GetParam();
	Expression exp{param};
	ASSERT_EQ(param, exp.to_string(context_mock));
}

/*
 * Name:                StringExpressionTest.to_ref_cstr
 *
 * Tested functions:    PDI::Expression::to_ref(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to ref.
 */
TEST_P(StringExpressionTest, to_ref_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	Ref_r ref {exp.to_ref(context_mock)};
	try {
		long res = std::stol(param);
		ASSERT_EQ(res, *static_cast<const long*>(ref.get()));
	} catch (std::invalid_argument) {
		ASSERT_STREQ(param, static_cast<const char*>(ref.get()));
	}
}

/*
 * Name:                StringExpressionTest.to_ref_string
 *
 * Tested functions:    PDI::Expression::to_ref(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to ref.
 */
TEST_P(StringExpressionTest, to_ref_string)
{
	string param = GetParam();
	Expression exp{param};
	Ref_r ref {exp.to_ref(context_mock)};
	try {
		long res = std::stol(param);
		ASSERT_EQ(res, *static_cast<const long*>(ref.get()));
	} catch (std::invalid_argument) {
		ASSERT_STREQ(param.c_str(), static_cast<const char*>(ref.get()));
	}
}

/*
 * Name:                AdvancedExpressionTest.operators
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if operators are evaluated correctly.
 */
TEST_F(AdvancedExpressionTest, operators)
{
	//pairs of strings to parse and expected value
	vector<pair<string, long>> checks {
		{"2+2", 4l},
		{"4-1", 3l},
		{"4/2", 2l},
		{"5/2", 2l},
		{"5/2", 2l},
		{"4\%2", 0l},
		{"5\%2", 1l},
		{"4=2", 0l},
		{"2=2", 1l},
		{"3&0", 0l},
		{"4&2", 1l},
		{"0&12", 0l},
		{"0&0", 0l},
		{"-5|0", 1l},
		{"-7|-9", 1l},
		{"0|-1234", 1l},
		{"0|0", 0l},
		{"-5 > -7", 1l},
		{"-6 > -3", 0l},
		{"5 > 7", 0l},
		{"6 > 3", 1l},
		{"3 > 3", 0l},
		{"-5 < -7", 0l},
		{"-6 < -3", 1l},
		{"5 < 7", 1l},
		{"6 < 3", 0l},
		{"3 < 3", 0l}
	};
	
	for (auto&& entry : checks) {
		Expression exp{entry.first};
		ASSERT_EQ(entry.second, exp.to_long(context_mock)) << "'" << entry.first << "' should be: " << entry.second << " is: " << exp.to_long(context_mock);
	}
}

/*
 * Name:                AdvancedExpressionTest.operator_precedence
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated.
 */
TEST_F(AdvancedExpressionTest, operator_precedence)
{
	vector<pair<string, long>> checks {
		{"1+2+3", 6l},
		{"4-2+1", 3l},
		{"4-2-1", 1l},
		{"1+2*3", 7l},
		{"2*3+1", 7l},
		{"1+4/2", 3l},
		{"4/2+1", 3l},
		{"1+4\%2", 1l},
		{"4\%2+1", 1l},
		{"1*2*3", 6l},
		{"4/2/2", 1l},
		{"4-(2+1)", 1l},
		{"4-(2-1)", 3l},
		{"(1+2)*3", 9l},
		{"2*(3+1)", 8l},
		{"(2+4)/2", 3l},
		{"6/(2+1)", 2l},
		{"4/(2/2)", 4l},
		{"1 & 0 & 1", 0l},
		{"1 | 0 | 1", 1l},
		{"1 & 0 | 1", 1l},
		{"0 & 0 | 1", 1l},
		{"0 & (0 | 1)", 0l},
		{"1 | 1 = 0", 1l},
		{"(1 | 1) = 0", 0l},
		{"3 > 2 & 2 > 1", 1l},
		{"1\%3+2*3-4/2=5", 1l}
	};
	
	for (auto&& entry : checks) {
		Expression exp{entry.first};
		ASSERT_EQ(entry.second, exp.to_long(context_mock)) << "'" << entry.first << "' should be: " << entry.second << " is: " << exp.to_long(context_mock);
	}
}

using ::testing::Matcher;
using ::testing::StrEq;
using ::testing::Return;
using ::testing::ReturnRef;

/*
 * Name:                AdvancedExpressionTest.simple_reference
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression reference is correctly evaluated.
 */
TEST_F(AdvancedExpressionTest, simple_reference)
{

	MockDataDescriptor desc_mock;
	long value = 10l;
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{new long{value}, &free, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("simple")))).WillOnce(ReturnRef(desc_mock));
	Expression exp{"$simple"};
	ASSERT_EQ(value, exp.to_long(context_mock));
}

/*
 * Name:                AdvancedExpressionTest.invalid_expressions
 *identifiers
 * Tested functions:    PDI::Expression::Expression(const string&)
 *
 * Description:         Checks if invalid expressions are caught.
 */
TEST_F(AdvancedExpressionTest, invalid_identifiers)
{
	vector<string> invalid_identifiers = {
		"$123",
		"$/something",
		"$^ident",
		"$$double",
		"$",
		"$(id)",
		"${id with spaces}"
	};
	for (auto&& entry : invalid_identifiers) {
		ASSERT_THROW(Expression exp{entry}, PDI::Error) << "'" << entry << "' did not throw an exception";
	}
}

/*
 * Name:                AdvancedExpressionTest.reference_in_operation
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression operation with reference is correctly evaluated.
 */
TEST_F(AdvancedExpressionTest, reference_in_operation)
{
	MockDataDescriptor desc_mock;
	long value1 = 10l;
	long value2 = 20l;
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{new long{value1}, &free, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}))
	.WillOnce(Return(Ref_r{new long{value2}, &free, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("value1")))).WillOnce(ReturnRef(desc_mock));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("value2")))).WillOnce(ReturnRef(desc_mock));
	Expression exp{"(${value1} + $value2) * 2"};
	ASSERT_EQ((value1 + value2) * 2, exp.to_long(context_mock));
}

INSTANTIATE_TEST_CASE_P(, LongExpressionTest, ::testing::Values(numeric_limits<long>::min(), -1l, 0l, 1l, numeric_limits<long>::max()));

INSTANTIATE_TEST_CASE_P(, StringExpressionTest, ::testing::Values("test_string", "s", "a+b+c","", "-100000", "0", "100000"));