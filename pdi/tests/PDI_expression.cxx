/*******************************************************************************
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

#include <limits>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

#include <pdi/array_datatype.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/ref_any.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include "global_context.h"

#include "mocks/context_mock.h"
#include "mocks/data_descriptor_mock.h"

using PDI::Array_datatype;
using PDI::Expression;
using PDI::Context;
using PDI::Datatype_uptr;
using PDI::Error;
using PDI::Global_context;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::Paraconf_wrapper;
using PDI::Ref;
using PDI::Ref_r;
using std::numeric_limits;
using std::string;
using std::to_string;
using std::pair;
using std::unique_ptr;
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

struct DoubleExpressionTest : public testing::TestWithParam<double> {
	MockContext context_mock;
};

struct StringExpressionTest : public testing::TestWithParam<const char*> {
	MockContext context_mock;
};

struct AdvancedExpressionTest: public testing::Test {
	MockContext context_mock;
};

struct AdvancedDoubleExpressionTest: public testing::Test {
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
 * Name:                LongExpressionTest.to_double
 *
 * Tested functions:    PDI::Expression::to_double(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to double.
 */
TEST_P(LongExpressionTest, to_double)
{
	long param = GetParam();
	Expression exp{param};
	
	ASSERT_EQ(static_cast<double>(param), exp.to_double(context_mock));
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
 * Name:                DoubleExpressionTest.copy
 *
 * Tested functions:    PDI::Expression::Expression(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(DoubleExpressionTest, copy)
{
	double param = GetParam();
	Expression exp{param};
	Expression copy{exp};
	
	ASSERT_EQ(exp.to_double(context_mock), copy.to_double(context_mock));
}

/*
 * Name:                DoubleExpressionTest.copy_operator
 *
 * Tested functions:    PDI::Expression::operator =(const Expression&)
 *
 * Description:         Checks if expression copy is correctly created.
 */
TEST_P(DoubleExpressionTest, copy_operator)
{
	double param = GetParam();
	Expression exp{param};
	Expression copy;
	copy = exp;
	
	ASSERT_EQ(exp.to_double(context_mock), copy.to_double(context_mock));
}

/*
 * Name:                DoubleExpressionTest.move
 *
 * Tested functions:    PDI::Expression::Expression(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(DoubleExpressionTest, move)
{
	double param = GetParam();
	Expression exp{param};
	double result = exp.to_double(context_mock);
	Expression copy{std::move(exp)};
	
	ASSERT_EQ(result, copy.to_double(context_mock));
}

/*
 * Name:                DoubleExpressionTest.move_operator
 *
 * Tested functions:    PDI::Expression::operator =(Expression&&)
 *
 * Description:         Checks if expression move is correctly executed.
 */
TEST_P(DoubleExpressionTest, move_operator)
{
	double param = GetParam();
	Expression exp{param};
	double result = exp.to_double(context_mock);
	Expression copy;
	copy = std::move(exp);
	
	ASSERT_EQ(result, copy.to_double(context_mock));
}

/*
 * Name:                DoubleExpressionTest.bool_operator
 *
 * Tested functions:    PDI::Expression::operator bool()
 *
 * Description:         Checks if non-empty expression evaluates to true.
 */
TEST_P(DoubleExpressionTest, bool_operator)
{
	double param = GetParam();
	Expression exp{param};
	ASSERT_TRUE(exp);
}

/*
 * Name:                DoubleExpressionTest.to_long
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_P(DoubleExpressionTest, to_long)
{
	double param = GetParam();
	Expression exp{param};
	
	ASSERT_EQ(static_cast<long>(param), exp.to_long(context_mock));
}

/*
 * Name:                DoubleExpressionTest.to_double
 *
 * Tested functions:    PDI::Expression::to_double(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to double.
 */
TEST_P(DoubleExpressionTest, to_double)
{
	double param = GetParam();
	Expression exp{param};
	
	ASSERT_DOUBLE_EQ(param, exp.to_double(context_mock));
}

/*
 * Name:                DoubleExpressionTest.to_string
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to string.
 */
TEST_P(DoubleExpressionTest, to_string)
{
	double param = GetParam();
	Expression exp{param};
	ASSERT_DOUBLE_EQ(param, stod(exp.to_string(context_mock)));
}

/*
 * Name:                DoubleExpressionTest.to_ref
 *
 * Tested functions:    PDI::Expression::to_ref(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to ref.
 */
TEST_P(DoubleExpressionTest, to_ref)
{
	double param = GetParam();
	Expression exp{param};
	Ref_r ref {exp.to_ref(context_mock)};
	ASSERT_DOUBLE_EQ(param, *static_cast<const double*>(ref.get()));
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
	} catch (const std::invalid_argument&) {
		ASSERT_THROW(exp.to_long(context_mock), PDI::Error);
	}
}

/*
 * Name:                StringExpressionTest.to_double_cstr
 *
 * Tested functions:    PDI::Expression::to_double(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to double.
 */
TEST_P(StringExpressionTest, to_double_cstr)
{
	const char* param = GetParam();
	Expression exp{param};
	try {
		double res = std::stod(param);
		ASSERT_EQ(res, exp.to_double(context_mock));
	} catch (const std::invalid_argument&) {
		ASSERT_THROW(exp.to_double(context_mock), PDI::Error);
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
	} catch (const std::invalid_argument& ) {
		ASSERT_THROW(exp.to_long(context_mock), PDI::Error);
	}
}

/*
 * Name:                StringExpressionTest.to_double_string
 *
 * Tested functions:    PDI::Expression::to_double(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long.
 */
TEST_P(StringExpressionTest, to_double_string)
{
	string param = GetParam();
	Expression exp{param};
	try {
		double res = std::stod(param);
		ASSERT_EQ(res, exp.to_double(context_mock));
	} catch (const std::invalid_argument& ) {
		ASSERT_THROW(exp.to_double(context_mock), PDI::Error);
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
	} catch (const std::invalid_argument& ) {
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
	} catch (const std::invalid_argument&) {
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
 * Name:                AdvancedExpressionTest.operators
 *
 * Tested functions:    PDI::Expression::to_double(Context&)
 *
 * Description:         Checks if operators are evaluated correctly.
 */
TEST_F(AdvancedDoubleExpressionTest, operators)
{
	//pairs of strings to parse and expected value
	vector<pair<string, double>> checks {
		{"2.1+2.2", 4.3},
		{"4.2-2.2", 2.0},
		{"6.0/2.0", 3.0},
		{"5.0/2.0", 2.5},
		{"4.0=2.0", 0.0},
		{"2.0=2.0", 1.0},
		{"3.0&0.0", 0.0},
		{"4.0&2.0", 1.0},
		{"0.0&12.0", 0.0},
		{"0.0&0.0", 0.0},
		{"-5.0|0.0", 1.0},
		{"-7.0|-9.0", 1.0},
		{"0.0|-1234.0", 1.0},
		{"0.0|0.0", 0.0},
		{"-5.0 > -7.0", 1.0},
		{"-6.0 > -3.0", 0.0},
		{"5.0 > 7.0", 0.0},
		{"6.0 > 3.0", 1.0},
		{"3.0 > 3.0", 0.0},
		{"-5.0 < -7.0", 0.0},
		{"-6.0 < -3.0", 1.0},
		{"5.0 < 7.0", 1.0},
		{"6.0 < 3.0", 0.0},
		{"3.0 < 3.0", 0.0}
	};
	
	for (auto&& entry : checks) {
		Expression exp{entry.first};
		ASSERT_DOUBLE_EQ(entry.second, exp.to_double(context_mock)) << "'" << entry.first << "' should be: " << entry.second << " is: " << exp.to_double(context_mock);
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
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{new long{value}, [](void* p){operator delete(p);}, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("simple")))).WillOnce(ReturnRef(desc_mock));
	Expression exp{"$simple"};
	ASSERT_EQ(value, exp.to_long(context_mock));
}

/*
 * Name:                AdvancedExpressionTest.string_ref
 *
 * Tested functions:    PDI::Expression::to_string(Context&)
 *
 * Description:         Checks if expression reference is correctly evaluated.
 */
TEST_F(AdvancedExpressionTest, string_ref)
{
	MockDataDescriptor desc_mock;
	char string_value[] = "a_string_example";
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{string_value, [](void*) {}, Datatype_uptr{ new Array_datatype{ Datatype_uptr{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}}, sizeof(string_value)-1 }}, true, true }));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("simple")))).WillOnce(ReturnRef(desc_mock));
	Expression exp_str{"$simple"};
	ASSERT_EQ(string_value, exp_str.to_string(context_mock));
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{ string_value, [](void*) {}, Datatype_uptr{ new Array_datatype{ Datatype_uptr{new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)}}, sizeof(string_value)-1 }}, true, true }));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("simple")))).WillOnce(ReturnRef(desc_mock));
	Expression exp_str2{"<${simple}>"};
	ASSERT_EQ("<a_string_example>", exp_str2.to_string(context_mock));
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
	EXPECT_CALL(desc_mock, ref()).WillOnce(Return(Ref_r{new long{value1}, [](void*p){operator delete(p);}, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}))
	.WillOnce(Return(Ref_r{new long{value2}, [](void*p){operator delete(p);}, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(long)}},true, true}));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("value1")))).WillOnce(ReturnRef(desc_mock));
	EXPECT_CALL(context_mock, desc(Matcher<const char*>(StrEq("value2")))).WillOnce(ReturnRef(desc_mock));
	Expression exp{"(${value1} + $value2) * 2"};
	ASSERT_EQ((value1 + value2) * 2, exp.to_long(context_mock));
}

/*
 * Name:                ExpressionTest.parse_reference
 *
 * Tested functions:    PDI::Expression::parse_reference(const char*)
 *
 * Description:         Checks if expression correctly parses a reference expression.
 */
TEST_F(AdvancedExpressionTest, parse_reference)
{
	string ref_expr_str = "${testing}1234";
	auto parse_result = Expression::parse_reference(ref_expr_str.c_str());
	ASSERT_EQ(10L, parse_result.second);
}

INSTANTIATE_TEST_CASE_P(, LongExpressionTest, ::testing::Values(numeric_limits<long>::min(), -1l, 0l, 1l, numeric_limits<long>::max()));

INSTANTIATE_TEST_CASE_P(, DoubleExpressionTest, ::testing::Values(numeric_limits<double>::min(), -1.0, 0.0, 1.0, numeric_limits<double>::max()));

INSTANTIATE_TEST_CASE_P(, StringExpressionTest, ::testing::Values("test_string", "s", "a+b+c","", "-100000", "0", "100000"));

TEST(PCTreeToRefDefault, scalar_value_long)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("4");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	ASSERT_EQ(4, ref.scalar_value<int>());
}

TEST(PCTreeToRefDefault, array_value_long)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("[1, 2, 3, 4]");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	ASSERT_EQ(1, static_cast<long*>(ref.get())[0]);
	ASSERT_EQ(2, static_cast<long*>(ref.get())[1]);
	ASSERT_EQ(3, static_cast<long*>(ref.get())[2]);
	ASSERT_EQ(4, static_cast<long*>(ref.get())[3]);
}

TEST(PCTreeToRefDefault, array_value_string)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("\"abcd\"");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	ASSERT_EQ('a', static_cast<char*>(ref.get())[0]);
	ASSERT_EQ('b', static_cast<char*>(ref.get())[1]);
	ASSERT_EQ('c', static_cast<char*>(ref.get())[2]);
	ASSERT_EQ('d', static_cast<char*>(ref.get())[3]);
}

TEST(PCTreeToRefDefault, array_value_float)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("[1.1, 2.2, 3.3, 4.4]");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	ASSERT_EQ(1.1, static_cast<double*>(ref.get())[0]);
	ASSERT_EQ(2.2, static_cast<double*>(ref.get())[1]);
	ASSERT_EQ(3.3, static_cast<double*>(ref.get())[2]);
	ASSERT_EQ(4.4, static_cast<double*>(ref.get())[3]);
}

TEST(PCTreeToRefDefault, record_value)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("{x: 42, y: [1, 2, 3, 4]}");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	const PDI::Record_datatype* record_type = dynamic_cast<const PDI::Record_datatype*>(&ref.type());
	
	long* x;
	long* y;
	if (record_type->members()[0].name() == "x") {
		// x is the first member
		x = static_cast<long*>(ref.get());
		y = x + 1;
	} else {
		// y is the first member
		y = static_cast<long*>(ref.get());
		x = y + 4;
	}
	ASSERT_EQ(42, *x);
	ASSERT_EQ(1, y[0]);
	ASSERT_EQ(2, y[1]);
	ASSERT_EQ(3, y[2]);
	ASSERT_EQ(4, y[3]);
}

TEST(PCTreeToRefDefault, record_value_string)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PC_tree_t tree = PC_parse_string("{y: 123456789, x: \"a b c d e f g h i j k\"}");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock);
	const PDI::Record_datatype* record_type = dynamic_cast<const PDI::Record_datatype*>(&ref.type());
	
	char* x;
	long* y;
	
	if (record_type->members()[0].name() == "x") {
		// x is the first member
		x = static_cast<char*>(ref.get());
		y = reinterpret_cast<long*>(x + 24);
	} else {
		// y is the first member
		y = static_cast<long*>(ref.get());
		x = reinterpret_cast<char*>(y + 1);
	}
	
	ASSERT_STREQ("a b c d e f g h i j k", x);
	ASSERT_EQ(123456789, *y);
}

TEST(PCTreeToRef, scalar_value_unsigned_char)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Scalar_datatype type {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned char)};
	PC_tree_t tree = PC_parse_string("4");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ(4, *static_cast<unsigned char*>(ref.get()));
}

TEST(PCTreeToRef, scalar_value_short)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Scalar_datatype type {PDI::Scalar_kind::SIGNED, sizeof(short)};
	PC_tree_t tree = PC_parse_string("4");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ(4, *static_cast<short*>(ref.get()));
}

TEST(PCTreeToRef, scalar_value_int)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Scalar_datatype type {PDI::Scalar_kind::SIGNED, sizeof(int)};
	PC_tree_t tree = PC_parse_string("4");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ(4, *static_cast<int*>(ref.get()));
}

TEST(PCTreeToRef, scalar_value_long)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Scalar_datatype type {PDI::Scalar_kind::SIGNED, sizeof(long)};
	PC_tree_t tree = PC_parse_string("4");
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ(4, *static_cast<long*>(ref.get()));
}

TEST(PCTreeToRef, array_value_long)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Array_datatype type {Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::SIGNED, sizeof(long)}}, 4};
	PC_tree_t tree = PC_parse_string("[1, 2, 3, 4]");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	long* array = static_cast<long*>(ref.get());
	ASSERT_EQ(1, array[0]);
	ASSERT_EQ(2, array[1]);
	ASSERT_EQ(3, array[2]);
	ASSERT_EQ(4, array[3]);
}

TEST(PCTreeToRef, array_value_string)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Array_datatype type {Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::UNSIGNED, sizeof(char)}}, 5};
	PC_tree_t tree = PC_parse_string("\"abcd\"");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ('a', static_cast<char*>(ref.get())[0]);
	ASSERT_EQ('b', static_cast<char*>(ref.get())[1]);
	ASSERT_EQ('c', static_cast<char*>(ref.get())[2]);
	ASSERT_EQ('d', static_cast<char*>(ref.get())[3]);
}

TEST(PCTreeToRef, array_value_float)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Array_datatype type {Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::FLOAT, sizeof(double)}}, 4};
	PC_tree_t tree = PC_parse_string("[1.1, 2.2, 3.3, 4.4]");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	ASSERT_EQ(1.1, static_cast<double*>(ref.get())[0]);
	ASSERT_EQ(2.2, static_cast<double*>(ref.get())[1]);
	ASSERT_EQ(3.3, static_cast<double*>(ref.get())[2]);
	ASSERT_EQ(4.4, static_cast<double*>(ref.get())[3]);
}

TEST(PCTreeToRef, record_value)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Record_datatype type {
		std::vector<PDI::Record_datatype::Member> {
			PDI::Record_datatype::Member {0, PDI::Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::UNSIGNED, sizeof(char)}}, "char"},
			PDI::Record_datatype::Member {2, PDI::Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::SIGNED, sizeof(short)}}, "short"},
			PDI::Record_datatype::Member {8, PDI::Datatype_uptr{new PDI::Array_datatype{PDI::Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::SIGNED, sizeof(long)}}, 4}}, "long"}
		},
		40
	};
	PC_tree_t tree = PC_parse_string("{char: 42, short: 84, long: [1, 2, 3, 4]}");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	
	struct Test_record {
		unsigned char x;
		short y;
		long z[4];
	};
	Test_record* result = static_cast<Test_record*>(ref.get());
	ASSERT_EQ(42, result->x);
	ASSERT_EQ(84, result->y);
	ASSERT_EQ(1, result->z[0]);
	ASSERT_EQ(2, result->z[1]);
	ASSERT_EQ(3, result->z[2]);
	ASSERT_EQ(4, result->z[3]);
}

TEST(PCTreeToRef, record_value_string)
{
	PDI::Paraconf_wrapper _pw;
	MockContext ctx_mock;
	
	PDI::Record_datatype type {
		std::vector<PDI::Record_datatype::Member> {
			PDI::Record_datatype::Member {0, PDI::Datatype_uptr{new PDI::Array_datatype{PDI::Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::UNSIGNED, sizeof(char)}}, 25}}, "string"},
			PDI::Record_datatype::Member {32, PDI::Datatype_uptr{new PDI::Scalar_datatype{PDI::Scalar_kind::SIGNED, sizeof(long)}}, "long"}
		},
		40
	};
	PC_tree_t tree = PC_parse_string("{string: \"a b c d e f g h i j k\", long: 123456789}");
	
	PDI::Expression ex(tree);
	PDI::Ref_rw ref = ex.to_ref(ctx_mock, type);
	
	struct Test_record {
		char x[25];
		long y;
	};
	Test_record* result = static_cast<Test_record*>(ref.get());
	ASSERT_STREQ("a b c d e f g h i j k", result->x);
	ASSERT_EQ(123456789, result->y);
}

/*
 * Struct prepared for CallbacksTest.
 */
struct ExpresionOperators : public ::testing::Test {
	ExpresionOperators():
		test_conf{PC_parse_string("data: {x : int, y : int}")}
	{}
	
	void SetUp() override
	{
		test_context.reset(new PDI::Global_context{test_conf});
	}
	
	PDI::Paraconf_wrapper fw;
	PC_tree_t test_conf;
	std::unique_ptr<PDI::Context> test_context;
};

/*
 * Name:                ExpresionOperators.empty_expr
 *
 * Tested functions:    PDI::Expression::operator+
 *
 *
 * Description:         Checks if cannot add empty expression.
 *
 */
TEST_F(ExpresionOperators, empty_expr)
{
	Expression first {"2"};
	Expression second;
	try {
		Expression result = first + second;
		FAIL();
	} catch (const Error& e) {}
}

/*
 * Name:                ExpresionOperators.add_two_expr
 *
 * Tested functions:    PDI::Expression::operator+
 *
 *
 * Description:         Checks if sum of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, add_two_expr)
{
	Expression first {"2"};
	Expression second {"2"};
	Expression result = first + second;
	if (!result) {
		throw;
	}
	ASSERT_EQ(4, result.to_long(*test_context));
}

/*
 * Name:                ExpresionOperators.add_two_ref_expr
 *
 * Tested functions:    PDI::Expression::operator+
 *
 *
 * Description:         Checks if sum of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, add_two_ref_expr)
{
	int x = 42;
	int y = 24;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"$y"};
	Expression result = first + second;
	ASSERT_EQ(66, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.add_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator+
 *
 *
 * Description:         Checks if sum of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, add_tree_ref_expr)
{
	int x = 42;
	int y = 24;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"22"};
	Expression third {"$y"};
	Expression result = first + second + third;
	ASSERT_EQ(88, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.sub_two_expr
 *
 * Tested functions:    PDI::Expression::operator-
 *
 *
 * Description:         Checks if subtraction of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, sub_two_expr)
{
	Expression first {"4"};
	Expression second {"2"};
	Expression result = first - second;
	if (!result) {
		throw;
	}
	ASSERT_EQ(2, result.to_long(*test_context));
}

/*
 * Name:                ExpresionOperators.sub_two_ref_expr
 *
 * Tested functions:    PDI::Expression::operator-
 *
 *
 * Description:         Checks if subtraction of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, sub_two_ref_expr)
{
	int x = 42;
	int y = 40;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"$y"};
	Expression result = first - second;
	ASSERT_EQ(2, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.sub_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator-
 *
 *
 * Description:         Checks if subtraction of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, sub_tree_ref_expr)
{
	int x = 42;
	int y = 20;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"10"};
	Expression third {"$y"};
	Expression result = first - second - third;
	ASSERT_EQ(12, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.multiply_two_expr
 *
 * Tested functions:    PDI::Expression::operator*
 *
 *
 * Description:         Checks if multiplication of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, multiply_two_expr)
{
	Expression first {"2"};
	Expression second {"2"};
	Expression result = first * second;
	ASSERT_EQ(4, result.to_long(*test_context));
}

/*
 * Name:                ExpresionOperators.multiply_two_ref_expr
 *
 * Tested functions:    PDI::Expression::operator*
 *
 *
 * Description:         Checks if multiplication of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, multiply_two_ref_expr)
{
	int x = 4;
	int y = 7;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"$y"};
	Expression result = first * second;
	ASSERT_EQ(28, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.multiply_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator*
 *
 *
 * Description:         Checks if multiplication of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, multiply_tree_ref_expr)
{
	int x = 4;
	int y = 2;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"5"};
	Expression third {"$y"};
	Expression result = first * second * third;
	ASSERT_EQ(40, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.comlex_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator*
 *
 *
 * Description:         Checks if sum and multiplication of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, sum_multiply_tree_ref_expr)
{
	int x = 4;
	int y = 2;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"5"};
	Expression third {"$y"};
	
	Expression result = first * second + third;
	ASSERT_EQ(22, result.to_long(*test_context));

	result = first + second * third;
	ASSERT_EQ(14, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.divide_two_expr
 *
 * Tested functions:    PDI::Expression::operator/
 *
 *
 * Description:         Checks if division of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, divide_two_expr)
{
	Expression first {"4"};
	Expression second {"2"};
	Expression result = first / second;
	ASSERT_EQ(2, result.to_long(*test_context));
}

/*
 * Name:                ExpresionOperators.divide_two_ref_expr
 *
 * Tested functions:    PDI::Expression::operator/
 *
 *
 * Description:         Checks if division of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, divide_two_ref_expr)
{
	int x = 8;
	int y = 4;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"$y"};
	Expression result = first / second;
	ASSERT_EQ(2, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.divide_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator/
 *
 *
 * Description:         Checks if division of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, divide_tree_ref_expr)
{
	int x = 16;
	int y = 2;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"4"};
	Expression third {"$y"};
	Expression result = first / second / third;
	ASSERT_EQ(2, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionOperators.comlex_tree_ref_expr
 *
 * Tested functions:    PDI::Expression::operator/
 *
 *
 * Description:         Checks if sum and multiplication of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, sum_divide_tree_ref_expr)
{
	int x = 16;
	int y = 2;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"4"};
	Expression third {"$y"};
	
	Expression result = first / second + third;
	ASSERT_EQ(6, result.to_long(*test_context));

	result = first + second / third;
	ASSERT_EQ(18, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}


/*
 * Name:                ExpresionOperators.mod_two_ref_expr
 *
 * Tested functions:    PDI::Expression::operator%
 *
 *
 * Description:         Checks if mod of expressions is correct.
 *
 */
TEST_F(ExpresionOperators, mod_two_ref_expr)
{
	int x = 42;
	int y = 10;
	test_context->desc("x").share(&x, true, false);
	test_context->desc("y").share(&y, true, false);

	Expression first {"$x"};
	Expression second {"$y"};
	Expression result = first % second;
	ASSERT_EQ(2, result.to_long(*test_context));

	test_context->desc("x").reclaim();
	test_context->desc("y").reclaim();
}

/*
 * Struct prepared for CallbacksTest.
 */
struct ExpresionFMTFormat : public ::testing::Test {
	ExpresionFMTFormat():
		test_conf{PC_parse_string("data: {x : int, y : double, z: {type: array, subtype: char, size: 16}}")}
	{}
	
	void SetUp() override
	{
		test_context.reset(new PDI::Global_context{test_conf});
	}
	
	PDI::Paraconf_wrapper fw;
	PC_tree_t test_conf;
	std::unique_ptr<PDI::Context> test_context;
};

/*
 * Name:                ExpresionFMTFormat.fmt_int_ref
 *
 * Tested functions:    PDI::Expression::to_string
 *
 *
 * Description:         Checks if fmt operator of expressions works correct.
 *
 */
TEST_F(ExpresionFMTFormat, fmt_int_ref)
{
	int x = 42;
	test_context->desc("x").share(&x, true, false);
	ASSERT_STREQ("00042", Expression{"${x:05d}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("00042", Expression{"$x:05d"}.to_string(*test_context).c_str());
	ASSERT_STREQ("2a", Expression{"${x:x}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("0x00002a", Expression{"${x:#08x}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("52", Expression{"${x:o}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("101010", Expression{"${x:b}"}.to_string(*test_context).c_str());

	test_context->desc("x").reclaim();
}

/*
 * Name:                ExpresionFMTFormat.fmt_float_ref
 *
 * Tested functions:    PDI::Expression::to_string
 *
 *
 * Description:         Checks if fmt operator of expressions works correct.
 *
 */
TEST_F(ExpresionFMTFormat, fmt_float_ref)
{
	double y = 42.424242;
	test_context->desc("y").share(&y, true, false);
	ASSERT_STREQ("42.42424", Expression{"${y:1.5f}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("+42.424242", Expression{"${y:+f}"}.to_string(*test_context).c_str());
	ASSERT_STREQ("+42.424242", Expression{"$y:+f"}.to_string(*test_context).c_str());
	test_context->desc("y").reclaim();
}

/*
 * Name:                ExpresionFMTFormat.fmt_string_ref
 *
 * Tested functions:    PDI::Expression::to_string
 *
 *
 * Description:         Checks if fmt operator of expressions works correct.
 *
 */
TEST_F(ExpresionFMTFormat, fmt_string_ref)
{
	char z[16] = "sometext";
	test_context->desc("z").share(z, true, false);
	ASSERT_STREQ("       sometext", Expression{"${z:>15s}"}.to_string(*test_context).c_str());
	test_context->desc("z").reclaim();
}


/*
 * Name:                ExpresionMemberAccess.access_simple_member
 *
 * Tested functions:    PDI::Expression::to_long
 *                      PDI::Expression::to_double
 *
 *
 * Description:         Checks if mod of expressions is correct.
 *
 */
TEST(ExpresionMemberAccess, access_simple_member)
{
	Paraconf_wrapper fw;
	PC_tree_t config = PC_parse_string(
	"logging: trace                    \n"
	"data:                             \n"
	"    record_data:                  \n"
	"        type: struct              \n"
	"        members:                  \n"
	"            - a: char             \n"
	"            - b: int16            \n"
	"            - c: int              \n"
	"            - d: int64            \n"
	"            - e: float            \n"
	"            - f: double           \n"
	);
	
	struct Test {
		char a;
		short b;
		int c;
		long d;
		float e;
		double f;
	};

	Test test;
	test.a = 1;
	test.b = 12;
	test.c = 123;
	test.d = 1234;
	test.e = 1234.5;
	test.f = 1234.56;

	unique_ptr<Context> ctx {new Global_context{config}};
	
	ctx->desc("record_data").share(&test, true, false);

	ASSERT_EQ(Expression{"${record_data.a}"}.to_long(*ctx), 1L);
	ASSERT_EQ(Expression{"${record_data.b}"}.to_long(*ctx), 12L);
	ASSERT_EQ(Expression{"${record_data.c}"}.to_long(*ctx), 123L);
	ASSERT_EQ(Expression{"${record_data.d}"}.to_long(*ctx), 1234L);
	ASSERT_EQ(Expression{"${record_data.e}"}.to_double(*ctx), 1234.5);
	ASSERT_EQ(Expression{"${record_data.f}"}.to_double(*ctx), 1234.56);

	ctx->desc("record_data").reclaim();
}

/*
 * Name:                ExpresionMemberAccess.access_string_member
 *
 * Tested functions:    PDI::Expression::to_string
 *
 *
 * Description:         Checks if mod of expressions is correct.
 *
 */
TEST(ExpresionMemberAccess, access_string_member)
{
	Paraconf_wrapper fw;
	PC_tree_t config = PC_parse_string(
	"logging: trace                    \n"
	"data:                             \n"
	"    record_data:                  \n"
	"        type: struct              \n"
	"        members:                  \n"
	"            - string:             \n"
	"                  type: array     \n"
	"                  subtype: char   \n"
	"                  size: 32        \n"
	);
	
	struct Test {
		char string[32];
	};

	Test test;
	strcpy(test.string, "abcdefgh");

	unique_ptr<Context> ctx {new Global_context{config}};
	
	ctx->desc("record_data").share(&test, true, false);

	ASSERT_STREQ(Expression{"${record_data.string}"}.to_string(*ctx).c_str(), test.string);

	ctx->desc("record_data").reclaim();
}

/*
 * Name:                ExpresionMemberAccess.access_array_record
 *
 * Tested functions:    PDI::Expression::to_long
 *                      PDI::Expression::to_double
 *
 *
 * Description:         Checks if mod of expressions is correct.
 *
 */
TEST(ExpresionMemberAccess, access_array_record)
{
	Paraconf_wrapper fw;
	PC_tree_t config = PC_parse_string(
	"logging: trace                       \n"
	"data:                                \n"
	"    array_data:                      \n"
	"        type: array                  \n"
	"        size: 32                     \n"
	"        subtype:                     \n"
	"            type: struct             \n"
	"            members:                 \n"
	"                - scalar: char       \n"
	"                - array:             \n"
	"                      type: array    \n"
	"                      subtype: int   \n"
	"                      size: 32       \n"
	);
	
	struct Record {
		char scalar;
		int array[32];
	};

	Record array_record[32];
	for (int i = 0; i < 32; i++) {
		array_record[i].scalar = i;
		for (int j = 0; j < 32; j++) {
			array_record[i].array[j] = i + j;
		}
	}

	unique_ptr<Context> ctx {new Global_context{config}};
	
	ctx->desc("array_data").share(array_record, true, false);

	for (int i = 0; i < 32; i++) {
		ASSERT_EQ(Expression{"${array_data[" + to_string(i) + "].scalar}"}.to_long(*ctx), static_cast<long>(i));
		for (int j = 0; j < 32; j++) {
			ASSERT_EQ(Expression{"${array_data[" + to_string(i) + "].array[" + to_string(j) + "]}"}.to_long(*ctx), static_cast<long>(i + j));
		}
	}

	ctx->desc("array_data").reclaim();
}


/*
 * Name:                ExpresionMemberAccess.access_complex_member
 *
 * Tested functions:    PDI::Expression::to_long
 *                      PDI::Expression::to_double
 *
 *
 * Description:         Checks if mod of expressions is correct.
 *
 */
TEST(ExpresionMemberAccess, access_complex_member)
{
	Paraconf_wrapper fw;
	PC_tree_t config = PC_parse_string(
	"logging: trace                           \n"
	"data:                                    \n"
	"    record_data:                         \n"
	"        type: struct                     \n"
	"        members:                         \n"
	"            - array:                     \n"
	"                type: array              \n"
	"                subtype: int             \n"
	"                size: 32                 \n"
	"            - subrecord:                 \n"
	"                  type: struct           \n"
	"                  members:               \n"
	"                      - scalar: char     \n"
	"                      - array:           \n"
	"                            type: array  \n"
	"                            subtype: int \n"
	"                            size: 32     \n"
	);
	
	struct Subrecord {
		char scalar;
		int array[32];
	};

	struct Record {
		int array[32];
		Subrecord subrecord;
	};

	Record record;
	for (int i = 0; i < 32; i++) {
		record.array[i] = i;
	}
	record.subrecord.scalar = 42;
	for (int i = 0; i < 32; i++) {
		record.subrecord.array[i] = i;
	}
	

	unique_ptr<Context> ctx {new Global_context{config}};
	
	ctx->desc("record_data").share(&record, true, false);

	for (int i = 0; i < 32; i++) {
		ASSERT_EQ(Expression{"${record_data.array[" + to_string(i) + "]}"}.to_long(*ctx), static_cast<long>(i));
	}
	ASSERT_EQ(Expression{"${record_data.subrecord.scalar}"}.to_long(*ctx), 42L);
	for (int i = 0; i < 32; i++) {
		ASSERT_EQ(Expression{"${record_data.subrecord.array[" + to_string(i) + "]}"}.to_long(*ctx), static_cast<long>(i));
	}

	ctx->desc("record_data").reclaim();
}

/*
 * Name:                StringExpressionBoolTest.to_long_boolean_string)
 *
 * Tested functions:    PDI::Expression::to_long(Context&)
 *
 * Description:         Checks if expression is correctly evaluated to long when string represents a boolean.
 */
TEST(StringExpressionBoolTest, to_long_boolean_string)
{
	MockContext context_mock;

	const vector<string> v_pos{"y", "Y", "yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"};
	for (auto&& string_value : v_pos) {
		ASSERT_EQ(PDI::Expression(string_value).to_long(context_mock), 1);
	}
	const vector<string> v_neg{"n", "N", "no", "No", "NO", "false", "False", "FALSE", "Off", "Off", "OFF"};
	for (auto&& string_value : v_neg) {
		ASSERT_EQ(PDI::Expression(string_value).to_long(context_mock), 0);
	}

	const vector<string> v_fpos{"y1", "YY", "yEs", "Yess", "YE5", "tru", "TrUe", "TRRUE", "onn", "0n", "ONN"};
	for (auto&& string_value : v_fpos) {
		try {
			ASSERT_EQ(PDI::Expression(string_value).to_long(context_mock), 1);
			FAIL();
		} catch (PDI::Error& e) {
			// ok
		}
	}

	const vector<string> v_fneg{"n1", "nO", "N0", "Nope", "NOO", "faLse", "Fals", "FaLSE", "OfF", "0f", "FF"};
	for (auto&& string_value : v_fneg) {
		try {
			ASSERT_EQ(PDI::Expression(string_value).to_long(context_mock), 1);
			FAIL();
		} catch (PDI::Error& e) {
			// ok
		}
	}

}
