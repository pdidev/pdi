/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "config.h"

#include <iomanip>
#include <memory>
#include <sstream>
#include <string>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "expression/impl.h"
#include "expression/impl/float_literal.h"
#include "expression/impl/int_literal.h"
#include "expression/impl/operation.h"
#include "expression/impl/reference_expression.h"

#include "pdi/expression.h"

namespace PDI {

using std::unique_ptr;

Expression::Expression(std::unique_ptr<Impl> impl)
	: m_impl(std::move(impl))
{}

Expression::Expression() = default;

Expression::Expression(const Expression& value)
{
	if (value) {
		m_impl = value.m_impl->clone();
	}
}

Expression::Expression(Expression&& value) = default;

Expression::Expression(const char* val_str)
	: m_impl{Impl::parse(val_str)}
{}

Expression::Expression(const std::string& val_str)
	: Expression{val_str.c_str()}
{}

Expression::Expression(long value)
	: m_impl{new Expression::Impl::Int_literal{value}}
{}

Expression::Expression(double value)
	: m_impl{new Expression::Impl::Float_literal{value}}
{}

Expression::Expression(PC_tree_t value)
	: m_impl{Impl::parse(value)}
{}

Expression::Expression(PC_tree_t value, const Expression& dflt)
	: m_impl{Impl::parse(value, *dflt.m_impl)}
{}

Expression::Expression(PC_tree_t value, Expression&& dflt)
	: m_impl{Impl::parse(value, std::move(dflt.m_impl))}
{}

Expression::~Expression() = default;

Expression& Expression::operator= (const Expression& value)
{
	m_impl.reset(nullptr);
	if (value) {
		m_impl = value.m_impl->clone();
	}
	return *this;
}

Expression& Expression::operator= (Expression&& value) = default;

Expression Expression::operator+ (const Expression& expr) const
{
	return unique_ptr<Expression::Impl>{new Expression::Impl::Operation(*this, Expression::Impl::Operation::Operator::PLUS, expr)};
}

Expression Expression::operator* (const Expression& expr) const
{
	return unique_ptr<Expression::Impl>{new Expression::Impl::Operation(*this, Expression::Impl::Operation::Operator::MULT, expr)};
	;
}

Expression Expression::operator- (const Expression& expr) const
{
	return unique_ptr<Expression::Impl>{new Expression::Impl::Operation(*this, Expression::Impl::Operation::Operator::MINUS, expr)};
	;
}

Expression Expression::operator/ (const Expression& expr) const
{
	return unique_ptr<Expression::Impl>{new Expression::Impl::Operation(*this, Expression::Impl::Operation::Operator::DIV, expr)};
	;
}

Expression Expression::operator% (const Expression& expr) const
{
	return unique_ptr<Expression::Impl>{new Expression::Impl::Operation(*this, Expression::Impl::Operation::Operator::MOD, expr)};
	;
}

long Expression::to_long(Context& ctx) const
{
	return m_impl->to_long(ctx);
}

double Expression::to_double(Context& ctx) const
{
	return m_impl->to_double(ctx);
}

Expression::operator bool () const
{
	return static_cast<bool>(m_impl);
}

std::string Expression::to_string(Context& ctx) const
{
	return m_impl->to_string(ctx);
}

Ref Expression::to_ref(Context& ctx) const
{
	return m_impl->to_ref(ctx);
}

Ref Expression::to_ref(Context& ctx, Datatype_sptr type) const
{
	return m_impl->to_ref(ctx, type);
}

std::pair<Expression, long> Expression::parse_reference(const char* reference_str)
{
	const char* reference_str_to_parse = reference_str;
	unique_ptr<Expression::Impl> reference_impl = Expression::Impl::Reference_expression::parse(&reference_str_to_parse);
	return {std::move(reference_impl), reference_str_to_parse - reference_str};
}

} // namespace PDI
