/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_EXPRESSION_H_
#define PDI_EXPRESSION_H_

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <pdi/paraconf_wrapper.h>

#include <pdi/pdi_fwd.h>

namespace PDI {

class PDI_EXPORT Expression
{
	struct PDI_NO_EXPORT Impl;

	std::unique_ptr<Impl> m_impl;

	Expression(std::unique_ptr<Impl>);

public:
	/** Builds an empty expression
	 *
	 * No operation can be used on an empty expression, it can only be assigned to
	 */
	Expression();

	/** Copies an expression
	 *
	 * \param[in] expr the expression to copy
	 */
	Expression(const Expression& expr);

	/** Moves an expression
	 *
	 * \param[in] expr the expression to move
	 */
	Expression(Expression&& expr);

	/** Builds (i.e. parse) an expression from a string
	 *
	 * The grammar of an expression is as follow:
	 * \include docs/expression_grammar.in.txt
	 *
	 * \param[in] expr the string to parse
	 */
	Expression(const char* expr);

	/** Builds (i.e. parse) an expression from a string
	 *
	 * The grammar of an expression is as follow:
	 * \include docs/expression_grammar.in.txt
	 *
	 * \param[in] expr the string to parse
	 */
	Expression(const std::string& expr);

	/** Builds an expression that represents an integer
	 *
	 * \param[in] expr the integer value
	 */
	Expression(long expr);

	/** Builds an expression that represents a float
	 *
	 * \param[in] expr the flaot value
	 */
	Expression(double expr);

	/** Builds an expression that is parsed from PC_tree_t
	 *
	 * \param[in] expr the PC_tree_t value
	 */
	Expression(PC_tree_t expr);

	/** Destroys an expression
	 */
	~Expression();

	/** Copies an expression
	 *
	 * \param[in] expr the expression to copy
	 * \return *this
	 */
	Expression& operator= (const Expression& expr);

	/** Moves an expression
	 *
	 * \param[in] expr the expression to move`
	 * \return *this
	 */
	Expression& operator= (Expression&& expr);

	/** Summation operator of an expression
	 *
	 * \param[in] expr the expression to add
	 * \return Expression as a result of sum
	 */
	Expression operator+ (const Expression& expr) const;

	/** Multiplication operator of an expression
	 *
	 * \param[in] expr the expression to multiply
	 * \return Expression as a result of multiplication
	 */
	Expression operator* (const Expression& expr) const;

	/** Subtraction operator of an expression
	 *
	 * \param[in] expr the expression to subtract
	 * \return Expression as a result of subtraction
	 */
	Expression operator- (const Expression& expr) const;

	/** Division operator of an expression
	 *
	 * \param[in] expr the expression to divide
	 * \return Expression as a result of division
	 */
	Expression operator/ (const Expression& expr) const;

	/** Modulo operator of an expression
	 *
	 * \param[in] expr the expression to use modulo
	 * \return Expression as a result of modulo
	 */
	Expression operator% (const Expression& expr) const;

	/** Checks whether this is an empty expression
	 *
	 * \return true if the expression is non-empty
	 */
	operator bool () const;

	/** Evaluates an expression as an integer
	 *
	 * \return the integer value
	 */
	long to_long(Context& ctx) const;

	/** Evaluates an expression as a float
	 *
	 * \return the float value
	 */
	double to_double(Context& ctx) const;

	/** Evaluates an expression as a string
	 *
	 * \return the string value
	 */
	std::string to_string(Context& ctx) const;

	/** Evaluates an expression as a data reference
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \return the data reference
	 */
	Ref to_ref(Context& ctx) const;

	/** Evaluates an expression as a data reference
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \param type the type of the created Ref
	 * \return the data reference
	 */
	Ref to_ref(Context& ctx, Datatype_sptr type) const;

	/** Parses a string that starts with `$` and represents a reference expression
	 *
	 * \param[in] reference_str string that represents a reference expression
	 *
	 * \return resulted reference exrpession and number of bytes read from reference_str
	 */
	static std::pair<Expression, long> parse_reference(const char* reference_str);
};

} // namespace PDI

#endif // PDI_EXPRESSION_H_
