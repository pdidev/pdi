/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <memory>
#include <string>

#include <pdi/pdi_fwd.h>
#include <pdi/ref_any.h>

namespace PDI {

class PDI_EXPORT Expression
{
	struct PDI_NO_EXPORT Impl;
	
	friend struct Impl;
	
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
	
	/** Destroys an expression
	 */
	~Expression();
	
	/** Copies an expression
	 *
	 * \param[in] expr the expression to copy
	 * \return *this
	 */
	Expression& operator=(const Expression& expr);
	
	/** Moves an expression
	 *
	 * \param[in] expr the expression to move`
	 * \return *this
	 */
	Expression& operator=(Expression&& expr);
	
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
	 * \return the data reference
	 */
	Ref to_ref(Context& ctx) const;
	
};

} // namespace PDI

#endif // PDI_EXPRESSION_H_