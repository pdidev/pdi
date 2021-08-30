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

#ifndef PDI_EXPRESSION_IMPL_H_
#define PDI_EXPRESSION_IMPL_H_

#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "pdi/expression.h"

namespace PDI {

struct PDI_NO_EXPORT Expression::Impl {

	/** An expression implemented by a a integer literal
	 */
	struct Int_literal;
	
	/** An expression implemented by a a float literal
	 */
	struct Float_literal;
	
	/** An expression implemented by a a string literal (with potential dollar refs)
	 */
	struct String_literal;
	
	/** An expression implemented by a an operation
	 */
	struct Operation;
	
	/** An expression implemented by a a reference to a data
	 */
	class Reference_expression;
	
	/** An expression implemented by a a sequence literal
	 */
	struct Sequence;
	
	/** An expression implemented by a a mapping literal
	 */
	struct Mapping;
	
	/** The destructor
	 *
	 */
	virtual ~Impl();
	
	/** Clones this expression implementation
	 * \return a pointer to a copy of this implementation
	 */
	virtual std::unique_ptr<Impl> clone() const = 0;
	
	/** Interprets this expression as an integer value if possible, throws otherwise
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \return the integer value as a long
	 */
	virtual long to_long(Context& ctx) const = 0;
	
	/** Interprets this expression as a floating point value if possible, throws otherwise
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \return the floating point value as a double
	 */
	virtual double to_double(Context& ctx) const = 0;
	
	/** Interprets this expression as a string value if possible, throws otherwise
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \return the string point value as a std::string
	 */
	virtual std::string to_string(Context& ctx) const;
	
	/** Interprets this expression as a reference
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \return the data reference
	 */
	virtual Ref to_ref(Context& ctx) const = 0;
	
	/** Interprets this expression as a reference with a given type
	 *
	 * \param ctx the context in which to evaluate the expression
	 * \param type the type of the created Ref
	 * \return the data reference
	 */
	Ref to_ref(Context& ctx, const Datatype& type) const;
	
	/** Copy value with given type of Expression to buffer
	 *
	 * \param ctx the context in which to evaluate subexpressions
	 * \param buffer the memory where to copy data
	 * \param type type of the data to copy
	 * \return number of copied bytes to buffer
	 */
	virtual size_t copy_value(Context& ctx, void* buffer, const Datatype& type) const = 0;
	
	/** Parse a double value as Impl
	 *
	 * \param value double value to parse
	 * \return unique_ptr with parsed Impl
	 */
	static std::unique_ptr<Impl> parse(double value);
	
	/** Parse a long value as Impl
	 *
	 * \param value long value to parse
	 * \return unique_ptr with parsed Impl
	 */
	static std::unique_ptr<Impl> parse(long value);
	
	/** Parse a PC_tree value as Impl
	 *
	 * \param value PC_tree value to parse
	 * \return unique_ptr with parsed Impl
	 */
	static std::unique_ptr<Impl> parse(PC_tree_t value);
	
	/** Parse a string value as Impl
	 *
	 * \param value string value to parse
	 * \return unique_ptr with parsed Impl
	 */
	static std::unique_ptr<Impl> parse(const char* val_str);
	
	/** Parse a string term as Impl
	 *
	 * \param value string value to parse
	 * \return unique_ptr with parsed Impl depending on string content
	 */
	static std::unique_ptr<Impl> parse_term(char const** val_str);
	
	/** Parse a string as a ID name
	 *
	 * \param value string value to parse
	 * \return string with ID name
	 */
	static std::string parse_id(char const** val_str);
};

} // namespace PDI

#endif // PDI_EXPRESSION_IMPL_H_
