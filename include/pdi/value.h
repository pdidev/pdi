/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_VALUE_H_
#define PDI_VALUE_H_

#include <memory>

#include <pdi/fwd.h>
#include <pdi/data_reference.h>

namespace PDI {

class PDI_EXPORT Value
{
protected:
	struct Impl
	{
		virtual ~Impl() {}
		virtual long to_long(Context&) const = 0;
		virtual std::string to_string(Context&) const;
		virtual Data_ref to_ref(Context&) const = 0;
		virtual std::unique_ptr<Impl> clone() const = 0;
	};
	
	std::unique_ptr<Impl> m_impl;
	
	Value(std::unique_ptr<Impl> impl): m_impl(move(impl)) {}
	
	static Value make_value(std::unique_ptr<Impl> impl)
	{
		return Value{std::move(impl)};
	}
	
public:
	/** Builds an empty value
	 *
	 * No operation can be used on an empty value
	 */
	Value() = default;
	
	/** Builds (i.e. parse) a value from a string
	 *
	 * The grammar of an expression is as follow:
	 * ```
	 * VALUE   := INTVAL | STRVAL
	 * STRVAL  := ( CHAR | '\' '\' | '\' '$' | REF | '$' '(' INTVAL ')' )*
	 * INTVAL  := INTVAL2 ( '|' INTVAL2 )*
	 * INTVAL2 := INTVAL3 ( '&' INTVAL3 )*
	 * INTVAL3 := INTVAL4 ( '=' INTVAL4 )*
	 * INTVAL4 := INTVAL5 ( ( '+' | '-' ) INTVAL5 )*
	 * INTVAL5 := TERM ( ( '*' | '/' | '%' ) TERM )*
	 * TERM    := ( CONST | REF | '(' INTVAL ')' )
	 * REF     := '$' ( IREF | '{' IREF '}' )
	 * IREF    := ID ( '[' INTVAL ']' )*
	 * CONST ~= (0x)? [0-9]+ ( \.  )
	 * ID    ~= [a-zA-Z_][a-zA-Z0-9_]*
	 * CHAR  ~= [^$\\]
	 * ```
	 *
	 * \param[in] value the string to parse
	 */
	Value(const char* val_str);
	
	/** Builds (i.e. parse) a value from a string
	 *
	 * The grammar of an expression is as follow:
	 * ```
	 * VALUE   := INTVAL | STRVAL
	 * STRVAL  := ( CHAR | '\' '\' | '\' '$' | REF | '$' '(' INTVAL ')' )*
	 * INTVAL  := INTVAL2 ( '|' INTVAL2 )*
	 * INTVAL2 := INTVAL3 ( '&' INTVAL3 )*
	 * INTVAL3 := INTVAL4 ( '=' INTVAL4 )*
	 * INTVAL4 := INTVAL5 ( ( '+' | '-' ) INTVAL5 )*
	 * INTVAL5 := TERM ( ( '*' | '/' | '%' ) TERM )*
	 * TERM    := ( CONST | REF | '(' INTVAL ')' )
	 * REF     := '$' ( IREF | '{' IREF '}' )
	 * IREF    := ID ( '[' INTVAL ']' )*
	 * CONST ~= (0x)? [0-9]+ ( \.  )
	 * ID    ~= [a-zA-Z_][a-zA-Z0-9_]*
	 * CHAR  ~= [^$\\]
	 * ```
	 *
	 * \param[in] value the string to parse
	 */
	Value(const std::string& value);
	
	/** Builds a value that represents an integer
	 *
	 * \param[in] value the integer value
	 */
	Value(int value);
	
	/** Builds a value that represents an integer
	 *
	 * \param[in] value the integer value
	 */
	Value(unsigned value);
	
	/** Builds a value that represents an integer
	 *
	 * \param[in] value the integer value
	 */
	Value(long value);
	
	/** Builds a value that represents an integer
	 *
	 * \param[in] value the integer value
	 */
	Value(unsigned long value);
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 */
	Value(const Value& value): m_impl(value.m_impl->clone()) {}
	
	/** Moves a value
	 *
	 * \param[in] value the value to move`
	 */
	Value(Value&& value) = default;
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 * \return *this
	 */
	Value& operator=(const Value& value)
	{
		m_impl = value.m_impl->clone();
		return *this;
	}
	
	/** Moves a value
	 *
	 * \param[in] value the value to move`
	 * \return *this
	 */
	Value& operator=(Value&& value) = default;
	
	/** Evaluates a value as an integer
	 *
	 * \return the integer value
	 */
	long to_long(Context& ctx) const
	{
		return m_impl->to_long(ctx);
	}
	
	/** Evaluates a value as a string
	 *
	 * \return the string value
	 */
	std::string to_string(Context& ctx) const
	{
		return m_impl->to_string(ctx);
	}
	
	/** Evaluates a value as a data reference
	 *
	 * \return the data reference
	 */
	Data_ref to_ref(Context& ctx) const
	{
		return m_impl->to_ref(ctx);
	}
	
};

} // namespace PDI

#endif // PDI_VALUE_H_
