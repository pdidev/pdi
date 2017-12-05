/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#ifndef PDI_VALUE_H__
#define PDI_VALUE_H__

#include <memory>

#include <pdi/value_fwd.h>

namespace PDI
{

class Value
{
protected:
	struct Impl {
		virtual ~Impl() {}
		virtual long to_long() const = 0;
		virtual std::string to_string() const;
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
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 */
	Value(const Value &value): m_impl(value.m_impl->clone()) {}
	
	/** Moves a value
	 *
	 * \param[in] value the value to move`
	 */
	Value(Value &&value) = default;
	
	/** Copies a value
	 *
	 * \param[in] value the value to copy
	 * \return *this
	 */
	Value &operator=(const Value &value)
	{
		m_impl = value.m_impl->clone();
		return *this;
	}
	
	/** Moves a value
	 *
	 * \param[in] value the value to move`
	 * \return *this
	 */
	Value &operator=(Value &&value) = default;
	
	/** Evaluates a value as an integer
	 *
	 * \return the integer value
	 */
	long to_long() const
	{
		return m_impl->to_long();
	}
	
	/** Evaluates a value as an integer
	 *
	 * \return the integer value
	 */
	operator long() const
	{
		return to_long();
	}
	
	/** Evaluates a value as a string
	 *
	 * \return the string value
	 */
	std::string to_string() const
	{
		return m_impl->to_string();
	}
	
	/** Evaluates a value as a string
	 *
	 * \return the string value
	 */
	operator std::string() const
	{
		return to_string();
	}
	
	/** Builds (i.e. parse) a value from a string
	 *
	 * \param[in] val_str the string to parse
	 */
	static Value parse(const char *val_str);
	
};

} // namespace PDI

#endif // PDI_VALUE_H__
