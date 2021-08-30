/*******************************************************************************
 * Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_EXPRESSION_IMPL_STRING_LITERAL_H_
#define PDI_EXPRESSION_IMPL_STRING_LITERAL_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a a string literal (with potential dollar refs)
 */
struct PDI_NO_EXPORT Expression::Impl::String_literal : public Expression::Impl {

	/** A Subvalue contains another value to insert and the string following it
	 */
	using Subvalue = std::pair<Expression, std::string>;
	
	/// a char string containing the beginning str_value
	std::string m_start;
	
	/// array of subvalues
	std::vector<Subvalue> m_values;
	
	std::unique_ptr<Impl> clone() const override;
	
	long to_long(Context& ctx) const override;
	
	double to_double(Context& ctx) const override;
	
	std::string to_string(Context& ctx) const override;
	
	Ref to_ref(Context& ctx) const override;
	
	size_t copy_value(Context& ctx, void* buffer, const Datatype& type) const override;
	
	static std::unique_ptr<Impl> parse(char const** val_str);
	
};


} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_STRING_LITERAL_H_
