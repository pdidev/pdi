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

#ifndef PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_
#define PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_

#include <memory>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "../impl.h"

namespace PDI {

/** An expression implemented by a a mapping
 */
struct PDI_NO_EXPORT Expression::Impl::Mapping : public Expression::Impl {
	
	std::unordered_map<std::string, std::unique_ptr<Expression>> m_value;
	
	Mapping(PC_tree_t value);

	Mapping(const std::unordered_map<std::string, std::unique_ptr<Expression>>& value);
	
	std::unique_ptr<Impl> clone() const override;
	
	long to_long(Context& ctx) const override;
	
	double to_double(Context& ctx) const override;
	
	std::string to_string(Context& ctx) const override;

	Ref to_ref(Context& ctx) const override;
	
	Ref to_ref(Context& ctx, const Datatype& type) const override;
	
	size_t copy_value(Context& ctx, void* buffer, const Datatype& type) const override;
};

} // namespace PDI

#endif //PDI_EXPRESSION_IMPL_MAPPING_LITERAL_H_
