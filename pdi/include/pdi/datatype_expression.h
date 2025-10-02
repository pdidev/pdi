/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_DATATYPE_EXPRESSION_H_
#define PDI_DATATYPE_EXPRESSION_H_

#include <memory>
#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace PDI {

/** A datatype expression that can be evaluated to a datatype
 * 
 * A datatype expression contains a reference to a specific datatype template, plus a set of arguments that associate
 * values to the parameters of the template.
 * 
 * One way to consider that, is that if a datatype template is a function definition, then a datatype expression is a
 * function call
 */
class PDI_EXPORT Datatype_expression
{
private:
	std::string m_referenced_datatype_name;

	std::unordered_map<std::string, Expression> m_value_args;

	std::unordered_map<std::string, Datatype_expression> m_type_args;

public:
	Datatype_expression(PC_tree_t datatype_tree);

	Datatype_expression(
		Datatype_template_sptr referenced,
		std::unordered_map<std::string, Expression> value_args,
		std::unordered_map<std::string, Datatype_expression> type_args
	);

	const std::unordered_map<std::string, Expression>& value_args() const;

	const std::unordered_map<std::string, Datatype_expression>& type_args() const;

	/** Creates a new datatype by resolving the value of all metadata references
	 *
	 * \param ctx the context in which to evaluate this template
	 * \return the evaluated type that is produced
	 */
	Datatype_sptr evaluate(const Context& ctx) const;
};

} // namespace PDI

#endif // PDI_DATATYPE_EXPRESSION_H_
