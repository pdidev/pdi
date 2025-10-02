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

#ifndef PDI_DATATYPE_TEMPLATE_ALIAS_H_
#define PDI_DATATYPE_TEMPLATE_ALIAS_H_

#include <memory>
#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>

#include <pdi/datatype_template.h>
#include <pdi/expression.h>

namespace PDI {

using Attributes_map = std::unordered_map<std::string, Expression>;

class PDI_EXPORT Datatype_template_alias: public Datatype_template
{
	Datatype_expression m_expression;

	std::unordered_map<std::string, Expression> m_attributes;

public:
	/** Creates datatype template with given attributes
	 *
	 * \param attributes attributes of datatype template
	 */
	Datatype_template_alias(std::unordered_map<std::string, Expression> attributes = {});

	/** Creates datatype template alias
	 */
	Datatype_template_alias(
		std::unordered_set<std::string> type_arguments,
		std::unordered_set<std::string> value_arguments,
		Datatype_expression expression
	);

	/** Creates datatype template alias
	 */
	Datatype_template_alias(PC_tree_t tree);

	Datatype_sptr evaluate(Context& ctx) const override;
};

} // namespace PDI

#endif // PDI_DATATYPE_TEMPLATE_ALIAS_H_
