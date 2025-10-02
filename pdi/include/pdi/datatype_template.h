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

#ifndef PDI_DATATYPE_TEMPLATE_H_
#define PDI_DATATYPE_TEMPLATE_H_

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_expression.h>
#include <pdi/expression.h>

namespace PDI {

/** A datatype template. 
 * 
 * A datatype template is something that given arguments that associate values to its parameters, resolves to a
 * datatype.
 * 
 * A datatype template supports two kind of parameters:
 * - data parameters with a type (e.g. integer or array of integers) where the argument associates a data value such 
 *   as 5 to the parameter.
 * - datatype parameters where the argument associates a datatype such as `int` to the parameter
 * 
 * A datatype template can either be a predefined one, such as array, or a user defined one.
 */
class PDI_EXPORT Datatype_template: public std::enable_shared_from_this<Datatype_template>
{
protected:
	std::unordered_set<std::string> m_type_parameters;

	std::unordered_set<std::string> m_value_attributes;

public:
	/** Destroys the template
	 */
	virtual ~Datatype_template() = default;

	const std::unordered_set<std::string>& type_parameters() const;

	const std::unordered_set<std::string> value_attributes() const;

	/** Creates a new datatype by resolving the value of all metadata references
	 *
	 * \param ctx the context in which to evaluate this template
	 * \return the evaluated type that is produced
	 */
	virtual Datatype_sptr evaluate(Context& ctx) const = 0;
};

} // namespace PDI

#endif // PDI_DATATYPE_TEMPLATE_H_
