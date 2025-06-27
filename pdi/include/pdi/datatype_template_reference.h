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

#ifndef PDI_DATATYPE_TEMPLATE_REFERENCE_H_
#define PDI_DATATYPE_TEMPLATE_REFERENCE_H_

#include <memory>
#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>
#include <pdi/expression.h>

namespace PDI {

class PDI_EXPORT Datatype_template_reference: public Datatype_template
{
	Datatype_template_definition_sptr m_definition;

	std::unordered_map<std::string, Datatype_template_sptr> m_arguments;

public:
	/** Creates datatype template reference
	 *
	 * \param attributes attributes of datatype template
	 */
	Datatype_template_reference(const Attributes_map& attributes = {});

	Datatype_sptr evaluate(Context& ctx) const override;
};

} // namespace PDI

#endif //PDI_DATATYPE_TEMPLATE_REFERENCE_H_
