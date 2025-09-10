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

#ifndef PDI_DATATYPE_TEMPLATE_DEFINITION_H_
#define PDI_DATATYPE_TEMPLATE_DEFINITION_H_

#include <memory>
#include <string>
#include <unordered_map>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>

namespace PDI {

class PDI_EXPORT Datatype_template_definition
{
	std::string m_name; // maybe not necessary

	std::vector<std::string> m_parameters;

	// each parameter represents a type
	// m_parameters is the list of the name of the parameters that each of them represents a type
	Datatype_template_sptr m_content;

public:
	/** Creates datatype template definition
	 *
	 * \param attributes attributes of datatype template
	 */
	Datatype_template_definition(const Attributes_map& attributes = {});
};

} // namespace PDI

#endif //PDI_DATATYPE_TEMPLATE_DEFINITION_H_
