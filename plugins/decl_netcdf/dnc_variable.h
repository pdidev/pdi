/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef DECL_NETCDF_DNC_VARIABLE_H_
#define DECL_NETCDF_DNC_VARIABLE_H_

#include <vector>

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

#include "dnc_attribute.h"

namespace decl_netcdf {

/// Represents NetCDF variable. Created from config.
class Dnc_variable
{
	/// Context of this variable
	PDI::Context& m_ctx;
	
	/// Name of this variable
	std::string m_path;
	
	/// Type of this variable. Can be nullptr.
	PDI::Datatype_template_ptr m_type;
	
	/// Dimensions names of this variable
	std::vector<PDI::Expression> m_dimensions_names;
	
	/// Attributes of the variable
	std::vector<Dnc_attribute> m_attributes;
	
public:

	/** Creates NetCDF variable information from yaml
	 *
	 * \param ctx Context of this variable
	 * \param config Configuration node of this variable
	 */
	Dnc_variable(PDI::Context& ctx, const std::string& path, PC_tree_t config);
	
	/** Getter for variable name
	 *
	 * \return name of the variable
	 */
	const std::string& path() const;
	
	/** Setter for variable type
	 *
	 * \param type type of this variable
	 */
	void type(PDI::Datatype_sptr type);
	
	/** Getter for variable type
	 *
	 * \return type of the variable if defined
	 */
	PDI::Datatype_sptr type() const;
	
	/** Getter for variable dimensions
	 *
	 * \return dimensions of the variable
	 */
	std::vector<std::string> dimensions_names() const;
	
	/** Getter for variable attributes
	 *
	 * \return attributes of the variable
	 */
	const std::vector<Dnc_attribute>& attributes() const;
	
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_VARIABLE_H_
