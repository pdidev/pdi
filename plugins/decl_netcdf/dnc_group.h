/*******************************************************************************
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

#ifndef DECL_NETCDF_DNC_GROUP_H_
#define DECL_NETCDF_DNC_GROUP_H_

#include <vector>

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

#include "dnc_attribute.h"

namespace decl_netcdf {

/// Represents NetCDF group. Created from config.
class Dnc_group
{
	/// Context of this group
	PDI::Context& m_ctx;

	std::string m_path;

	/// Attributes of the group
	std::vector<Dnc_attribute> m_attributes;

public:
	/** Creates a NetCDF group information from yaml
	 *
	 * \param ctx Context of this group
	 * \param config (optional) config with group attributes
	 */
	Dnc_group(PDI::Context& ctx, const std::string& path, PC_tree_t config = {});

	/** Returns variable path
	 *
	 * \return variable path
	 */
	const std::string& path() const;

	/** Returns variable attributes
	 *
	 * \return variable attributes
	 */
	const std::vector<Dnc_attribute>& attributes() const;
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_GROUP_H_
