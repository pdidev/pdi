/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
