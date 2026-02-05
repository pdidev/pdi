/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
	PDI::Datatype_template_sptr m_type;

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
