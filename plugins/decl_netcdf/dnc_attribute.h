/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef DECL_NETCDF_DNC_ATTRIBUTE_H_
#define DECL_NETCDF_DNC_ATTRIBUTE_H_

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

namespace decl_netcdf {

/// Represents NetCDF attribute. Created from config.
class Dnc_attribute
{
	/// Context of this attribute
	PDI::Context& m_ctx;

	/// Name of this attribute
	std::string m_name;

	/// Value of this attribute
	PDI::Expression m_value;

public:
	/** Creates NetCDF attribute information from yaml
	 *
	 * \param ctx Context of this I/O
	 * \param name Name of this attribute
	 * \param config Configuration node of this attribute
	 */
	Dnc_attribute(PDI::Context& ctx, const std::string& name, PC_tree_t config);

	/** Getter for attribute name
	 *
	 * \return name of the attribute
	 */
	const std::string& name() const;

	/** Getter for value
	 *
	 * \return Ref created from value
	 */
	PDI::Ref value() const;
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_ATTRIBUTE_H_
