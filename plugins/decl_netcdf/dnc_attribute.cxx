// SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "dnc_attribute.h"

namespace decl_netcdf {

Dnc_attribute::Dnc_attribute(PDI::Context& ctx, const std::string& name, PC_tree_t config)
	: m_ctx{ctx}
	, m_name{name}
{
	m_value = PDI::Expression{PDI::to_string(config)};
	m_ctx.logger().trace("`{}' attribute created", name);
}

const std::string& Dnc_attribute::name() const
{
	return m_name;
}

PDI::Ref Dnc_attribute::value() const
{
	return m_value.to_ref(m_ctx);
}

} // namespace decl_netcdf
