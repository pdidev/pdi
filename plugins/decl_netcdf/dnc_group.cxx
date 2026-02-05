// SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "dnc_group.h"

namespace decl_netcdf {

Dnc_group::Dnc_group(PDI::Context& ctx, const std::string& path, PC_tree_t config)
	: m_ctx{ctx}
	, m_path{path}
{
	PC_tree_t attributes_node = PC_get(config, ".attributes");
	if (!PC_status(attributes_node)) {
		PDI::each(attributes_node, [this](PC_tree_t attr_name, PC_tree_t attr_value) {
			this->m_attributes.emplace_back(this->m_ctx, PDI::to_string(attr_name), attr_value);
		});
	}
}

const std::string& Dnc_group::path() const
{
	return m_path;
}

const std::vector<Dnc_attribute>& Dnc_group::attributes() const
{
	return m_attributes;
}

}; // namespace decl_netcdf
