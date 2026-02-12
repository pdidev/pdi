/*******************************************************************************
 * Copyright (C) 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "dnc_variable.h"

#include <pdi/paraconf_wrapper.h>

namespace decl_netcdf {

Dnc_variable::Dnc_variable(PDI::Context& ctx, const std::string& path, PC_tree_t config, PDI::Expression deflate)
	: m_chunking{PC_get(config, ".chunking")}
	, m_ctx{ctx}
	, m_deflate{PC_get(config, ".deflate"), std::move(deflate)}
	, m_path{path}
{
	PDI::opt_each(PC_get(config, ".attributes"), [this](PC_tree_t attr_name, PC_tree_t attr_value) {
		this->m_attributes.emplace_back(this->m_ctx, PDI::to_string(attr_name), attr_value);
	});

	PDI::opt_one_or_each(PC_get(config, ".dimensions"), [&](PC_tree_t dimensions_node){
		m_dimensions_names.emplace_back(PDI::to_string(dimensions_node));
	});

	PDI::opt_one(PC_get(config, ".type"), [&](PC_tree_t){
		m_type = m_ctx.datatype(config);
	});
}

const std::string& Dnc_variable::path() const
{
	return m_path;
}

void Dnc_variable::type(PDI::Datatype_sptr type)
{
	m_type = std::move(type);
}

PDI::Datatype_sptr Dnc_variable::type() const
{
	if (m_type) {
		return m_type->evaluate(m_ctx);
	} else {
		return nullptr;
	}
}

std::vector<std::string> Dnc_variable::dimensions_names() const
{
	std::vector<std::string> result;
	for (auto&& dim_name: m_dimensions_names) {
		result.emplace_back(dim_name.to_string(m_ctx));
	}
	return result;
}

const std::vector<Dnc_attribute>& Dnc_variable::attributes() const
{
	return m_attributes;
}

const PDI::Expression& Dnc_variable::deflate() const
{
	return m_deflate;
}

const PDI::Expression& Dnc_variable::chunking() const
{
	return m_chunking;
}

} // namespace decl_netcdf
