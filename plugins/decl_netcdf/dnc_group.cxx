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

#include "dnc_group.h"

namespace decl_netcdf {

Dnc_group::Dnc_group(PDI::Context& ctx, const std::string& path, PC_tree_t config)
	: m_ctx{ctx}
	, m_path{path}
{
	PDI::opt_each(PC_get(config, ".attributes"), [this](PC_tree_t attr_name, PC_tree_t attr_value) {
		this->m_attributes.emplace_back(this->m_ctx, PDI::to_string(attr_name), attr_value);
	});
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
