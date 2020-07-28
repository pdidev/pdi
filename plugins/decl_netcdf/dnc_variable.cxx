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

#include "dnc_variable.h"

namespace decl_netcdf {

Dnc_variable::Dnc_variable(PDI::Context& ctx, const std::string& path, PC_tree_t config):
    m_ctx{ctx},
    m_path{path}
{
    PC_tree_t attributes_node = PC_get(config, ".attributes");
    if (!PC_status(attributes_node)) {
        PDI::each(attributes_node, [this](PC_tree_t attr_name, PC_tree_t attr_value) {
            this->m_attributes.emplace_back(this->m_ctx, PDI::to_string(attr_name), attr_value);
        });
    }

    PC_tree_t dimensions_node = PC_get(config, ".dimensions");
    if (!PC_status(dimensions_node)) {
        if (PDI::is_list(dimensions_node)) {
            int len = PDI::len(dimensions_node);
            for (int i = 0; i < len; i++) {
                m_dimensions_names.emplace_back(PDI::to_string(PC_get(dimensions_node, "[%d]", i)));
            }
        }
    }

    PC_tree_t type_node = PC_get(config, ".type");
    if (!PC_status(type_node)) {
        m_type = m_ctx.datatype(config);
    }
}
const std::string& Dnc_variable::path() const
{
    return m_path;
}

void Dnc_variable::type(PDI::Datatype_uptr type)
{
    m_type = std::move(type);
}

PDI::Datatype_uptr Dnc_variable::type() const
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
    for (auto&& dim_name : m_dimensions_names) {
        result.emplace_back(dim_name.to_string(m_ctx));
    }
    return result;
}

const std::vector<Dnc_attribute>& Dnc_variable::attributes() const
{
    return m_attributes;
}


} // namespace decl_netcdf
