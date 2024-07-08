/*******************************************************************************
 * Copyright (C) 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "dnc_io.h"

namespace decl_netcdf {

Dnc_io::Dnc_io(PDI::Context& ctx, PC_tree_t config):
	m_ctx{ctx}
{
	PC_tree_t variable_node = PC_get(config, ".variable");
	if (!PC_status(variable_node)) {
		m_variable_path = PDI::Expression{PDI::to_string(variable_node)};
	}
	
	PC_tree_t when_node = PC_get(config, ".when");
	if (!PC_status(when_node)) {
		m_when = PDI::Expression{PDI::to_string(when_node)};
	}
	
	PC_tree_t sizeof_node = PC_get(config, ".size_of");
	if (!PC_status(sizeof_node)) {
		m_sizeof_var = PDI::Expression{PDI::to_string(sizeof_node)};
	}
	
	PC_tree_t var_selection_node = PC_get(config, ".variable_selection");
	if (!PC_status(var_selection_node)) {
		PC_tree_t start_node = PC_get(var_selection_node, ".start");
		if (!PC_status(start_node)) {
			int len = PDI::len(start_node);
			for (int i = 0; i < len; i++) {
				m_start.emplace_back(PDI::to_string(PC_get(start_node, "[%d]", i)));
			}
		}
		
		PC_tree_t subsize_node = PC_get(var_selection_node, ".subsize");
		if (!PC_status(subsize_node)) {
			int len = PDI::len(subsize_node);
			for (int i = 0; i < len; i++) {
				m_subsize.emplace_back(PDI::to_string(PC_get(subsize_node, "[%d]", i)));
			}
		}
	}
	
	
}

std::string Dnc_io::variable_path() const
{
	if (m_variable_path) {
		return m_variable_path.to_string(m_ctx);
	} else {
		return {};
	}
}

std::string Dnc_io::sizeof_variable_path() const
{
	if (m_sizeof_var) {
		return m_sizeof_var.to_string(m_ctx);
	} else {
		return {};
	}
}

bool Dnc_io::when() const
{
	if (m_when) {
		return m_when.to_long(m_ctx);
	} else {
		return true;
	}
	
}

std::vector<size_t> Dnc_io::get_dims_start(const std::vector<size_t>& var_stride) const
{
	std::vector<size_t> var_start;
	if (m_start.empty()) {
		for (auto&& stride : var_stride) {
			var_start.emplace_back(0L);
		}
	} else {
		if (m_start.size() != var_stride.size()) {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Dimensions of stride and start differ: {} != {}", m_start.size(), var_stride.size()};
		}
		for (auto&& start : m_start) {
			var_start.emplace_back(start.to_long(m_ctx));
		}
	}
	return var_start;
}

std::vector<size_t> Dnc_io::get_dims_count(const std::vector<size_t>& var_stride) const
{
	std::vector<size_t> var_count;
	if (m_subsize.empty()) {
		for (auto&& stride : var_stride) {
			var_count.emplace_back(stride);
		}
	} else {
		if (m_subsize.size() != var_stride.size()) {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Dimensions of stride and subsize differ: {} != {}", m_subsize.size(), var_stride.size()};
		}
		for (auto&& subsize : m_subsize) {
			var_count.emplace_back(subsize.to_long(m_ctx));
		}
	}
	return var_count;
}

} // namespace decl_netcdf

