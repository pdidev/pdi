/*******************************************************************************
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>

#include "ports/payloads/data/payload_data.h"

namespace _flowvr_plugin {

Payload_data::Payload_data(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port):
	m_ctx{ctx},
	m_name{name},
	m_parent_port{parent_port},
	m_sharing_buffer{false}
{
	PC_tree_t data_node = PC_get(config, ".data");
	if (!PC_status(data_node)) {
		m_data_desc = PDI::to_string(data_node);
		m_ctx.logger()->debug("Data Payload ({}): Data descriptor = {}", m_name, m_data_desc);
		PC_tree_t data_selection_node = PC_get(config, ".copy_data_selection");
		if (!PC_status(data_selection_node)) {
			m_data_selection = m_ctx.datatype(data_selection_node);
		}
	}
}

Payload_data::Payload_data(Payload_data&& other):
	m_ctx{other.m_ctx},
	m_parent_port{other.m_parent_port},
	m_name{std::move(other.m_name)},
	m_data_desc{std::move(other.m_data_desc)},
	m_sharing_buffer{other.m_sharing_buffer}
{}

Payload_data& Payload_data::operator=(Payload_data&& other)
{
	m_ctx = other.m_ctx;
	m_parent_port = other.m_parent_port;
	m_name = std::move(other.m_name);
	m_data_desc = std::move(other.m_data_desc);
	m_sharing_buffer = other.m_sharing_buffer;
	return *this;
}

Payload_data::~Payload_data()
{
	for (auto& callback : m_callbacks_remove) {
		callback();
	}
}

} // namespace _flowvr_plugin
