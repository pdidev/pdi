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

#include <pdi/error.h>
#include <pdi/scalar_datatype.h>

#include "ports/payloads/data/input_payload_data.h"

namespace _flowvr_plugin {

void Input_payload_data::load_data_size_desc(PC_tree_t input_port_node)
{
	PC_tree_t size_node = PC_get(input_port_node, ".size");
	if (!PC_status(size_node)) {
		m_data_size_desc = PDI::to_string(size_node);
		m_ctx.logger()->debug("{} port: Payload data size descriptor = {}", m_name, m_data_size_desc);
	}
}

Input_payload_data::Input_payload_data (PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port):
	Payload_data(ctx, name, config, parent_port),
	m_parent_input_port{parent_port}
{
	load_data_size_desc(config);
	if (!m_data_desc.empty()) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->copy_data_to_ref(name, ref);
		}, m_data_desc));
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_empty_desc_access_callback([this](const std::string& name) {
			this->empty_desc_access(name);
		}, m_data_desc));
	}
	m_ctx.logger()->debug("{} port: Created data payload", m_name);
}

Input_payload_data::Input_payload_data(Input_payload_data&& other):
	Payload_data(std::move(other)),
	m_parent_input_port{other.m_parent_input_port},
	m_flowvr_buffer{std::move(other.m_flowvr_buffer)},
	m_data_size_desc{std::move(other.m_data_size_desc)}
{}

Input_payload_data& Input_payload_data::operator=(Input_payload_data&& other)
{
	Payload_data::operator = (std::move(other));
	m_parent_input_port = other.m_parent_input_port;
	m_flowvr_buffer = std::move(other.m_flowvr_buffer);
	m_data_size_desc = std::move(other.m_data_size_desc);
	return *this;
}

void Input_payload_data::copy_data_to_ref(const std::string& data_name, const PDI::Ref_w& ref) const
{
	if (!m_sharing_buffer) { // if buffer is not shared, copy it
		if (ref) {
			if (ref.get() != m_flowvr_buffer.readAccess() && m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
				m_ctx.logger()->debug("{} port: Copy data from FlowVR memory to `{}' descriptor", m_name, data_name);
				if (m_data_selection) {
					m_data_selection->evaluate(m_ctx)->data_from_dense_copy(ref.get(), m_flowvr_buffer.readAccess());
				} else {
					m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->data_from_dense_copy(ref.get(), m_flowvr_buffer.readAccess());
				}
			}
		} else {
			throw PDI::Right_error{"{} port: Cannot get write access to `{}' descriptor", m_name, data_name};
		}
	}
}

void Input_payload_data::empty_desc_access(const std::string& data_name)
{
	if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
		if (m_flowvr_buffer.unique(flowvr::Buffer::ALLSEGMENTS)) {
			m_ctx.logger()->debug("{} port: Sharing (read/write) `{}'", m_name, m_data_desc);
			m_sharing_buffer = true;
			m_ctx[m_data_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess()), true, true);
			m_ctx.logger()->debug("{} port: Share complete (read/write) `{}'", m_name, m_data_desc);
		} else {
			if (m_flowvr_buffer.valid()) {
				m_ctx.logger()->debug("{} port: Share (read only) `{}'", m_name, m_data_desc);
				m_sharing_buffer = true;
				m_ctx[m_data_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess()), true, false);
				m_ctx.logger()->debug("{} port: Share complete (read only) `{}'", m_name, m_data_desc);
			}
		}
	} else {
		m_ctx.logger()->warn("{} port: Flowvr data {} is empty or not valid", m_name, m_data_desc);
		m_ctx.logger()->debug("{} port: Sharing (nullptr) `{}'", m_name, m_data_desc);
		m_sharing_buffer = true;
		m_ctx[m_data_desc].share(PDI::Ref(nullptr, nullptr, PDI::UNDEF_TYPE.clone_type(), true, false), false, false);
		m_ctx.logger()->debug("{} port: Share complete (nullptr) `{}'", m_name, m_data_desc);
	}
}

flowvr::Stamps Input_payload_data::get_message()
{
	if (!m_data_desc.empty()) {
		if (!m_ctx[m_data_desc].empty() && m_sharing_buffer) {
			m_ctx.logger()->debug("{} port: Reclaiming `{}'", m_name, m_data_desc);
			m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
			m_sharing_buffer = false;
			m_ctx.logger()->debug("{} port: Reclaim complete `{}'", m_name, m_data_desc);
		}
		m_flowvr_buffer.clear();
	}
	
	flowvr::Message msg_to_get;
	m_parent_port->getModule()->get(m_parent_input_port, msg_to_get);
	m_ctx.logger()->debug("{} port: Received {} B", m_name, msg_to_get.data.getSize());
	
	if (!m_data_desc.empty()) {
		m_flowvr_buffer = msg_to_get.data;
		
		if (!m_data_size_desc.empty()) {
			//update size in metadata
			long size = m_flowvr_buffer.getSize();
			m_ctx.logger()->debug("{} port: Sharing size descripotr `{}' = {}", m_name, m_data_size_desc, size);
			m_ctx[m_data_size_desc].share(&size, true, false);
			m_ctx[m_data_size_desc].reclaim();
			m_ctx.logger()->debug("{} port: Share complete `{}'", m_name, m_data_size_desc);
		}
	}
	return msg_to_get.stamps;
}

Input_payload_data::~Input_payload_data()
{
	if (!m_ctx[m_data_desc].empty()) {
		if (m_sharing_buffer) {
			m_ctx.logger()->debug("{} port: Reclaiming {}", m_name, m_data_desc);
			m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
			m_sharing_buffer = false;
			m_ctx.logger()->debug("{} port: Reclaim complete {}", m_name, m_data_desc);
		}
	}
}

} // namespace _flowvr_plugin
