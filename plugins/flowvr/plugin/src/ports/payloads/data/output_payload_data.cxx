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

#include "ports/payloads/data/output_payload_data.h"

namespace _flowvr_plugin {

void Output_payload_data::alloc_buffer()
{
	size_t datasize = 0;
	if (m_data_selection) {
		datasize = m_data_selection->evaluate(m_ctx)->datasize();
	} else {
		datasize = m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->datasize();
	}
	m_ctx.logger()->debug("{} port: Allocating {} B", m_name, datasize);
	if (m_flowvr_buffer_pool) {
		m_flowvr_buffer = m_flowvr_buffer_pool->alloc(m_parent_output_port->getModule()->getAllocator(), datasize);
	} else {
		m_flowvr_buffer = m_parent_port->getModule()->alloc(datasize);
	}
}

Output_payload_data::Output_payload_data(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port):
	Payload_data(ctx, name, config, parent_port),
	m_parent_output_port{parent_port}
{
	PC_tree_t data_pool_node = PC_get(config, ".data_size");
	if (!PC_status(data_pool_node)) {
		std::string data_pool = PDI::to_string(data_pool_node);
		if (data_pool == "const") {
			m_flowvr_buffer_pool.reset(new flowvr::BufferPool());
		} else {
			throw PDI::Config_error{data_pool_node, "{} port: `data_size' can be only const", m_name.c_str()};
		}
	}
	
	if (!m_data_desc.empty()) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->copy_data_from_ref(name, ref);
		}, m_data_desc));
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_empty_desc_access_callback([this](const std::string& name) {
			this->empty_desc_access(name);
		}, m_data_desc));
	}
	m_ctx.logger()->debug("{} port: Created data payload", m_name);
}

Output_payload_data::Output_payload_data(Output_payload_data&& other):
	Payload_data(std::move(other)),
	m_parent_output_port{other.m_parent_output_port},
	m_flowvr_buffer{std::move(other.m_flowvr_buffer)},
	m_flowvr_buffer_pool{std::move(other.m_flowvr_buffer_pool)}
{}

Output_payload_data& Output_payload_data::operator=(Output_payload_data&& other)
{
	Payload_data::operator = (std::move(other));
	m_parent_output_port = other.m_parent_output_port;
	m_flowvr_buffer = std::move(other.m_flowvr_buffer);
	m_flowvr_buffer_pool = std::move(other.m_flowvr_buffer_pool);
	return *this;
}

void Output_payload_data::copy_data_from_ref(const std::string& data_name, const PDI::Ref_r& ref)
{
	if (!m_sharing_buffer) { // if buffer is not shared, copy it
		if (ref) {
			if (m_flowvr_buffer.empty()) {
				alloc_buffer();
			}
			if (ref.get() != m_flowvr_buffer.writeAccess()) {
				if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
					m_ctx.logger()->debug("{} port: Copy data from `{}' descriptor to flowvr memory", m_name, data_name);
					if (m_data_selection) {
						m_data_selection->evaluate(m_ctx)->data_to_dense_copy(m_flowvr_buffer.writeAccess(), ref.get());
					} else {
						m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->data_to_dense_copy(m_flowvr_buffer.writeAccess(), ref.get());
					}
				}
			}
		} else {
			throw PDI::Right_error{"{} port: Cannot get read access to `{}' descriptor", m_name, data_name};
		}
	}
}

void Output_payload_data::empty_desc_access(const std::string& data_name)
{
	if (m_flowvr_buffer.empty()) {
		alloc_buffer();
	}
	m_ctx.logger()->debug("{} port: Sharing `{}'", m_name, m_data_desc);
	m_sharing_buffer = true; //must be before share (to not copy data)
	m_ctx[data_name].share(m_flowvr_buffer.writeAccess(), false, true);
	m_ctx.logger()->debug("{} port: Share complete `{}'", m_name, m_data_desc);
}

flowvr::Stamps Output_payload_data::put_message(const flowvr::StampsWrite& stamps)
{
	flowvr::MessageWrite msg_to_put;
	msg_to_put.stamps = stamps;
	
	if (m_data_desc.empty()) {
		msg_to_put.data = m_parent_port->getModule()->alloc(0);
	} else {
		msg_to_put.data = m_flowvr_buffer;
	}
	
	m_ctx.logger()->debug("{} port: Putting message", m_name);
	m_parent_port->getModule()->put(m_parent_output_port, msg_to_put);
	
	if (!m_data_desc.empty()) {
		m_flowvr_buffer.clear();
	}
	if (m_sharing_buffer) {
		m_ctx.logger()->debug("{} port: Reclaiming `{}'", m_name, m_data_desc);
		m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
		m_sharing_buffer = false;
		m_ctx.logger()->debug("{} port: Reclaim complete `{}'", m_name, m_data_desc);
	}
	return msg_to_put.stamps;
}

Output_payload_data::~Output_payload_data()
{
	if (!m_ctx[m_data_desc].empty()) {
		if (m_sharing_buffer) {
			m_ctx.logger()->debug("{} port: Reclaiming `{}'", m_name, m_data_desc);
			m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
			m_sharing_buffer = false;
			m_ctx.logger()->debug("{} port: Reclaim complete `{}'", m_name, m_data_desc);
		}
	}
}

} // namespace _flowvr_plugin
