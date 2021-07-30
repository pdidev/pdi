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

#include "ports/payloads/button_event/output_payload_button_event.h"

namespace _flowvr_plugin {

Output_payload_button_event::Output_payload_button_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port):
	Payload_button_event{ctx, name, config, parent_port},
	m_flowvr_output_port{parent_port}
{
	for (const auto& desc_value : m_desc_value_map) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, desc_value.first));
	}
	m_ctx.logger().debug("{} port: Created output data payload", m_name);
}

Output_payload_button_event::Output_payload_button_event(Output_payload_button_event&& other):
	Payload_button_event{std::move(other)},
	m_chunk_event_writer{std::move(other.m_chunk_event_writer)},
	m_flowvr_output_port{other.m_flowvr_output_port}
{}

Output_payload_button_event& Output_payload_button_event::operator=(Output_payload_button_event&& other)
{
	Payload_button_event::operator=(std::move(other));
	m_chunk_event_writer = std::move(other.m_chunk_event_writer);
	m_flowvr_output_port = other.m_flowvr_output_port;
	return *this;
}

void Output_payload_button_event::data(const std::string& data_name, const PDI::Ref_r& ref)
{
	if (ref) {
		const auto& desc_value = m_desc_value_map.find(data_name);
		desc_value->second.second = *static_cast<const int*>(ref.get());
		m_ctx.logger().trace("{} port: Copied key value = {} from `{}' descriptor", m_name, desc_value->second.second, data_name);
	} else {
		throw PDI::Right_error{"{} port: Cannot get read access to `{}' descriptor", m_name, data_name};
	}
}

flowvr::Stamps Output_payload_button_event::put_message(const flowvr::StampsWrite& stamps)
{
	for (const auto& desc_value_map : m_desc_value_map) {
		if (desc_value_map.second.second) {
			m_chunk_event_writer.addEventButton(desc_value_map.second.first, desc_value_map.second.second);
		}
	}
	
	flowvr::Message msg = m_chunk_event_writer.put(m_flowvr_output_port, stamps);
	return msg.stamps;
}

Output_payload_button_event::~Output_payload_button_event() = default;

} // namespace _flowvr_plugin
