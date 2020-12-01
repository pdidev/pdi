/*******************************************************************************
 * Copyright (C) 2018-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <spdlog/spdlog.h>

#include "ports/payloads/mouse_event/output_payload_mouse_event.h"

namespace _flowvr_plugin {

Output_payload_mouse_event::Output_payload_mouse_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port):
	Payload_mouse_event{ctx, name, config, parent_port},
	m_flowvr_output_port{parent_port}
{
	m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
		this->data_pos_xy(name, ref);
	}, m_desc_pos_xy.first));
	for (const auto& desc_value : m_desc_value_map) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, desc_value.first));
	}
	m_ctx.logger()->debug("{} port: Created mouse event payload", m_name);
}

Output_payload_mouse_event::Output_payload_mouse_event(Output_payload_mouse_event&& other):
	Payload_mouse_event{std::move(other)},
	m_chunk_event_writer{std::move(other.m_chunk_event_writer)},
	m_flowvr_output_port{other.m_flowvr_output_port}
{}

Output_payload_mouse_event& Output_payload_mouse_event::operator=(Output_payload_mouse_event&& other)
{
	Payload_mouse_event::operator=(std::move(other));
	m_chunk_event_writer = std::move(other.m_chunk_event_writer);
	m_flowvr_output_port = other.m_flowvr_output_port;
	return *this;
}

void Output_payload_mouse_event::data_pos_xy(const std::string& data_name, const PDI::Ref_r& ref)
{
	if (ref) {
		m_ctx.logger()->debug("{} port: Copy mouse position to `{}'", m_name, data_name);
		memcpy(m_desc_pos_xy.second.second, ref.get(), 2 * sizeof(float));
	} else {
		throw PDI::Right_error{"{} port: Cannot get read access to `{}' descriptor", m_name, data_name};
	}
}

void Output_payload_mouse_event::data(const std::string& data_name, const PDI::Ref_r& ref)
{
	if (ref) {
		const auto& desc_value = m_desc_value_map.find(data_name);
		desc_value->second.second = *static_cast<const int*>(ref.get());
		m_ctx.logger()->debug("{} port: Copy mouse button state to `{}'", m_name, data_name);
	} else {
		throw PDI::Right_error{"{} port: Cannot get read access to `{}' descriptor", m_name, data_name};
	}
}

flowvr::Stamps Output_payload_mouse_event::put_message(const flowvr::StampsWrite& stamps)
{
	int mouse_keys = 0;
	
	for (const auto& desc_value: m_desc_value_map) {
		if (desc_value.second.second) { //if pressed
			m_ctx.logger()->debug("{} port: Pressed key with descriptor `{}'", m_name, desc_value.first);
			mouse_keys = mouse_keys | desc_value.second.first;
		}
	}
	
	m_ctx.logger()->debug("{} port: Mouse position [{}, {}]", m_name, m_desc_pos_xy.second.second[0], m_desc_pos_xy.second.second[1]);
	m_chunk_event_writer.addEventMouse(mouse_keys, m_desc_pos_xy.second.second);
	flowvr::Message msg = m_chunk_event_writer.put(m_flowvr_output_port, stamps);
	return msg.stamps;
}

Output_payload_mouse_event::~Output_payload_mouse_event() = default;

} // namespace _flowvr_plugin
