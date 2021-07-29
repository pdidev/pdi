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

#include <ftl/chunkevents.h>

#include <pdi/error.h>

#include "ports/payloads/mouse_event/input_payload_mouse_event.h"

namespace _flowvr_plugin {

Input_payload_mouse_event::Input_payload_mouse_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port):
	Payload_mouse_event{ctx, name, config, parent_port},
	m_flowvr_input_port{parent_port}
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

Input_payload_mouse_event::Input_payload_mouse_event(Input_payload_mouse_event&& other):
	Payload_mouse_event{std::move(other)},
	m_flowvr_input_port{other.m_flowvr_input_port}
{}

Input_payload_mouse_event& Input_payload_mouse_event::operator=(Input_payload_mouse_event&& other)
{
	Payload_mouse_event::operator=(std::move(other));
	m_flowvr_input_port = other.m_flowvr_input_port;
	return *this;
}

void Input_payload_mouse_event::data_pos_xy(const std::string& data_name, const PDI::Ref_w& ref) const
{
	if (ref) {
		m_ctx.logger()->trace("{} port: Copy mouse position to `{}'", m_name, data_name);
		memcpy(ref.get(), m_desc_pos_xy.second.second, 2 * sizeof(float));
	} else {
		throw PDI::Right_error{"{} port: Cannot get write access to `{}' descriptor", m_name, data_name};
	}
}

void Input_payload_mouse_event::data(const std::string& data_name, const PDI::Ref_w& ref) const
{
	if (ref) {
		const auto& desc_value = m_desc_value_map.find(data_name);
		*static_cast<int*>(ref.get()) = desc_value->second.second;
		m_ctx.logger()->trace("{} port: Copy mouse button state to `{}'", m_name, data_name);
	} else {
		throw PDI::Right_error{"{} port: Cannot get write access to `{}' descriptor", m_name, data_name};
	}
}

flowvr::Stamps Input_payload_mouse_event::get_message()
{
	flowvr::Message msg_to_get;
	m_parent_port->getModule()->get(m_flowvr_input_port, msg_to_get);
	
	//set all keys as false
	for (auto& desc_value : m_desc_value_map) {
		desc_value.second.second = false;
	}
	
	//set true to pressed keys
	for (ftl::ChunkIterator it = ftl::chunkBegin(msg_to_get); it != ftl::chunkEnd(msg_to_get); it++) {
		ftl::ChunkEventMouse* chunk_mouse = (ftl::ChunkEventMouse*)((const ftl::Chunk*)it);
		
		int key_left = chunk_mouse->mouseKeys & 0x01; //0x0001
		int key_middle = chunk_mouse->mouseKeys & 0x02; //0x0010
		int key_right = chunk_mouse->mouseKeys & 0x04; //0x0100
		
		const auto& left_it = m_key_desc.find(0x01);
		if (left_it != m_key_desc.end()) {
			m_desc_value_map[left_it->second].second = static_cast<bool>(key_left);
			m_ctx.logger()->trace("{} port: Received left mouse button state = {}", m_name, static_cast<bool>(key_left));
		}
		
		const auto& middle_it = m_key_desc.find(0x02);
		if (middle_it != m_key_desc.end()) {
			m_desc_value_map[middle_it->second].second = static_cast<bool>(key_middle);
			m_ctx.logger()->trace("{} port: Received middle mouse button state = {}", m_name, static_cast<bool>(key_middle));
		}
		
		const auto& right_it = m_key_desc.find(0x04);
		if (right_it != m_key_desc.end()) {
			m_desc_value_map[right_it->second].second = static_cast<bool>(key_right);
			m_ctx.logger()->trace("{} port: Received right mouse button state = {}", m_name, static_cast<bool>(key_right));
		}
		
		const auto& mouse_it = m_key_desc.find(0x08);
		if (mouse_it != m_key_desc.end()) {
			m_desc_pos_xy.second.second[0] = chunk_mouse->mouseTranslation[0];
			m_desc_pos_xy.second.second[1] = chunk_mouse->mouseTranslation[1];
			m_ctx.logger()->trace("{} port: Received mouse position = [{}, {}]", m_name, m_desc_pos_xy.second.second[0], m_desc_pos_xy.second.second[1]);
		}
	}
	return msg_to_get.stamps;
}

Input_payload_mouse_event::~Input_payload_mouse_event() = default;

} // namespace _flowvr_plugin
