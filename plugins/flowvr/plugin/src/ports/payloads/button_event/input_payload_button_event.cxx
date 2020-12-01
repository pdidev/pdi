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

#include "ports/payloads/button_event/input_payload_button_event.h"

namespace _flowvr_plugin {

Input_payload_button_event::Input_payload_button_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port):
	Payload_button_event{ctx, name, config, parent_port},
	m_flowvr_input_port{parent_port}
{
	for (const auto& desc_value : m_desc_value_map) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data(name, ref);
		}, desc_value.first));
	}
	m_ctx.logger()->debug("{} port: Created input data payload", m_name);
}

Input_payload_button_event::Input_payload_button_event(Input_payload_button_event&& other):
	Payload_button_event{std::move(other)},
	m_flowvr_input_port{other.m_flowvr_input_port}
{}

Input_payload_button_event& Input_payload_button_event::operator=(Input_payload_button_event&& other)
{
	Payload_button_event::operator = (std::move(other));
	m_flowvr_input_port = other.m_flowvr_input_port;
	return *this;
}

void Input_payload_button_event::data(const std::string& data_name, const PDI::Ref_w& ref) const
{
	if (ref) {
		const auto& desc_value = m_desc_value_map.find(data_name);
		*static_cast<int*>(ref.get()) = desc_value->second.second;
		m_ctx.logger()->trace("{} port: Copy key value = {} to `{}' descriptor", m_name, desc_value->second.second, data_name);
	} else {
		throw PDI::Right_error{"{} port: Cannot get write access to `{}' descriptor", m_name, data_name};
	}
}

flowvr::Stamps Input_payload_button_event::get_message()
{
	flowvr::Message msg_to_get;
	m_parent_port->getModule()->get(m_flowvr_input_port, msg_to_get);
	
	//set all keys as false
	for (auto& desc_value : m_desc_value_map) {
		desc_value.second.second = false;
	}
	
	//set true to pressed keys
	for (ftl::ChunkIterator it = ftl::chunkBegin(msg_to_get); it != ftl::chunkEnd(msg_to_get); it++ ) {
		ftl::ChunkEventButton* chunk_button = (ftl::ChunkEventButton*)((const ftl::Chunk*)it);
		unsigned char key = chunk_button->key;
		if (chunk_button->val) {
			const auto& key_to_desc = m_key_desc.find(key);
			if (key_to_desc == m_key_desc.end()) {
				m_ctx.logger()->warn("{} port: Got unknown `{}' button", m_name, key);
				continue;
			}
			
			// this key was pressed
			m_desc_value_map[key_to_desc->second].second = true;
		}
	}
	return msg_to_get.stamps;
}

Input_payload_button_event::~Input_payload_button_event() = default;

} // namespace _flowvr_plugin
