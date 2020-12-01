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

#include <spdlog/spdlog.h>

#include "ports/payloads/button_event/payload_button_event.h"
#include "ports/payloads/button_event/input_payload_button_event.h"
#include "ports/payloads/chunk/payload_chunk.h"
#include "ports/payloads/chunk/input_payload_chunk.h"
#include "ports/payloads/data/payload_data.h"
#include "ports/payloads/data/input_payload_data.h"
#include "ports/payloads/mouse_event/payload_mouse_event.h"
#include "ports/payloads/mouse_event/input_payload_mouse_event.h"

#include "ports/input_port.h"

namespace _flowvr_plugin {

bool Input_port::event_port(PC_tree_t config)
{
	PC_tree_t event_port_node = PC_get(config, ".event_port");
	if (!PC_status(event_port_node)) {
		if (PDI::to_string(event_port_node) == "true") {
			return true;
		}
	}
	return false;
}

void Input_port::load_payload(PC_tree_t config)
{
	PC_tree_t node = PC_get(config, ".event_button");
	if (!PC_status(node)) {
		m_payload.reset(new Input_payload_button_event(m_ctx, m_name, config, m_flowvr_input_port));
		return;
	}
	node = PC_get(config, ".event_mouse");
	if (!PC_status(node)) {
		m_payload.reset(new Input_payload_mouse_event(m_ctx, m_name, config, m_flowvr_input_port));
		return;
	}
	node = PC_get(config, ".chunks");
	if (!PC_status(node)) {
		m_payload.reset(new Input_payload_chunk(m_ctx, m_name, config, m_flowvr_input_port));
		return;
	}
	
	m_payload.reset(new Input_payload_data(m_ctx, m_name, config, m_flowvr_input_port));
}

Input_port::Input_port (PDI::Context& ctx, const std::string& name, PC_tree_t config):
	Port{ctx, name, config}
{
	m_flowvr_port.reset(new flowvr::InputPort(m_name, nullptr, false, event_port(config)));
	m_flowvr_input_port = dynamic_cast<flowvr::InputPort*>(m_flowvr_port.get());
	
	load_stamps(config);
	fill_stamp_list();
	
	load_payload(config);
	
	m_ctx.logger()->debug("{} port is an input port with {} stamps", m_name, m_stamps.size());
}

Input_port::Input_port(Input_port&& other):
	Port{std::move(other)},
	m_payload{std::move(other.m_payload)},
	m_flowvr_input_port{other.m_flowvr_input_port}
{
	other.m_flowvr_input_port = nullptr;
}

Input_port& Input_port::operator=(Input_port&& other)
{
	Port::operator = (std::move(other));
	m_payload = std::move(other.m_payload);
	m_flowvr_input_port = other.m_flowvr_input_port;
	other.m_flowvr_input_port = nullptr;
	return *this;
}

void Input_port::get_message()
{
	flowvr::Stamps updated_stamps = m_payload->get_message();
	update_stamps(updated_stamps);
}

Input_port::~Input_port() = default;

} // namespace _flowvr_plugin
