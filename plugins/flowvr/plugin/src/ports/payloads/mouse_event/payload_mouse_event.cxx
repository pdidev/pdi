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

#include "ports/payloads/mouse_event/payload_mouse_event.h"

namespace _flowvr_plugin {

void Payload_mouse_event::load_key_desc(PC_tree_t config)
{
	PC_tree_t keys_node = PC_get(config, ".event_mouse");
	int nb_keys = PDI::len(keys_node, 0);
	for (int key_id = 0; key_id < nb_keys; key_id++) {
		std::string key = PDI::to_string(PC_get(keys_node, "{%d}", key_id));
		std::string desc = PDI::to_string(PC_get(keys_node, "<%d>", key_id));
		
		const auto& name_to_key_it = m_name_to_flowvr_key.find(key);
		if (name_to_key_it == m_name_to_flowvr_key.end()) {
			throw PDI::Config_error{"{} is not valid event mouse KEY", key};
		}
		
		if (key == "POS_XY") {
			m_desc_pos_xy.first = desc;
			m_desc_pos_xy.second.first = name_to_key_it->second;
			m_desc_pos_xy.second.second[0] = 0.0;
			m_desc_pos_xy.second.second[1] = 0.0;
		} else {
			m_desc_value_map.emplace(desc, std::make_pair(name_to_key_it->second, false));
		}
		m_key_desc.emplace(name_to_key_it->second, std::move(desc));
	}
}

Payload_mouse_event::Payload_mouse_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port):
	m_ctx{ctx},
	m_parent_port{parent_port},
	m_name{name}
{
	load_key_desc(config);
}

Payload_mouse_event::Payload_mouse_event(Payload_mouse_event&& other):
	m_ctx{other.m_ctx},
	m_name{std::move(other.m_name)},
	m_parent_port{other.m_parent_port},
	m_key_desc{std::move(other.m_key_desc)},
	m_name_to_flowvr_key{std::move(other.m_name_to_flowvr_key)}
{}

Payload_mouse_event& Payload_mouse_event::operator = (Payload_mouse_event&& other)
{
	m_ctx = other.m_ctx;
	m_name = std::move(other.m_name);
	m_parent_port = other.m_parent_port;
	m_key_desc = std::move(other.m_key_desc);
	m_name_to_flowvr_key = std::move(other.m_name_to_flowvr_key);
	return *this;
}

Payload_mouse_event::~Payload_mouse_event()
{
	for (auto& callback : m_callbacks_remove) {
		callback();
	}
}

} // namespace _flowvr_plugin
