/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_FLOWVR_INPUT_PORT
#define PDI_FLOWVR_INPUT_PORT

#include <functional>
#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>
#include <spdlog/spdlog.h>

#include "payloads/payload_button_event.h"
#include "payloads/payload_chunk.h"
#include "payloads/payload_data.h"
#include "payloads/payload_interface.h"
#include "payloads/payload_mouse_event.h"
#include "port.h"

namespace  {

class Input_port : public Port
{
	std::unique_ptr<Input_payload> m_payload; // payload handler
	
	flowvr::InputPort* m_flowvr_input_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
	/**
	 *  Reads configuration and return if this is event Port
	 *
	 *  \param[in] config the configuration to read (port root)
	 *  \return  true if event port, false otherwise
	 */
	bool event_port(PC_tree_t config)
	{
		PC_tree_t event_port_node = PC_get(config, ".event_port");
		if (!PC_status(event_port_node)) {
			if (PDI::to_string(event_port_node) == "true") {
				return true;
			}
		}
		return false;
	}
	
	/**
	 *  Reads configuration and load proper payload handler
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_payload(PC_tree_t config)
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
	
public:
	Input_port (PDI::Context& ctx, std::string name, PC_tree_t config):
		Port{ctx, name, config}
	{
		m_flowvr_port.reset(new flowvr::InputPort(m_name, nullptr, false, event_port(config))); //flowvr destroys stamps
		m_flowvr_input_port = dynamic_cast<flowvr::InputPort*>(m_flowvr_port.get());
		
		load_stamps(config);
		fill_stamp_list();
		
		load_payload(config);
		
		m_ctx.logger()->debug("(FlowVR) Input Port ({}): Created input port with {} stamps", m_name, m_stamps.size());
	}
	
	Input_port(const Input_port& other) = delete;
	
	Input_port(Input_port&& other):
		Port{std::move(other)},
		m_payload{std::move(other.m_payload)},
		m_flowvr_input_port{other.m_flowvr_input_port}
	{}
	
	Input_port& operator = (const Input_port& other) = delete;
	
	Input_port& operator = (Input_port&& other)
	{
		Port::operator = (std::move(other));
		m_payload = std::move(other.m_payload);
		m_flowvr_input_port = other.m_flowvr_input_port;
		return *this;
	}
	
	/**
	 *  Gets stamps from payload::get_message and updates stamps
	 */
	void get_message()
	{
		flowvr::Stamps updated_stamps = m_payload->get_message();
		set_stamps(updated_stamps);
	}
	
	~Input_port() = default;
	
}; // Input_port

} // namespace <anonymous>

#endif // PDI_FLOWVR_PORT