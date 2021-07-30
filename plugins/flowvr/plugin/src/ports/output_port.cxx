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

#include "ports/payloads/button_event/payload_button_event.h"
#include "ports/payloads/button_event/output_payload_button_event.h"
#include "ports/payloads/chunk/payload_chunk.h"
#include "ports/payloads/chunk/output_payload_chunk.h"
#include "ports/payloads/data/payload_data.h"
#include "ports/payloads/data/output_payload_data.h"
#include "ports/payloads/mouse_event/payload_mouse_event.h"
#include "ports/payloads/mouse_event/output_payload_mouse_event.h"

#include "ports/output_port.h"

namespace _flowvr_plugin {

void Output_port::init_stamps_to_put()
{
	for (const Stamp& stamp : m_stamps) {
		std::string stamp_name = stamp.get_name();
		if (stamp_name != "it" && stamp_name != "source" && stamp_name != "num") {
			m_stamps_to_put.emplace_back(stamp);
		}
	}
}

void Output_port::load_payload(PC_tree_t config)
{
	PC_tree_t node = PC_get(config, ".event_button");
	if (!PC_status(node)) {
		m_payload.reset(new Output_payload_button_event(m_ctx, m_name, config, m_flowvr_output_port));
		return;
	}
	node = PC_get(config, ".event_mouse");
	if (!PC_status(node)) {
		m_payload.reset(new Output_payload_mouse_event(m_ctx, m_name, config, m_flowvr_output_port));
		return;
	}
	node = PC_get(config, ".chunks");
	if (!PC_status(node)) {
		m_payload.reset(new Output_payload_chunk(m_ctx, m_name, config, m_flowvr_output_port));
		return;
	}
	
	m_payload.reset(new Output_payload_data(m_ctx, m_name, config, m_flowvr_output_port));
}

Output_port::Output_port(PDI::Context& ctx, std::string name, PC_tree_t config):
	Port{ctx, name, config}
{
	m_flowvr_port.reset(new flowvr::OutputPort(m_name));
	m_flowvr_output_port = dynamic_cast<flowvr::OutputPort*>(m_flowvr_port.get());
	
	load_stamps(config);
	fill_stamp_list();
	init_stamps_to_put();
	
	load_payload(config);
	
	m_ctx.logger().debug("{} port is an output port with {} stamps", m_name, m_stamps.size());
}


Output_port::Output_port(Output_port&& other):
	Port(std::move(other)),
	m_payload{std::move(other.m_payload)},
	m_stamps_to_put{std::move(other.m_stamps_to_put)},
	m_flowvr_output_port{other.m_flowvr_output_port}
{
	other.m_flowvr_output_port = nullptr;
}

Output_port& Output_port::operator=(Output_port&& other)
{
	Port::operator = (std::move(other));
	m_payload = std::move(other.m_payload);
	m_stamps_to_put = std::move(other.m_stamps_to_put);
	m_flowvr_output_port = other.m_flowvr_output_port;
	other.m_flowvr_output_port = nullptr;
	return *this;
}

void Output_port::put_message()
{
	flowvr::StampsWrite stamps_write;
	for (const Stamp& stamp : m_stamps_to_put) {
		stamp.write_to_flowvr_stamp(stamps_write);
	}
	flowvr::Stamps sent_stamps = m_payload->put_message(std::move(stamps_write));
	update_stamps(sent_stamps);
}

Output_port::~Output_port() = default;

} // namespace _flowvr_plugin
