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

#ifndef PDI_FLOWVR_OUTPUT_PORT
#define PDI_FLOWVR_OUTPUT_PORT

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

class Output_port : public Port
{
	std::unique_ptr<Output_payload> m_payload; // payload handler
	flowvr::OutputPort* m_flowvr_output_port;  // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
	std::vector<std::reference_wrapper<const Stamp>> m_stamps_to_put; //all stamps exluding: it, num, source
	
	/**
	 *  Fills stamps to put vector (all stamps exluding: it, num, source)
	 */
	void init_stamps_to_put()
	{
		for (const Stamp& stamp : m_stamps) {
			std::string stamp_name = stamp.get_name();
			if (stamp_name != "it" && stamp_name != "source" && stamp_name != "num") {
				m_stamps_to_put.emplace_back(stamp);
			}
		}
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
	
public:
	Output_port (PDI::Context& ctx, std::string name, PC_tree_t config):
		Port{ctx, name, config}
	{
		m_flowvr_port.reset(new flowvr::OutputPort(m_name, nullptr)); //flowvr destroys stamps
		m_flowvr_output_port = dynamic_cast<flowvr::OutputPort*>(m_flowvr_port.get());
		
		load_stamps(config);
		fill_stamp_list();
		init_stamps_to_put();
		
		load_payload(config);
		
		m_ctx.logger()->debug("(FlowVR) Output Port ({}): Created output port with {} stamps", m_name, m_stamps.size());
	}
	
	Output_port(const Output_port& other) = delete;
	
	Output_port(Output_port&& other):
		Port(std::move(other)),
		m_payload{std::move(other.m_payload)},
		m_flowvr_output_port{other.m_flowvr_output_port},
		m_stamps_to_put{other.m_stamps_to_put}
	{}
	
	Output_port& operator = (const Output_port& other) = delete;
	
	Output_port& operator = (Output_port&& other)
	{
		Port::operator = (std::move(other));
		m_payload = std::move(other.m_payload);
		m_flowvr_output_port = other.m_flowvr_output_port;
		m_stamps_to_put = other.m_stamps_to_put;
		return *this;
	}
	
	/**
	 *  Called if user accessing data descriptor. Pass it to payload.
	 *
	 *  \param[in] data_name descriptor name
	 */
	bool data(const char* data_name, const PDI::Ref& ref)
	{
	
		for (Stamp& stamp: m_stamps) {
			if (stamp.data(data_name, ref)) {
				return true;
			}
		}
		if (m_payload->data(data_name, ref)) {
			return true;
		}
		if (m_connected_desc == data_name) {
			PDI::Ref_w ref_w{ref};
			if (ref_w) {
				*static_cast<int*>(ref_w.get()) = m_flowvr_port->isConnected();
			} else {
				throw PDI::Error {PDI_ERR_RIGHT, "(FlowVR) Port ({}): Unable to get write permissions for `{}'", m_name, m_connected_desc};
			}
			return true;
		}
		return false;
	}
	
	/**
	 *  Called if user accessing empty descriptor. Pass it to payload.
	 *
	 *  \param[in] data_name empty descriptor name
	 */
	void share(const char* data_name) const
	{
		m_payload->share(data_name);
	}
	
	/**
	 *  Prepares stamps to put, calls payload::put_message with this stamps
	 *  ande update stamps from sent message (sometimes it, num sent needed)
	 */
	void put_message()
	{
		flowvr::StampsWrite stamps_write;
		for (const Stamp& stamp : m_stamps_to_put) {
			stamp.write_to_flowvr_stamp(stamps_write);
		}
		flowvr::Stamps sent_stamps = m_payload->put_message(std::move(stamps_write));
		set_stamps(sent_stamps);
	}
	
	~Output_port() = default;
	
}; // class Output_port

} // namespace <anonymous>

#endif // PDI_FLOWVR_PORT