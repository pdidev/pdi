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

#ifndef PDI_FLOWVR_PORT
#define PDI_FLOWVR_PORT

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
#include "stamps/stamp.h"

namespace  {

class Port
{
protected:
	PDI::Context& m_ctx;
	
	std::string m_name;             // name of the port
	std::string m_connected_desc;   // isConnected descriptor name
	
	std::unique_ptr<flowvr::Port> m_flowvr_port;
	std::vector<Stamp> m_stamps;    // stamps signed to this port (every message is stamped with this stamps)
	
	/**
	 *  Load descriptor `isConnected' name
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_connected_desc(PC_tree_t config)
	{
		PC_tree_t isConnected_node = PC_get(config, ".isConnected");
		if (!PC_status(isConnected_node)) {
			m_connected_desc = PDI::to_string(isConnected_node);
		}
	}
	
	Port(PDI::Context& ctx, std::string name, PC_tree_t config):
		m_ctx{ctx},
		m_name{std::move(name)}
	{
		load_connected_desc(config);
	}
	
	Port(const Port& other) = delete;
	
	Port(Port&& other):
		m_ctx{other.m_ctx},
		m_name{std::move(other.m_name)},
		m_connected_desc{std::move(other.m_connected_desc)},
		m_flowvr_port{std::move(other.m_flowvr_port)},
		m_stamps{std::move(other.m_stamps)}
	{}
	
	Port& operator = (const Port& other) = delete;
	
	Port& operator = (Port&& other)
	{
		m_ctx = other.m_ctx;
		m_name = std::move(other.m_name);
		m_connected_desc = std::move(other.m_connected_desc);
		m_flowvr_port = std::move(other.m_flowvr_port);
		m_stamps = std::move(other.m_stamps);
		return *this;
	}
	
	
	/**
	 *  Reads configuration and creates proper stamps.
	 *  Can be called only after m_flowvr_port initialization.
	 *
	 *  \param[in] write true if read/write stamp, false if read only stamp
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_stamps(PC_tree_t config)
	{
		PC_tree_t stamps_node = PC_get(config, ".stamps");
		if (!PC_status(stamps_node)) {
			int nb_stamps = PDI::len(stamps_node, 0);
			for (int stamp_id = 0; stamp_id < nb_stamps; stamp_id++) {
				std::string stamp_name = PDI::to_string(PC_get(stamps_node, "{%d}", stamp_id));
				PC_tree_t stamp_node = PC_get(stamps_node, "<%d>", stamp_id);
				m_stamps.emplace_back(m_ctx, m_flowvr_port.get(), std::move(stamp_name), std::move(stamp_node));
			}
		}
	}
	
	/**
	 *  Adds stamps to flowvr::Port stamp list
	 */
	void fill_stamp_list()
	{
		for (const Stamp& stamp : m_stamps) {
			std::string stamp_name = stamp.get_name();
			if (stamp_name != "it" && stamp_name != "source" && stamp_name != "num") {
				m_flowvr_port->stamps->add(stamp.get_stamp_info());
			}
		}
	}
	
	/**
	 *  Updates Port stamps from received stamp
	 *
	 *  \param[in] read_stamps received stamps from flowvr::Message
	 */
	void set_stamps(const flowvr::Stamps& read_stamps)
	{
		for (Stamp& stamp : m_stamps) {
			stamp.read_from_flowvr_stamp(read_stamps);
		}
	}
	
public:
	/**
	 *  \param[out] flowvr::Port pointer signed to this Port
	 */
	flowvr::Port* get_flowvr_port() const
	{
		return m_flowvr_port.get();
	}
	
	virtual ~Port() = default;
	
}; // class Port

} // namespace <anonymous>

#endif // PDI_FLOWVR_PORT