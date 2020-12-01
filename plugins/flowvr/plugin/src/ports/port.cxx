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

#include "ports/port.h"

namespace _flowvr_plugin {

Port::Port(PDI::Context& ctx, const std::string& name, PC_tree_t config):
	m_ctx{ctx},
	m_name{std::move(name)}
{
	PC_tree_t isConnected_node = PC_get(config, ".isConnected");
	if (!PC_status(isConnected_node)) {
		m_remove_callback = m_ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->isConnected(name, ref);
		}, PDI::to_string(isConnected_node));
	}
}

Port::Port(Port&& other):
	m_ctx{other.m_ctx},
	m_name{std::move(other.m_name)},
	m_flowvr_port{std::move(other.m_flowvr_port)},
	m_stamps{std::move(other.m_stamps)}
{}

Port& Port::operator=(Port&& other)
{
	m_ctx = other.m_ctx;
	m_name = std::move(other.m_name);
	m_flowvr_port = std::move(other.m_flowvr_port);
	m_stamps = std::move(other.m_stamps);
	return *this;
}

void Port::load_stamps(PC_tree_t config)
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

void Port::fill_stamp_list()
{
	for (const Stamp& stamp : m_stamps) {
		std::string stamp_name = stamp.get_name();
		if (stamp_name != "it" && stamp_name != "source" && stamp_name != "num") {
			m_flowvr_port->stamps->add(stamp.get_stamp_info());
		}
	}
}

void Port::update_stamps(const flowvr::Stamps& read_stamps)
{
	for (Stamp& stamp : m_stamps) {
		stamp.read_from_flowvr_stamp(read_stamps);
	}
}

void Port::isConnected(const std::string& data_name, PDI::Ref_w ref)
{
	if (ref) {
		*static_cast<int*>(ref.get()) = m_flowvr_port->isConnected();
	} else {
		throw PDI::Right_error{"{} port: Unable to get write permissions for `{}'", m_name, data_name};
	}
}

const std::string& Port::name() const
{
	return m_name;
}

flowvr::Port* Port::get_flowvr_port() const
{
	return m_flowvr_port.get();
}

bool Port::isConnected() const
{
	return m_flowvr_port->isConnected();
}

Port::~Port()
{
	if (m_remove_callback) {
		m_remove_callback();
	}
}

} // namespace _flowvr_plugin
