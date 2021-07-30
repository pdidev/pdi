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

#include <pdi/scalar_datatype.h>

#include "ports/payloads/chunk/input_payload_chunk.h"

namespace _flowvr_plugin {

void Input_payload_chunk::load_chunk_size_desc(PC_tree_t chunks_node)
{
	int nb_chunk = PDI::len(chunks_node);
	for (int chunk_id = 0; chunk_id < nb_chunk; chunk_id++) {
		PC_tree_t chunk_node = PC_get(chunks_node, "[%d]", chunk_id);
		std::string chunk_desc = PDI::to_string(PC_get(chunk_node, ".data"));
		std::string chunk_size;
		PC_tree_t size_node = PC_get(chunk_node, ".size");
		if (!PC_status(size_node)) {
			chunk_size = PDI::to_string(size_node);
			m_ctx.logger().debug("{} port: Data size of `{}' = {}", m_name, chunk_desc, chunk_size);
		}
		m_chunk_size_desc.insert({chunk_desc, chunk_size});
	}
}

Input_payload_chunk::Input_payload_chunk(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port):
	Payload_chunk(ctx, name, config, parent_port),
	m_parent_input_port{parent_port}
{
	load_chunk_size_desc(PC_get(config, ".chunks"));
	m_ctx.logger().debug("{} port: Created chunk payload", m_name);
}

Input_payload_chunk::Input_payload_chunk(Input_payload_chunk&& other):
	Payload_chunk(std::move(other)),
	m_parent_input_port{other.m_parent_input_port},
	m_chunk_size_desc{std::move(other.m_chunk_size_desc)}
{}

Input_payload_chunk& Input_payload_chunk::operator=(Input_payload_chunk&& other)
{
	Payload_chunk::operator=(std::move(other));
	m_parent_input_port = other.m_parent_input_port;
	m_chunk_size_desc = std::move(other.m_chunk_size_desc);
	return *this;
}

void Input_payload_chunk::empty_desc_access(const std::string& data_name)
{
	auto chunk_it = std::find(m_chunk_descs.begin(), m_chunk_descs.end(), data_name);
	std::string chunk_desc = *chunk_it;
	int chunk_id = std::distance(m_chunk_descs.begin(), chunk_it);
	
	auto chunk_size_desc_it = m_chunk_size_desc.find(chunk_desc);
	if (chunk_size_desc_it != m_chunk_size_desc.end()) {
		std::string data_size_desc = chunk_size_desc_it->second;
		//update size descriptor if exists
		long size = static_cast<long>(m_chunk_info.chunks_sizes[chunk_id]);
		if (!chunk_size_desc_it->second.empty()) {
			m_ctx.logger().trace("{} port: Sharing size descriptor `{}' = {}", m_name, data_size_desc, size);
			m_ctx[data_size_desc].share(&size, true, false);
			m_ctx[data_size_desc].reclaim();
			m_ctx.logger().trace("{} port: Share size descriptor complete ", m_name);
		}
		
	}
	
	size_t offset = sizeof(size_t) * (1 + m_chunk_info.chunk_count) ; //m_chunk_info
	for (int i = 0; i < chunk_id; i++) {
		offset += m_chunk_info.chunks_sizes[i];
	}
	
	if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
		if (m_flowvr_buffer.unique(flowvr::Buffer::ALLSEGMENTS) ) {
			m_ctx.logger().trace("{} port: Share (read/write) `{}'", m_name, chunk_desc);
			m_sharing_chunks = true;
			m_ctx[chunk_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess() + offset), true, true);
			m_ctx.logger().trace("{} port: Share (read/write) complete `{}'", m_name, chunk_desc);
		} else {
			if (m_flowvr_buffer.valid()) {
				m_ctx.logger().trace("{} port: Share (read only) `{}'", m_name, chunk_desc);
				m_sharing_chunks = true;
				m_ctx[chunk_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess() + offset), true, false);
				m_ctx.logger().trace("{} port: Share (read only) complete `{}'", m_name, chunk_desc);
			}
		}
	} else {
		m_ctx.logger().warn("{} port: Flowvr data `{}' is empty or not valid", m_name, chunk_desc);
		m_ctx.logger().trace("{} port: Sharing (nullptr) `{}'", m_name, chunk_desc);
		m_sharing_chunks = true;
		m_ctx[chunk_desc].share(PDI::Ref(nullptr, nullptr, PDI::UNDEF_TYPE.clone_type(), true, false), false, false);
		m_ctx.logger().trace("{} port: Share (nullptr) complete `{}'", m_name, chunk_desc);
	}
}

flowvr::Stamps Input_payload_chunk::get_message()
{
	for (std::string chunk_desc : m_chunk_descs) {
		if (!chunk_desc.empty()) {
			if (!m_ctx[chunk_desc].empty()) {
				m_ctx.logger().trace("{} port: Reclaiming {}", m_name, chunk_desc);
				m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_chunks = false;
			}
		}
	}
	m_flowvr_buffer.clear();
	
	flowvr::Message msg_to_get;
	m_parent_port->getModule()->get(m_parent_input_port, msg_to_get);
	m_ctx.logger().trace("{} port: Got message chunk", m_name);
	m_flowvr_buffer = msg_to_get.data;
	
	m_chunk_info.chunk_count = *((size_t*)m_flowvr_buffer.readAccess());
	m_chunk_info.chunks_sizes.reset(new size_t[m_chunk_info.chunk_count]);
	for (int i = 0; i < m_chunk_info.chunk_count; i++) {
		m_chunk_info.chunks_sizes[i] = *(((size_t*)m_flowvr_buffer.readAccess()) + (i + 1));
	}
	
	return msg_to_get.stamps;
}

Input_payload_chunk::~Input_payload_chunk()
{
	for (std::string chunk_desc : m_chunk_descs) {
		if (!m_ctx[chunk_desc].empty() && m_sharing_chunks) {
			m_ctx.logger().debug("{} port: Destroing chunk, reclaiming {}", m_name, chunk_desc);
			m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
		}
		m_sharing_chunks = false;
	}
}

} // namespace _flowvr_plugin
