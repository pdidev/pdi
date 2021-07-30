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

#include "ports/payloads/chunk/output_payload_chunk.h"

namespace _flowvr_plugin {

Output_payload_chunk::Output_payload_chunk(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port):
	Payload_chunk(ctx, name, config, parent_port),
	m_parent_output_port{parent_port}
{
	m_ctx.logger().debug("{} port: Created chunk payload", m_name);
}

Output_payload_chunk::Output_payload_chunk(Output_payload_chunk&& other):
	Payload_chunk(std::move(other)),
	m_parent_output_port{other.m_parent_output_port},
	m_flowvr_buffer{std::move(other.m_flowvr_buffer)}
{}

Output_payload_chunk& Output_payload_chunk::operator=(Output_payload_chunk&& other)
{
	Payload_chunk::operator=(std::move(other));
	m_parent_output_port = other.m_parent_output_port;
	m_flowvr_buffer = std::move(other.m_flowvr_buffer);
	return *this;
}

void Output_payload_chunk::empty_desc_access(const std::string& data_name)
{
	if (m_chunk_info.chunk_count == 0) { //if not allocated -> allocate
		m_chunk_info.chunk_count = m_chunk_descs.size();
		m_chunk_info.chunks_sizes.reset(new size_t[m_chunk_info.chunk_count]);
		size_t buffersize = sizeof(size_t) * (1 + m_chunk_info.chunk_count) ; //m_chunk_info
		for (int i = 0; i < m_chunk_info.chunk_count; i++) {
			m_chunk_info.chunks_sizes[i] = m_ctx[m_chunk_descs[i]].default_type()->evaluate(m_ctx)->buffersize();
			buffersize += m_chunk_info.chunks_sizes[i];
		}
		m_flowvr_buffer = m_parent_port->getModule()->alloc(buffersize);
		m_ctx.logger().trace("{} port: Allocated {} B", m_name, buffersize);
		
		//write count and sizes
		flowvr::ubyte* buffer_shr = m_flowvr_buffer.writeAccess();
		memcpy(buffer_shr, &m_chunk_info.chunk_count, sizeof(size_t));
		
		for (int i = 0; i < m_chunk_info.chunk_count; i++) {
			memcpy(buffer_shr + (i+1) * (sizeof(size_t)), &m_chunk_info.chunks_sizes[i], sizeof(size_t));
		}
	}
	
	auto chunk_it = std::find(m_chunk_descs.begin(), m_chunk_descs.end(), data_name);
	int chunk_id = std::distance(m_chunk_descs.begin(), chunk_it);
	size_t offset = sizeof(size_t) * (1 + m_chunk_info.chunk_count) ; //m_chunk_info
	for (int i = 0; i < chunk_id; i++) {
		offset += m_chunk_info.chunks_sizes[i];
	}
	
	m_ctx.logger().trace("{} port: Sharing `{}'", m_name, data_name);
	m_sharing_chunks = true;
	m_ctx[data_name].share(m_flowvr_buffer.writeAccess() + offset, false, true);
	m_ctx.logger().trace("{} port: Share complete `{}'", m_name, data_name);
}

flowvr::Stamps Output_payload_chunk::put_message(const flowvr::StampsWrite& stamps)
{
	flowvr::MessageWrite msg_to_put;
	msg_to_put.stamps = stamps;
	
	if (m_flowvr_buffer.empty()) {
		msg_to_put.data = m_parent_port->getModule()->alloc(0);
	} else {
		msg_to_put.data = m_flowvr_buffer;
	}
	
	m_ctx.logger().trace("{} port: Putting message", m_name);
	m_parent_port->getModule()->put(m_parent_output_port, msg_to_put);
	m_chunk_info.chunk_count = 0;
	
	for (std::string chunk_desc : m_chunk_descs) {
		if (!chunk_desc.empty()) {
			m_ctx.logger().trace("{} port: Reclaiming `{}'", m_name, chunk_desc);
			m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
			m_sharing_chunks = false;
		}
	}
	m_flowvr_buffer.clear();
	return msg_to_put.stamps;
}

Output_payload_chunk::~Output_payload_chunk()
{
	for (std::string chunk_desc : m_chunk_descs) {
		if (!m_ctx[chunk_desc].empty() && m_sharing_chunks) {
			m_ctx.logger().debug("{} port: Destroing payload, reclaiming {}", m_name, chunk_desc);
			m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
		}
		m_sharing_chunks = false;
	}
}

} // namespace _flowvr_plugin
