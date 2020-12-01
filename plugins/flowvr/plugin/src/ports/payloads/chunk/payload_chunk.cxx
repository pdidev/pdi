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

#include "ports/payloads/chunk/payload_chunk.h"

namespace _flowvr_plugin {

Chunk_info::Chunk_info():
	chunk_count{0},
	chunks_sizes{nullptr}
{}

Payload_chunk::Payload_chunk(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port):
	m_ctx{ctx},
	m_name{name},
	m_parent_port{parent_port},
	m_sharing_chunks{false}
{
	PC_tree_t chunks_node = PC_get(config, ".chunks");
	int nb_chunk = PDI::len(chunks_node);
	for (int chunk_id = 0; chunk_id < nb_chunk; chunk_id++) {
		PC_tree_t chunk_node = PC_get(chunks_node, "[%d]", chunk_id);
		
		m_chunk_descs.emplace_back(PDI::to_string(PC_get(chunk_node, ".data")));
	}
	for (const std::string& data_name : m_chunk_descs) {
		m_callbacks_remove.emplace_back(m_ctx.callbacks().add_empty_desc_access_callback([this](const std::string& name) {
			this->empty_desc_access(name);
		}, data_name));
	}
}

Payload_chunk::Payload_chunk(Payload_chunk&& other):
	m_ctx{other.m_ctx},
	m_parent_port{other.m_parent_port},
	m_chunk_descs{std::move(other.m_chunk_descs)},
	m_sharing_chunks{other.m_sharing_chunks},
	m_callbacks_remove{std::move(other.m_callbacks_remove)}
{
	other.m_parent_port = nullptr;
	other.m_sharing_chunks = false;
}

Payload_chunk& Payload_chunk::operator=(Payload_chunk&& other)
{
	m_ctx = other.m_ctx;
	m_parent_port = other.m_parent_port;
	other.m_parent_port = nullptr;
	m_chunk_descs = std::move(other.m_chunk_descs);
	m_sharing_chunks = other.m_sharing_chunks;
	other.m_sharing_chunks = false;
	m_callbacks_remove = std::move(other.m_callbacks_remove);
	return *this;
}

Payload_chunk::~Payload_chunk()
{
	for (auto& callback : m_callbacks_remove) {
		callback();
	}
}

} // namespace _flowvr_plugin
