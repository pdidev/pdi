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

#ifndef PDI_FLOWVR_PAYLOAD_CHUNK
#define PDI_FLOWVR_PAYLOAD_CHUNK

#include <algorithm>
#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>
#include <spdlog/spdlog.h>

#include "payload_interface.h"

namespace  {

struct Chunk_info {
	size_t chunk_count;
	std::unique_ptr<size_t[]> chunks_sizes; // array[chunk_count]
};

class Payload_chunk
{
protected:
	PDI::Context& m_ctx;
	flowvr::Port* m_parent_port;
	
	std::string m_name;
	std::vector<std::string> m_chunk_descs; //name of the descriptors where to read/write data
	Chunk_info m_chunk_info;
	bool m_sharing_chunks;
	
	Payload_chunk(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::Port* parent_port):
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
			m_ctx.add_empty_desc_access_callback([this](const std::string& name) {
				this->empty_desc_access(name);
			}, data_name);
		}
	}
	
	Payload_chunk(const Payload_chunk& other) = delete;
	
	Payload_chunk(Payload_chunk&& other):
		m_ctx{other.m_ctx},
		m_parent_port{other.m_parent_port},
		m_chunk_descs{std::move(other.m_chunk_descs)},
		m_sharing_chunks{other.m_sharing_chunks}
	{}
	
	Payload_chunk& operator = (const Payload_chunk& other) = delete;
	
	Payload_chunk& operator = (Payload_chunk&& other)
	{
		m_ctx = other.m_ctx;
		m_parent_port = other.m_parent_port;
		m_chunk_descs = std::move(other.m_chunk_descs);
		m_sharing_chunks = other.m_sharing_chunks;
		return *this;
	}
	
	virtual void empty_desc_access(const std::string& data_name) = 0;
	
	virtual ~Payload_chunk() = default;
	
}; // Payload_chunk



class Input_payload_chunk : public Input_payload, public Payload_chunk
{
	flowvr::InputPort* m_parent_input_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::Buffer m_flowvr_buffer;
	std::unordered_map<std::string, std::string> m_chunk_size_desc;  // <desc_name, chunk_size>
	
	void load_data_size_desc(PC_tree_t chunks_node)
	{
		int nb_chunk = PDI::len(chunks_node);
		for (int chunk_id = 0; chunk_id < nb_chunk; chunk_id++) {
			PC_tree_t chunk_node = PC_get(chunks_node, "[%d]", chunk_id);
			std::string chunk_desc = PDI::to_string(PC_get(chunk_node, ".data"));
			std::string chunk_size;
			PC_tree_t size_node = PC_get(chunk_node, ".size");
			if (!PC_status(size_node)) {
				chunk_size = PDI::to_string(size_node);
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Data size of `{}' = {}", m_name, chunk_desc, chunk_size);
			}
			m_chunk_size_desc.insert({chunk_desc, chunk_size});
		}
	}
	
public:
	Input_payload_chunk (PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::InputPort* parent_port):
		Payload_chunk(ctx, name, config, parent_port),
		m_parent_input_port{parent_port}
	{
		load_data_size_desc(PC_get(config, ".chunks"));
		m_ctx.logger()->debug("(FlowVR) Intput Chunk Payload ({}) : Created", m_name);
	}
	
	Input_payload_chunk(const Input_payload_chunk& other) = delete;
	
	Input_payload_chunk(Input_payload_chunk&& other):
		Payload_chunk(std::move(other)),
		m_parent_input_port{other.m_parent_input_port},
		m_chunk_size_desc{std::move(other.m_chunk_size_desc)}
	{}
	
	Input_payload_chunk& operator = (const Input_payload_chunk& other) = delete;
	
	Input_payload_chunk& operator = (Input_payload_chunk&& other)
	{
		Payload_chunk::operator = (std::move(other));
		m_parent_input_port = other.m_parent_input_port;
		m_chunk_size_desc = std::move(other.m_chunk_size_desc);
		return *this;
	}
	
	/**
	 *  Share the reference to the flowvr memory to the data_name descriptor
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name) override
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
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Sharing size descriptor `{}' = {}", m_name, data_size_desc, size);
				m_ctx[data_size_desc].share(&size, true, false);
				m_ctx[data_size_desc].reclaim();
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share size descriptor complete ", m_name);
			}
			
		}
		
		size_t offset = sizeof(size_t) * (1 + m_chunk_info.chunk_count) ; //m_chunk_info
		for (int i = 0; i < chunk_id; i++) {
			offset += m_chunk_info.chunks_sizes[i];
		}
		
		if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
			if (m_flowvr_buffer.unique(flowvr::Buffer::ALLSEGMENTS) ) {
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share (read/write) `{}'", m_name, chunk_desc);
				m_sharing_chunks = true;
				m_ctx[chunk_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess() + offset), true, true);
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share (read/write) complete `{}'", m_name, chunk_desc);
			} else {
				if (m_flowvr_buffer.valid()) {
					m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share (read only) `{}'", m_name, chunk_desc);
					m_sharing_chunks = true;
					m_ctx[chunk_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess() + offset), true, false);
					m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share (read only) complete `{}'", m_name, chunk_desc);
				}
			}
		} else {
			m_ctx.logger()->warn("(FlowVR) Input Chunk Payload ({}): Flowvr data `{}' is empty or not valid", m_name, chunk_desc);
			m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Sharing (nullptr) `{}'", m_name, chunk_desc);
			m_sharing_chunks = true;
			m_ctx[chunk_desc].share(PDI::Ref(nullptr, nullptr, PDI::UNDEF_TYPE.clone_type(), true, false), false, false);
			m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Share (nullptr) complete `{}'", m_name, chunk_desc);
		}
	}
	
	/**
	 *  Get message, udpate m_flowvr_buffer
	 *
	 *  \return stamp from flowvr::Message
	 */
	flowvr::Stamps get_message() override
	{
		for (std::string chunk_desc : m_chunk_descs) {
			if (!chunk_desc.empty()) {
				if (!m_ctx[chunk_desc].empty()) {
					m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Reclaiming {}", m_name, chunk_desc);
					m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
					m_sharing_chunks = false;
				}
			}
		}
		m_flowvr_buffer.clear();
		
		flowvr::Message msg_to_get;
		m_parent_port->getModule()->get(m_parent_input_port, msg_to_get);
		m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Got message chunk", m_name);
		m_flowvr_buffer = msg_to_get.data;
		
		m_chunk_info.chunk_count = *((size_t*)m_flowvr_buffer.readAccess());
		m_chunk_info.chunks_sizes.reset(new size_t[m_chunk_info.chunk_count]);
		for (int i = 0; i < m_chunk_info.chunk_count; i++) {
			m_chunk_info.chunks_sizes[i] = *(((size_t*)m_flowvr_buffer.readAccess()) + (i + 1));
		}
		
		return msg_to_get.stamps;
	}
	
	~Input_payload_chunk()
	{
		for (std::string chunk_desc : m_chunk_descs) {
			if (!m_ctx[chunk_desc].empty() && m_sharing_chunks) {
				m_ctx.logger()->debug("(FlowVR) Input Chunk Payload ({}): Destroing chunk, reclaiming {}", m_name, chunk_desc);
				m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_chunks = false;
			}
		}
	}
	
}; // Input_payload_chunk



class Output_payload_chunk : public Output_payload, public Payload_chunk
{
	flowvr::OutputPort* m_parent_output_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::BufferWrite m_flowvr_buffer;
	
public:
	Output_payload_chunk(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::OutputPort* parent_port):
		Payload_chunk(ctx, name, config, parent_port),
		m_parent_output_port{parent_port}
	{
		m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Created", m_name);
	}
	
	Output_payload_chunk(const Output_payload_chunk& other) = delete;
	
	Output_payload_chunk(Output_payload_chunk&& other):
		Payload_chunk(std::move(other)),
		m_parent_output_port{other.m_parent_output_port},
		m_flowvr_buffer{std::move(other.m_flowvr_buffer)}
	{}
	
	Output_payload_chunk& operator = (const Output_payload_chunk& other) = delete;
	
	Output_payload_chunk& operator = (Output_payload_chunk&& other)
	{
		Payload_chunk::operator = (std::move(other));
		m_parent_output_port = other.m_parent_output_port;
		m_flowvr_buffer = std::move(other.m_flowvr_buffer);
		return *this;
	}
	
	/**
	 *  Allocate the flowvr memory and share the reference to it
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name) override
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
			m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Allocated {} B", m_name, buffersize);
			
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
		
		m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Sharing `{}'", m_name, data_name);
		m_sharing_chunks = true;
		m_ctx[data_name].share(m_flowvr_buffer.writeAccess() + offset, false, true);
		m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Share complete `{}'", m_name, data_name);
	}
	
	/**
	 *  Put a message with chunk payload and stamps from argument
	 *
	 *  \param[in] stamps flowvr::Stamps to put to the message
	 *  \return stamp from sent flowvr::Message
	 */
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override
	{
		flowvr::MessageWrite msg_to_put;
		msg_to_put.stamps = stamps;
		
		if (m_flowvr_buffer.empty()) {
			msg_to_put.data = m_parent_port->getModule()->alloc(0);
		} else {
			msg_to_put.data = m_flowvr_buffer;
		}
		
		m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Putting message", m_name);
		m_parent_port->getModule()->put(m_parent_output_port, msg_to_put);
		m_chunk_info.chunk_count = 0;
		
		for (std::string chunk_desc : m_chunk_descs) {
			if (!chunk_desc.empty()) {
				m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Reclaiming `{}'", m_name, chunk_desc);
				m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_chunks = false;
			}
		}
		m_flowvr_buffer.clear();
		return msg_to_put.stamps;
	}
	
	~Output_payload_chunk()
	{
		for (std::string chunk_desc : m_chunk_descs) {
			if (!m_ctx[chunk_desc].empty() && m_sharing_chunks) {
				m_ctx.logger()->debug("(FlowVR) Output Chunk Payload ({}): Destroing payload, reclaiming {}", m_name, chunk_desc);
				m_ctx[chunk_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_chunks = false;
			}
		}
	}
	
}; // class Output_payload_chunk

} // namespace <anonymous>

#endif // PDI_FLOWVR_PAYLOAD_CHUNK