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

#ifndef PDI_FLOWVR_PAYLOAD_DATA
#define PDI_FLOWVR_PAYLOAD_DATA

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <spdlog/spdlog.h>

#include "payload_interface.h"

namespace  {

class Payload_data
{
protected:
	PDI::Context& m_ctx;
	flowvr::Port* m_parent_port;
	
	std::string m_name;
	std::string m_data_desc; //name of the descriptor where to read/write data
	bool m_sharing_buffer; // true if plugin is sharing buffer
	PDI::Datatype_template_uptr m_data_selection; //type for data_selection to copy
	
	Payload_data(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::Port* parent_port):
		m_ctx{ctx},
		m_name{name},
		m_parent_port{parent_port},
		m_sharing_buffer{false}
	{
		PC_tree_t data_node = PC_get(config, ".data");
		if (!PC_status(data_node)) {
			m_data_desc = PDI::to_string(data_node);
			m_ctx.logger()->debug("(FlowVR) Data Payload ({}): Data descriptor = {}", m_name, m_data_desc);
			PC_tree_t data_selection_node = PC_get(config, ".copy_data_selection");
			if (!PC_status(data_selection_node)) {
				m_data_selection = m_ctx.datatype(data_selection_node);
			}
		}
	}
	
	Payload_data(const Payload_data& other) = delete;
	
	Payload_data(Payload_data&& other):
		m_ctx{other.m_ctx},
		m_parent_port{other.m_parent_port},
		m_name{std::move(other.m_name)},
		m_data_desc{std::move(other.m_data_desc)},
		m_sharing_buffer{other.m_sharing_buffer}
	{}
	
	Payload_data& operator = (const Payload_data& other) = delete;
	
	Payload_data& operator = (Payload_data&& other)
	{
		m_ctx = other.m_ctx;
		m_parent_port = other.m_parent_port;
		m_name = std::move(other.m_name);
		m_data_desc = std::move(other.m_data_desc);
		m_sharing_buffer = other.m_sharing_buffer;
		return *this;
	}
	
	virtual ~Payload_data() = default;
	
}; // Payload_data



class Input_payload_data : public Input_payload, public Payload_data
{
	flowvr::InputPort* m_parent_input_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::Buffer m_flowvr_buffer;         //flowvr buffer where we store data
	
	std::string m_data_size_desc;  // name of the descritor to put the size of the received message
	
	void load_data_size_desc(PC_tree_t input_port_node)
	{
		PC_tree_t size_node = PC_get(input_port_node, ".size");
		if (!PC_status(size_node)) {
			m_data_size_desc = PDI::to_string(size_node);
			m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Data size descriptor = {}", m_name, m_data_size_desc);
		}
	}
	
public:
	Input_payload_data (PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::InputPort* parent_port):
		Payload_data(ctx, name, config, parent_port),
		m_parent_input_port{parent_port}
	{
		load_data_size_desc(config);
		if (!m_data_desc.empty()) {
			m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->copy_data_to_ref(name, ref);
			}, m_data_desc);
			m_ctx.add_empty_desc_access_callback([this](const std::string& name) {
				this->empty_desc_access(name);
			}, m_data_desc);
		}
		m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Created", m_name);
	}
	
	Input_payload_data(const Input_payload_data& other) = delete;
	
	Input_payload_data(Input_payload_data&& other):
		Payload_data(std::move(other)),
		m_parent_input_port{other.m_parent_input_port},
		m_flowvr_buffer{std::move(other.m_flowvr_buffer)},
		m_data_size_desc{std::move(other.m_data_size_desc)}
	{}
	
	Input_payload_data& operator = (const Input_payload_data& other) = delete;
	
	Input_payload_data& operator = (Input_payload_data&& other)
	{
		Payload_data::operator = (std::move(other));
		m_parent_input_port = other.m_parent_input_port;
		m_flowvr_buffer = std::move(other.m_flowvr_buffer);
		m_data_size_desc = std::move(other.m_data_size_desc);
		return *this;
	}
	
	/**
	 *  Called if user shares data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference where to copy data
	 */
	void copy_data_to_ref(const std::string& data_name, const PDI::Ref_w& ref) const
	{
		if (!m_sharing_buffer) { // if buffer is not shared, copy it
			if (ref) {
				if (ref.get() != m_flowvr_buffer.readAccess() && m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
					m_ctx.logger()->debug("(FlowVR) Input Data Payload  ({}): Copy data from FlowVR memory to `{}' descriptor", m_name, data_name);
					if (m_data_selection) {
						m_data_selection->evaluate(m_ctx)->data_from_dense_copy(ref.get(), m_flowvr_buffer.readAccess());
					} else {
						m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->data_from_dense_copy(ref.get(), m_flowvr_buffer.readAccess());
					}
				}
			} else {
				throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Input Data Payload (%s): Cannot get write access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
			}
		}
	}
	
	/**
	 *  Share the reference to the flowvr memory to the data_name descriptor
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name)
	{
		if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
			if (m_flowvr_buffer.unique(flowvr::Buffer::ALLSEGMENTS)) {
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Sharing (read/write) `{}'", m_name, m_data_desc);
				m_sharing_buffer = true;
				m_ctx[m_data_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess()), true, true);
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Share complete (read/write) `{}'", m_name, m_data_desc);
			} else {
				if (m_flowvr_buffer.valid()) {
					m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Share (read only) `{}'", m_name, m_data_desc);
					m_sharing_buffer = true;
					m_ctx[m_data_desc].share(const_cast<flowvr::ubyte*>(m_flowvr_buffer.readAccess()), true, false);
					m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Share complete (read only) `{}'", m_name, m_data_desc);
				}
			}
		} else {
			m_ctx.logger()->warn("(FlowVR) Input Data Payload ({}): Flowvr data {} is empty or not valid", m_name, m_data_desc);
			m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Sharing (nullptr) `{}'", m_name, m_data_desc);
			m_sharing_buffer = true;
			m_ctx[m_data_desc].share(PDI::Ref(nullptr, nullptr, PDI::UNDEF_TYPE.clone_type(), true, false), false, false);
			m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Share complete (nullptr) `{}'", m_name, m_data_desc);
		}
	}
	
	/**
	 *  Get message, udpate m_flowvr_buffer
	 *
	 *  \return stamp from flowvr::Message
	 */
	flowvr::Stamps get_message() override
	{
		if (!m_data_desc.empty()) {
			if (!m_ctx[m_data_desc].empty() && m_sharing_buffer) {
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Reclaiming `{}'", m_name, m_data_desc);
				m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_buffer = false;
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Reclaim complete `{}'", m_name, m_data_desc);
			}
			m_flowvr_buffer.clear();
		}
		
		flowvr::Message msg_to_get;
		m_parent_port->getModule()->get(m_parent_input_port, msg_to_get);
		m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Received {} B", m_name, msg_to_get.data.getSize());
		
		if (!m_data_desc.empty()) {
			m_flowvr_buffer = msg_to_get.data;
			
			if (!m_data_size_desc.empty()) {
				//update size in metadata
				long size = m_flowvr_buffer.getSize();
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Sharing size descripotr `{}' = {}", m_name, m_data_size_desc, size);
				m_ctx[m_data_size_desc].share(&size, true, false);
				m_ctx[m_data_size_desc].reclaim();
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Share complete `{}'", m_name, m_data_size_desc);
			}
		}
		return msg_to_get.stamps;
	}
	
	~Input_payload_data()
	{
		if (!m_ctx[m_data_desc].empty()) {
			if (m_sharing_buffer) {
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Reclaiming {}", m_name, m_data_desc);
				m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_buffer = false;
				m_ctx.logger()->debug("(FlowVR) Input Data Payload ({}): Reclaim complete {}", m_name, m_data_desc);
			}
		}
	}
	
}; // Input_payload_data



class Output_payload_data : public Output_payload, public Payload_data
{
	flowvr::OutputPort* m_parent_output_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::BufferWrite m_flowvr_buffer;
	
	std::unique_ptr<flowvr::BufferPool> m_flowvr_buffer_pool;
	
	void alloc_buffer()
	{
		size_t datasize = 0;
		if (m_data_selection) {
			datasize = m_data_selection->evaluate(m_ctx)->datasize();
		} else {
			datasize = m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->datasize();
		}
		m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Allocating {} B", m_name, datasize);
		if (m_flowvr_buffer_pool) {
			m_flowvr_buffer = m_flowvr_buffer_pool->alloc(m_parent_output_port->getModule()->getAllocator(), datasize);
		} else {
			m_flowvr_buffer = m_parent_port->getModule()->alloc(datasize);
		}
	}
	
public:
	Output_payload_data(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::OutputPort* parent_port):
		Payload_data(ctx, name, config, parent_port),
		m_parent_output_port{parent_port}
	{
		PC_tree_t data_pool_node = PC_get(config, ".data_size");
		if (!PC_status(data_pool_node)) {
			std::string data_pool = PDI::to_string(data_pool_node);
			if (data_pool == "const") {
				m_flowvr_buffer_pool.reset(new flowvr::BufferPool());
			} else {
				throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Payload ({}): `data_size' can be only const", m_name.c_str()};
			}
		}
		
		if (!m_data_desc.empty()) {
			m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->copy_data_from_ref(name, ref);
			}, m_data_desc);
			m_ctx.add_empty_desc_access_callback([this](const std::string& name) {
				this->empty_desc_access(name);
			}, m_data_desc);
		}
		m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Created", m_name);
	}
	
	Output_payload_data(const Output_payload_data& other) = delete;
	
	Output_payload_data(Output_payload_data&& other):
		Payload_data(std::move(other)),
		m_parent_output_port{other.m_parent_output_port},
		m_flowvr_buffer{std::move(other.m_flowvr_buffer)},
		m_flowvr_buffer_pool{std::move(other.m_flowvr_buffer_pool)}
	{}
	
	Output_payload_data& operator = (const Output_payload_data& other) = delete;
	
	Output_payload_data& operator = (Output_payload_data&& other)
	{
		Payload_data::operator = (std::move(other));
		m_parent_output_port = other.m_parent_output_port;
		m_flowvr_buffer = std::move(other.m_flowvr_buffer);
		m_flowvr_buffer_pool = std::move(other.m_flowvr_buffer_pool);
		return *this;
	}
	
	/**
	 *  Called if user shares data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference from we make a copy data
	 */
	void copy_data_from_ref(const std::string& data_name, const PDI::Ref_r& ref)
	{
		if (!m_sharing_buffer) { // if buffer is not shared, copy it
			if (ref) {
				if (m_flowvr_buffer.empty()) {
					alloc_buffer();
				}
				if (ref.get() != m_flowvr_buffer.writeAccess()) {
					if (m_flowvr_buffer.valid() && !m_flowvr_buffer.empty()) {
						m_ctx.logger()->debug("(FlowVR) Output Payload ({}): Copy data from `{}' descriptor to flowvr memory", m_name, data_name);
						if (m_data_selection) {
							m_data_selection->evaluate(m_ctx)->data_to_dense_copy(m_flowvr_buffer.writeAccess(), ref.get());
						} else {
							m_ctx[m_data_desc].default_type()->evaluate(m_ctx)->data_to_dense_copy(m_flowvr_buffer.writeAccess(), ref.get());
						}
					}
				}
			} else {
				throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Output Data Payload (%s): Cannot get read access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
			}
		}
	}
	
	/**
	 *  Allocate the flowvr memory and share the reference to it
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name)
	{
		if (m_flowvr_buffer.empty()) {
			alloc_buffer();
		}
		m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Sharing `{}'", m_name, m_data_desc);
		m_sharing_buffer = true; //must be before share (to not copy data)
		m_ctx[data_name].share(m_flowvr_buffer.writeAccess(), false, true);
		m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Share complete `{}'", m_name, m_data_desc);
	}
	
	/**
	 *  Put a message with data payload and stamps from argument
	 *
	 *  \param[in] stamps flowvr::Stamps to put to the message
	 *  \return stamp from sent flowvr::Message
	 */
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override
	{
		flowvr::MessageWrite msg_to_put;
		msg_to_put.stamps = stamps;
		
		if (m_data_desc.empty()) {
			msg_to_put.data = m_parent_port->getModule()->alloc(0);
		} else {
			msg_to_put.data = m_flowvr_buffer;
		}
		
		m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Putting message", m_name);
		m_parent_port->getModule()->put(m_parent_output_port, msg_to_put);
		
		if (!m_data_desc.empty()) {
			m_flowvr_buffer.clear();
		}
		if (m_sharing_buffer) {
			m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Reclaiming `{}'", m_name, m_data_desc);
			m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
			m_sharing_buffer = false;
			m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Reclaim complete `{}'", m_name, m_data_desc);
		}
		return msg_to_put.stamps;
	}
	
	~Output_payload_data()
	{
		if (!m_ctx[m_data_desc].empty()) {
			if (m_sharing_buffer) {
				m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Reclaiming `{}'", m_name, m_data_desc);
				m_ctx[m_data_desc].reclaim(); // have to reclaim (this is flowvr shared buffer)
				m_sharing_buffer = false;
				m_ctx.logger()->debug("(FlowVR) Output Data Payload ({}): Reclaim complete `{}'", m_name, m_data_desc);
			}
		}
	}
	
}; // class Output_payload_data

} // namespace <anonymous>

#endif // PDI_FLOWVR_PAYLOAD_DATA