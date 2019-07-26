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

#ifndef PDI_FLOWVR_PAYLOAD_MOUSE_EVENT
#define PDI_FLOWVR_PAYLOAD_MOUSE_EVENT

#include <functional>
#include <memory>
#include <string>

#include <flowvr/module.h>
#include <ftl/chunkevents.h>

#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>
#include <spdlog/spdlog.h>

#include "payload_interface.h"

namespace  {

class Payload_mouse_event
{
	std::unordered_map<std::string, unsigned char> m_name_to_flowvr_key {
		{"LEFT_BUTTON",   0x01},
		{"MIDDLE_BUTTON", 0x02},
		{"RIGHT_BUTTON",  0x04},
		{"POS_XY",        0x08}
	};
	
protected:
	PDI::Context& m_ctx;
	flowvr::Port* m_parent_port;
	std::string m_name;
	
	std::unordered_map<unsigned char, std::string> m_key_desc; // loaded keys (defined in config)
	std::pair<std::string, std::pair<unsigned char, float[2]>> m_desc_pos_xy;
	std::unordered_map<std::string, std::pair<unsigned char, bool>> m_desc_value_map;
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/**
	 *  Load defined descriptors keys
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_key_desc(PC_tree_t config)
	{
		PC_tree_t keys_node = PC_get(config, ".event_mouse");
		int nb_keys = PDI::len(keys_node, 0);
		for (int key_id = 0; key_id < nb_keys; key_id++) {
			std::string key = PDI::to_string(PC_get(keys_node, "{%d}", key_id));
			std::string desc = PDI::to_string(PC_get(keys_node, "<%d>", key_id));
			
			const auto& name_to_key_it = m_name_to_flowvr_key.find(key);
			if (name_to_key_it == m_name_to_flowvr_key.end()) {
				throw PDI::Error{PDI_ERR_CONFIG, "%s is not valid event mouse KEY", key.c_str()};
			}
			
			if (key == "POS_XY") {
				m_desc_pos_xy.first = desc;
				m_desc_pos_xy.second.first = name_to_key_it->second;
				m_desc_pos_xy.second.second[0] = 0.0;
				m_desc_pos_xy.second.second[1] = 0.0;
				
				// float default_value[2] = {0.0};
				// m_ctx[desc].share(&default_value, true, false);
				// m_ctx[desc].reclaim();
			} else {
				m_desc_value_map.emplace(desc, std::make_pair(name_to_key_it->second, false));
				// int default_value = 0;
				// m_ctx[desc].share(&default_value, true, false);
				// m_ctx[desc].reclaim();
			}
			m_key_desc.emplace(name_to_key_it->second, std::move(desc));
		}
	}
	
	Payload_mouse_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::Port* parent_port):
		m_ctx{ctx},
		m_parent_port{parent_port},
		m_name{name}
	{
		load_key_desc(config);
	}
	
	Payload_mouse_event(const Payload_mouse_event& other) = delete;
	
	Payload_mouse_event(Payload_mouse_event&& other):
		m_ctx{other.m_ctx},
		m_name{std::move(other.m_name)},
		m_parent_port{other.m_parent_port},
		m_key_desc{std::move(other.m_key_desc)},
		m_name_to_flowvr_key{std::move(other.m_name_to_flowvr_key)}
	{}
	
	Payload_mouse_event& operator = (const Payload_mouse_event& other) = delete;
	
	Payload_mouse_event& operator = (Payload_mouse_event&& other)
	{
		m_ctx = other.m_ctx;
		m_name = std::move(other.m_name);
		m_parent_port = other.m_parent_port;
		m_key_desc = std::move(other.m_key_desc);
		m_name_to_flowvr_key = std::move(other.m_name_to_flowvr_key);
		return *this;
	}
	
public:

	virtual ~Payload_mouse_event()
	{
		for (auto& func : m_callbacks_remove) {
			func();
		}
	}
	
}; // class Payload_mouse_event



class Input_payload_mouse_event : public Payload_mouse_event, public Input_payload
{
	flowvr::InputPort* m_flowvr_input_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
public:
	Input_payload_mouse_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::InputPort* parent_port):
		Payload_mouse_event{ctx, name, config, parent_port},
		m_flowvr_input_port{parent_port}
	{
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data_pos_xy(name, ref);
		}, m_desc_pos_xy.first));
		for (const auto& desc_value : m_desc_value_map) {
			m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->data(name, ref);
			}, desc_value.first));
		}
		m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Created", m_name);
	}
	
	Input_payload_mouse_event(const Input_payload_mouse_event& other) = delete;
	
	Input_payload_mouse_event(Input_payload_mouse_event&& other):
		Payload_mouse_event{std::move(other)},
		m_flowvr_input_port{other.m_flowvr_input_port}
	{}
	
	Input_payload_mouse_event& operator = (const Input_payload_mouse_event& other) = delete;
	
	Input_payload_mouse_event& operator = (Input_payload_mouse_event&& other)
	{
		Payload_mouse_event::operator = (std::move(other));
		m_flowvr_input_port = other.m_flowvr_input_port;
		return *this;
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	void data_pos_xy(const std::string& data_name, const PDI::Ref_w& ref) const
	{
		if (ref) {
			m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Copy mouse position to `{}'", m_name, data_name);
			memcpy(ref.get(), m_desc_pos_xy.second.second, 2 * sizeof(float));
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Input Mouse Payload (%s): Cannot get write access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
		}
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	void data(const std::string& data_name, const PDI::Ref_w& ref) const
	{
		if (ref) {
			const auto& desc_value = m_desc_value_map.find(data_name);
			*static_cast<int*>(ref.get()) = desc_value->second.second;
			m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Copy mouse button state to `{}'", m_name, data_name);
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Input Mouse Payload (%s): Cannot get write access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
		}
	}
	
	/**
	 *  Get message, update keys descriptors state.
	 *
	 *  \return stamp from flowvr::Message
	 */
	flowvr::Stamps get_message() override
	{
		flowvr::Message msg_to_get;
		m_parent_port->getModule()->get(m_flowvr_input_port, msg_to_get);
		
		//set all keys as false
		for (auto& desc_value : m_desc_value_map) {
			desc_value.second.second = false;
		}
		
		//set true to pressed keys
		for (ftl::ChunkIterator it = ftl::chunkBegin(msg_to_get); it != ftl::chunkEnd(msg_to_get); it++) {
			ftl::ChunkEventMouse* chunk_mouse = (ftl::ChunkEventMouse*)((const ftl::Chunk*)it);
			
			int key_left = chunk_mouse->mouseKeys & 0x01; //0x0001
			int key_middle = chunk_mouse->mouseKeys & 0x02; //0x0010
			int key_right = chunk_mouse->mouseKeys & 0x04; //0x0100
			
			const auto& left_it = m_key_desc.find(0x01);
			if (left_it != m_key_desc.end()) {
				m_desc_value_map[left_it->second].second = static_cast<bool>(key_left);
				m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Received left mouse button state = {}", m_name, static_cast<bool>(key_left));
			}
			
			const auto& middle_it = m_key_desc.find(0x02);
			if (middle_it != m_key_desc.end()) {
				m_desc_value_map[middle_it->second].second = static_cast<bool>(key_middle);
				m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Received middle mouse button state = {}", m_name, static_cast<bool>(key_middle));
			}
			
			const auto& right_it = m_key_desc.find(0x04);
			if (right_it != m_key_desc.end()) {
				m_desc_value_map[right_it->second].second = static_cast<bool>(key_right);
				m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Received right mouse button state = {}", m_name, static_cast<bool>(key_right));
			}
			
			const auto& mouse_it = m_key_desc.find(0x08);
			if (mouse_it != m_key_desc.end()) {
				m_desc_pos_xy.second.second[0] = chunk_mouse->mouseTranslation[0];
				m_desc_pos_xy.second.second[1] = chunk_mouse->mouseTranslation[1];
				m_ctx.logger()->debug("(FlowVR) Input Mouse Payload ({}): Received mouse position = [{}, {}]", m_name, m_desc_pos_xy.second.second[0], m_desc_pos_xy.second.second[1]);
			}
		}
		return msg_to_get.stamps;
	}
	
	~Input_payload_mouse_event() = default;
	
}; // class Input_payload_mouse_event



class Output_payload_mouse_event : public Payload_mouse_event, public Output_payload
{
	ftl::ChunkEventWriter m_chunk_event_writer;
	flowvr::OutputPort* m_flowvr_output_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
public:
	Output_payload_mouse_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::OutputPort* parent_port):
		Payload_mouse_event{ctx, name, config, parent_port},
		m_flowvr_output_port{parent_port}
	{
		m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->data_pos_xy(name, ref);
		}, m_desc_pos_xy.first));
		for (const auto& desc_value : m_desc_value_map) {
			m_callbacks_remove.emplace_back(m_ctx.add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->data(name, ref);
			}, desc_value.first));
		}
		m_ctx.logger()->debug("(FlowVR) Output Mouse Payload ({}): Created", m_name);
	}
	
	Output_payload_mouse_event(const Output_payload_mouse_event& other) = delete;
	
	Output_payload_mouse_event(Output_payload_mouse_event&& other):
		Payload_mouse_event{std::move(other)},
		m_chunk_event_writer{std::move(other.m_chunk_event_writer)},
		m_flowvr_output_port{other.m_flowvr_output_port}
	{}
	
	Output_payload_mouse_event& operator = (const Output_payload_mouse_event& other) = delete;
	
	Output_payload_mouse_event& operator = (Output_payload_mouse_event&& other)
	{
		Payload_mouse_event::operator = (std::move(other));
		m_chunk_event_writer = std::move(other.m_chunk_event_writer);
		m_flowvr_output_port = other.m_flowvr_output_port;
		return *this;
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	void data_pos_xy(const std::string& data_name, const PDI::Ref_r& ref)
	{
		if (ref) {
			m_ctx.logger()->debug("(FlowVR) Output Mouse Payload ({}): Copy mouse position to `{}'", m_name, data_name);
			memcpy(m_desc_pos_xy.second.second, ref.get(), 2 * sizeof(float));
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Output Mouse Payload (%s): Cannot get read access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
		}
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	void data(const std::string& data_name, const PDI::Ref_r& ref)
	{
		if (ref) {
			const auto& desc_value = m_desc_value_map.find(data_name);
			desc_value->second.second = *static_cast<const int*>(ref.get());
			m_ctx.logger()->debug("(FlowVR) Output Mouse Payload ({}): Copy mouse button state to `{}'", m_name, data_name);
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Output Mouse Payload (%s): Cannot get read access to `%s' descriptor", m_name.c_str(), data_name.c_str()};
		}
	}
	
	/**
	 *  Get keys descriptors state, put message
	 *
	 *  \param[in] stamps flowvr::Stamps to put to the message
	 *  \return stamp from sent flowvr::Message
	 */
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override
	{
		int mouse_keys = 0;
		
		for (const auto& desc_value: m_desc_value_map) {
			if (desc_value.second.second) { //if pressed
				m_ctx.logger()->debug("(FlowVR) Output Mouse Payload ({}): Pressed key with descriptor `{}'", m_name, desc_value.first);
				mouse_keys = mouse_keys | desc_value.second.first;
			}
		}
		
		m_ctx.logger()->debug("(FlowVR) Output Mouse Payload ({}): Mouse position [{}, {}]", m_name, m_desc_pos_xy.second.second[0], m_desc_pos_xy.second.second[1]);
		m_chunk_event_writer.addEventMouse(mouse_keys, m_desc_pos_xy.second.second);
		flowvr::Message msg = m_chunk_event_writer.put(m_flowvr_output_port, stamps);
		return msg.stamps;
	}
	
	~Output_payload_mouse_event() = default;
	
}; // class Output_payload_mouse_event

} // namespace <anonymous>

#endif // PDI_FLOWVR_PAYLOAD_MOUSE_EVENT