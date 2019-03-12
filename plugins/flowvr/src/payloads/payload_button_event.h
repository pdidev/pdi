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

#ifndef PDI_FLOWVR_PAYLOAD_BUTTON_EVENT
#define PDI_FLOWVR_PAYLOAD_BUTTON_EVENT

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

class Payload_button_event
{
	std::unordered_map<std::string, unsigned char> m_name_to_flowvr_key {
		{"KEY_F1",        FLOWVR_KEY_F1},
		{"KEY_F2",        FLOWVR_KEY_F2},
		{"KEY_F3",        FLOWVR_KEY_F3},
		{"KEY_F4",        FLOWVR_KEY_F4},
		{"KEY_F5",        FLOWVR_KEY_F5},
		{"KEY_F6",        FLOWVR_KEY_F6},
		{"KEY_F7",        FLOWVR_KEY_F7},
		{"KEY_F8",        FLOWVR_KEY_F8},
		{"KEY_F9",        FLOWVR_KEY_F9},
		{"KEY_F10",       FLOWVR_KEY_F10},
		{"KEY_F11",       FLOWVR_KEY_F11},
		{"KEY_F12",       FLOWVR_KEY_F12},
		{"KEY_LEFT",      FLOWVR_KEY_LEFT},
		{"KEY_UP",        FLOWVR_KEY_UP},
		{"KEY_RIGHT",     FLOWVR_KEY_RIGHT},
		{"KEY_DOWN",      FLOWVR_KEY_DOWN},
		{"KEY_PAGE_UP",   FLOWVR_KEY_PAGE_UP},
		{"KEY_PAGE_DOWN", FLOWVR_KEY_PAGE_DOWN},
		{"KEY_HOME",      FLOWVR_KEY_HOME},
		{"KEY_END",       FLOWVR_KEY_END},
		{"KEY_INSERT",    FLOWVR_KEY_INSERT}
	};
	
protected:
	PDI::Context& m_ctx;
	flowvr::Port* m_parent_port;
	std::string m_name;
	
	std::unordered_map<unsigned char, std::string> m_key_desc; // loaded keys (defined in config)
	std::unordered_map<std::string, std::pair<unsigned char, bool>> m_desc_value_map;
	
	/**
	 *  Load defined descriptors keys
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_key_desc(PC_tree_t config)
	{
		PC_tree_t keys_node = PC_get(config, ".event_button");
		int nb_keys = PDI::len(keys_node, 0);
		for (int key_id = 0; key_id < nb_keys; key_id++) {
			std::string key = PDI::to_string(PC_get(keys_node, "{%d}", key_id));
			std::string desc = PDI::to_string(PC_get(keys_node, "<%d>", key_id));
			
			const auto& name_to_key_it = m_name_to_flowvr_key.find(key);
			if (name_to_key_it == m_name_to_flowvr_key.end()) {
				throw PDI::Error{PDI_ERR_CONFIG, "%s is not valid event button KEY", key};
			}
			
			m_key_desc.emplace(name_to_key_it->second, desc);
			m_desc_value_map.emplace(desc, std::make_pair(name_to_key_it->second,false));
		}
	}
	
	Payload_button_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::Port* parent_port):
		m_ctx{ctx},
		m_name{name},
		m_parent_port{parent_port}
	{
		load_key_desc(config);
	}
	
	Payload_button_event(const Payload_button_event& other) = delete;
	
	Payload_button_event(Payload_button_event&& other):
		m_ctx{other.m_ctx},
		m_name{std::move(other.m_name)},
		m_parent_port{other.m_parent_port},
		m_desc_value_map{std::move(other.m_desc_value_map)},
		m_name_to_flowvr_key{std::move(other.m_name_to_flowvr_key)}
	{}
	
	Payload_button_event& operator = (const Payload_button_event& other) = delete;
	
	Payload_button_event& operator = (Payload_button_event&& other)
	{
		m_ctx = other.m_ctx;
		m_name = std::move(other.m_name);
		m_parent_port = other.m_parent_port;
		m_desc_value_map = std::move(other.m_desc_value_map);
		m_name_to_flowvr_key = std::move(other.m_name_to_flowvr_key);
		return *this;
	}
	
public:

	virtual ~Payload_button_event() = default;
	
}; // class Payload_button_event



class Input_payload_button_event : public Payload_button_event, public Input_payload
{
	flowvr::InputPort* m_flowvr_input_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
public:
	Input_payload_button_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::InputPort* parent_port):
		Payload_button_event{ctx, name, config, parent_port},
		m_flowvr_input_port{parent_port}
	{
		m_ctx.logger()->debug("(FlowVR) Input Button Payload ({}): Created", m_name);
	}
	
	Input_payload_button_event(const Input_payload_button_event& other) = delete;
	
	Input_payload_button_event(Input_payload_button_event&& other):
		Payload_button_event{std::move(other)},
		m_flowvr_input_port{other.m_flowvr_input_port}
	{}
	
	Input_payload_button_event& operator = (const Input_payload_button_event& other) = delete;
	
	Input_payload_button_event& operator = (Input_payload_button_event&& other)
	{
		Payload_button_event::operator = (std::move(other));
		m_flowvr_input_port = other.m_flowvr_input_port;
		return *this;
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	bool data(const char* data_name, const PDI::Ref_w& ref) const override
	{
		const auto& desc_value = m_desc_value_map.find(data_name);
		if (desc_value != m_desc_value_map.end()) {
			if (ref) {
				m_ctx.logger()->debug("(FlowVR) Input Button Payload ({}): Copy key value = {} to `{}' descriptor", m_name, desc_value->second.second, data_name);
				*static_cast<int*>(ref.get()) = desc_value->second.second;
			} else {
				throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Input Button Payload (%s): Cannot get write access to `%s' descriptor", m_name.c_str(), data_name};
			}
			return true;
		}
		return false;
	}
	
	/**
	 *  Do nothing on share
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void share(const char* data_name) override
	{}
	
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
		for (ftl::ChunkIterator it = ftl::chunkBegin(msg_to_get); it != ftl::chunkEnd(msg_to_get); it++ ) {
			ftl::ChunkEventButton* chunk_button = (ftl::ChunkEventButton*)((const ftl::Chunk*)it);
			unsigned char key = chunk_button->key;
			if (chunk_button->val) {
				const auto& key_to_desc = m_key_desc.find(key);
				if (key_to_desc == m_key_desc.end()) {
					m_ctx.logger()->warn("(FlowVR) Input Button Payload ({}): Got unknown `{}' button", m_name, key);
					continue;
				}
				
				// this key was pressed
				m_desc_value_map[key_to_desc->second].second = true;
			}
		}
		return msg_to_get.stamps;
	}
	
	~Input_payload_button_event() = default;
	
}; // class Input_payload_button_event



class Output_payload_button_event : public Payload_button_event, public Output_payload
{
	ftl::ChunkEventWriter m_chunk_event_writer;
	flowvr::OutputPort* m_flowvr_output_port; // pointer to flowvr::Port (to avoid dynamic_cast every time)
	
public:
	Output_payload_button_event(PDI::Context& ctx, std::string name, PC_tree_t config, flowvr::OutputPort* parent_port):
		Payload_button_event{ctx, name, config, parent_port},
		m_flowvr_output_port{parent_port}
	{
		m_ctx.logger()->debug("(FlowVR) Output Button Payload ({}): Created", m_name);
	}
	
	Output_payload_button_event(const Output_payload_button_event& other) = delete;
	
	Output_payload_button_event(Output_payload_button_event&& other):
		Payload_button_event{std::move(other)},
		m_chunk_event_writer{std::move(other.m_chunk_event_writer)},
		m_flowvr_output_port{other.m_flowvr_output_port}
	{}
	
	Output_payload_button_event& operator = (const Output_payload_button_event& other) = delete;
	
	Output_payload_button_event& operator = (Output_payload_button_event&& other)
	{
		Payload_button_event::operator = (std::move(other));
		m_chunk_event_writer = std::move(other.m_chunk_event_writer);
		m_flowvr_output_port = other.m_flowvr_output_port;
		return *this;
	}
	
	/**
	 *  Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 */
	bool data(const char* data_name, const PDI::Ref_r& ref) override
	{
		const auto& desc_value = m_desc_value_map.find(data_name);
		if (desc_value != m_desc_value_map.end()) {
			if (ref) {
				desc_value->second.second = *static_cast<const int*>(ref.get());
				m_ctx.logger()->debug("(FlowVR) Output Button Payload ({}): Copied key value = {} from `{}' descriptor", m_name, desc_value->second.second, data_name);
			} else {
				throw PDI::Error{PDI_ERR_RIGHT, "(FlowVR) Output Button Payload (%s): Cannot get read access to `%s' descriptor", m_name.c_str(), data_name};
			}
			return true;
		}
		return false;
	}
	
	/**
	 *  Do nothing on share
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void share(const char* data_name) override
	{}
	
	/**
	 *  Get keys descriptors state, put message
	 *
	 *  \param[in] stamps flowvr::Stamps to put to the message
	 *  \return stamp from sent flowvr::Message
	 */
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override
	{
		for (const auto& desc_value_map : m_desc_value_map) {
			if (desc_value_map.second.second) {
				m_ctx.logger()->debug("(FlowVR) Output Button Payload ({}): Putting button: {}", m_name, desc_value_map.first);
				m_chunk_event_writer.addEventButton(desc_value_map.second.first, desc_value_map.second.second);
			}
		}
		
		flowvr::Message msg = m_chunk_event_writer.put(m_flowvr_output_port, stamps);
		return msg.stamps;
	}
	
	~Output_payload_button_event() = default;
	
}; // class Output_payload_button_event

} // namespace <anonymous>

#endif // PDI_FLOWVR_PAYLOAD_BUTTON_EVENT