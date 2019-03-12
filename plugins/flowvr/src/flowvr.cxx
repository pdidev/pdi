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

#include <memory>

#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>

#include <spdlog/spdlog.h>

#include "component.h"
#include "module.h"

namespace {

struct flowvr_plugin: PDI::Plugin {
private:
	bool m_init;
	std::vector<std::string> m_init_on;
	PC_tree_t m_config;
	std::vector<std::unique_ptr<Component>> m_flowvr_components;
	
	void create_component(PC_tree_t component_node)
	{
		if (PC_status(component_node)) {
			throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Component type must be defined. Available options: `module'"};
		}
		std::string component_str = PDI::to_string(PC_get(component_node, ".component"));
		if (component_str == "module") {
			m_flowvr_components.emplace_back(new Module(context(), component_node));
		} else {
			throw PDI::Error {PDI_ERR_CONFIG, "(FlowVR) Component `%s' is invalid. Available options: `module'", component_str};
		}
	}
	
	void add_init_on(PC_tree_t init_on_node)
	{
		if (!PC_status(init_on_node)) {
			std::string event_name = PDI::to_string(init_on_node);
			context().logger()->debug("(FlowVR) Wait on `{}' event", event_name);
			m_init_on.emplace_back(std::move(event_name));
		}
	}
	
	void get_init_on()
	{
		//check if load multiple components
		if (!PC_status(PC_get(m_config, "[0]"))) {
			int nb_comp = PDI::len(m_config);
			for (int comp_id = 0; comp_id < nb_comp; comp_id++) {
				add_init_on(PC_get(m_config, "[%d].init_on", comp_id));
			}
		} else {
			add_init_on(PC_get(m_config, ".init_on"));
		}
	}
	
	void init_flowvr()
	{
		if (!m_init) {
			//check if load multiple components
			if (!PC_status(PC_get(m_config, "[0]"))) {
				int nb_comp = PDI::len(m_config);
				for (int comp_id = 0; comp_id < nb_comp; comp_id++) {
					create_component(PC_get(m_config, "[%d]", comp_id));
				}
			} else {
				create_component(m_config);
			}
			
			m_init = true;
			context().logger()->info("(FlowVR) Plugin initialized successfully");
		}
	}
	
public:
	flowvr_plugin(PDI::Context& ctx, PC_tree_t config):
		Plugin{ctx},
		m_init{false},
		m_config{config}
	{
		//FlowVR reads types of data descriptors, but data is read after plugins. That's why cannot init flowvr in constructor.
		get_init_on();
	}
	
	/** Notification for a named event
	 * \param[in] event the event name
	 */
	void event(const char* event_name) override
	{
		for (const std::string& wait_on : m_init_on) {
			if (wait_on == event_name) {
				init_flowvr();
			}
		}
		if (m_init_on.empty()) { //if init_on, need to init on event
			init_flowvr();
		}
		if (m_init) {
			for (auto& component : m_flowvr_components) {
				component->event(event_name);
			}
		}
	}
	
	/** Notification that some data becomes available
	 * \param[in] name the name of the data made available
	 * \param[in] ref a reference to the data value
	 */
	void data(const char* data_name, PDI::Ref ref) override
	{
		if (m_init_on.empty()) { //if init_on, need to init on event
			init_flowvr();
		}
		if (m_init) {
			for (auto& component : m_flowvr_components) {
				component->data(data_name, ref);
			}
		}
	}
	
	/** Notification for accessing empty desc by user
	 * \param[in] name the name of accessing desc
	 */
	void empty_desc_access(const char* data_name) override
	{
		if (m_init_on.empty()) { //if init_on, need to init on event
			init_flowvr();
		}
		if (m_init) {
			for (auto& component : m_flowvr_components) {
				component->empty_desc_access(data_name);
			}
		}
	}
	
	~flowvr_plugin() = default;
};

} // namespace <anonymous>

PDI_PLUGIN(flowvr)
