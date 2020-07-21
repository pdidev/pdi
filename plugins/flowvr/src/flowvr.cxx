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
	std::vector<std::unique_ptr<Component>> m_flowvr_components;
	
	void create_component(PC_tree_t component_node)
	{
		if (PC_status(component_node)) {
			throw PDI::Config_error{"(FlowVR) Component type must be defined. Available options: `module'"};
		}
		std::string component_str = PDI::to_string(PC_get(component_node, ".component"));
		if (component_str == "module") {
			m_flowvr_components.emplace_back(new Module(context(), component_node));
		} else {
			throw PDI::Config_error {"(FlowVR) Component `{}' is invalid. Available options: `module'", component_str};
		}
	}
	
	void load_init_event(PC_tree_t component_node)
	{
		PC_tree_t init_on_node = PC_get(component_node, ".init_on");
		if (!PC_status(init_on_node)) {
			std::string event_name = PDI::to_string(init_on_node);
			context().logger()->debug("(FlowVR) Init on `{}' event", event_name);
			context().add_event_callback([this, component_node](const std::string& name) {
				this->create_component(component_node);
			}, event_name);
		} else {
			context().add_init_callback([this, component_node]() {
				this->create_component(component_node);
			});
		}
	}
	
public:
	flowvr_plugin(PDI::Context& ctx, PC_tree_t config):
		Plugin{ctx}
	{
		//FlowVR reads types of data descriptors, but data is read after plugins. That's why cannot init flowvr in constructor.
		if (!PC_status(PC_get(config, "[0]"))) {    //check if load multiple components
			int nb_comp = PDI::len(config);
			for (int comp_id = 0; comp_id < nb_comp; comp_id++) {
				load_init_event(PC_get(config, "[%d]", comp_id));
			}
		} else {
			load_init_event(config);
		}
	}
	
	~flowvr_plugin() = default;
};

} // namespace <anonymous>

PDI_PLUGIN(flowvr)
