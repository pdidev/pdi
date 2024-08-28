/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <list>
#include <map>
#include <memory>
#include <regex>
#include <string>
#include <unordered_map>
#include <utility>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include "trigger.h"

namespace {

struct set_value_plugin: PDI::Plugin {
	// Trigger sets/shares/exposes values
	std::list<set_value::Trigger> m_triggers_list;

	/// Trigger on plugin finalize
	std::vector<set_value::Trigger> m_trigger_on_finalize;

	/** Creates Triggers and adds init/event/callbacks from PC_tree_t
	 * \param[in] config set_value plugin config node
	 */
	void load_config(PC_tree_t config)
	{
		PC_tree_t on_init = PC_get(config, ".on_init");
		if (!PC_status(on_init)) {
			m_triggers_list.emplace_back(context(), on_init);
			m_triggers_list.back().execute();
		}

		PC_tree_t on_event = PC_get(config, ".on_event");
		if (!PC_status(on_event)) {
			int map_len = PDI::len(on_event);
			for (int i = 0; i < map_len; i++) {
				std::string event_name = PDI::to_string(PC_get(on_event, "{%d}", i));
				PC_tree_t value_node = PC_get(on_event, "<%d>", i);
				m_triggers_list.emplace_back(context(), value_node);
				set_value::Trigger& trigger = m_triggers_list.back();
				context().callbacks().add_event_callback([&trigger](const std::string&) { trigger.execute(); }, event_name);
			}
		}

		PC_tree_t on_data = PC_get(config, ".on_data");
		if (!PC_status(on_data)) {
			int map_len = PDI::len(on_data);
			for (int i = 0; i < map_len; i++) {
				std::string data_name = PDI::to_string(PC_get(on_data, "{%d}", i));
				PC_tree_t value_node = PC_get(on_data, "<%d>", i);
				m_triggers_list.emplace_back(context(), value_node);
				set_value::Trigger& trigger = m_triggers_list.back();
				context().callbacks().add_data_callback([&trigger](const std::string&, PDI::Ref) { trigger.execute(); }, data_name);
			}
		}

		PC_tree_t on_finalize = PC_get(config, ".on_finalize");
		if (!PC_status(on_finalize)) {
			m_trigger_on_finalize.emplace_back(context(), on_finalize);
		}
	}

	set_value_plugin(PDI::Context& ctx, PC_tree_t config)
		: PDI::Plugin{ctx}
	{
		// initialize after all descriptors are loaded, user can set value on_init
		context().callbacks().add_init_callback([this, config]() { this->load_config(config); });
		context().logger().info("Plugin loaded successfully");
	}

	~set_value_plugin()
	{
		for (auto&& trigger: m_trigger_on_finalize) {
			trigger.execute();
		}
		context().logger().info("Closing plugin");
	}

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "Set-value"; }
};

} // namespace

PDI_PLUGIN(set_value)
