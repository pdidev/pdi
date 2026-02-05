// SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <string>

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>

namespace {

using PDI::Context;
using PDI::Plugin;
using PDI::Ref;
using std::string;

struct trace_plugin: Plugin {
	trace_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
	{
		ctx.callbacks().add_data_callback([this](const string& name, Ref ref) {
			this->context().logger().info("=>>   data becoming available in the store: {}", name);
		});
		ctx.callbacks().add_data_remove_callback([this](const string& name, Ref ref) {
			this->context().logger().info("<<= data stop being available in the store: {}", name);
		});
		ctx.callbacks().add_event_callback([this](const string& name) {
			this->context().logger().info("!!!                            named event: {}", name);
		});
		context().logger().info("Welcome!");
	}

	~trace_plugin() { context().logger().info("Goodbye!"); }

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static string pretty_name() { return "Trace"; }

}; // struct trace_plugin

} // namespace

PDI_PLUGIN(trace)
