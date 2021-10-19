/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <string>

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>


namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Plugin;
using std::string;

struct test_plugin: Plugin {

	test_plugin(Context& ctx, PC_tree_t config):
		Plugin {ctx}
	{
		ctx.callbacks().add_data_callback([this](const string& name, Ref ref) {
			this->context().logger().info("=>> data stop being available to the test plugin: {}", name);
		});
		ctx.callbacks().add_data_remove_callback([this](const string& name, Ref ref) {
			this->context().logger().info("<<= data stop being available to the test plugin: {}", name);
		});
		ctx.callbacks().add_event_callback([this](const string& name) {
			this->context().logger().info("               The test plugin received an event: {}", name);
		});
		context().logger().info("Welcome to the test plugin!");
		context().logger().warn("The test plugin is deprecated, use the `trace' plugin instead!");
	}
	
	~test_plugin()
	{
		context().logger().info("Goodbye from the test plugin!");
	}
	
	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static string pretty_name()
	{
		return "Test-plugin";
	}
	
}; // struct test_plugin

} // namespace <anonymous>

PDI_PLUGIN(test)
