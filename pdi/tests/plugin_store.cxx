/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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
#include <string>
#include <unordered_set>
#include <utility>

#include <gtest/gtest.h>

#include <paraconf.h>
#include <spdlog/logger.h>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/testing.h>

using PDI::Context;
using PDI::PdiTest;
using PDI::Plugin;
using std::pair;
using std::string;
using std::unique_ptr;
using std::unordered_set;

namespace {

/// The name of the logger of the last `no_pretty_name` plugin created
string no_pretty_name_logger_name;

/// A plugin that records the name of its logger
struct no_pretty_name_plugin: Plugin {
	no_pretty_name_plugin(Context& ctx, PC_tree_t)
		: Plugin{ctx}
	{
		no_pretty_name_logger_name = ctx.logger().real_logger()->name();
	}
};

} // namespace

/*
 * The symbols of the `no_pretty_name` plugin, written by hand, as a plugin built without PDI_PLUGIN may be, with no pretty name symbol.
 * PDI finds them in the test executable, that exports them.
 */

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#endif

/// The constructor of the `no_pretty_name` plugin
extern "C" unique_ptr<Plugin> PDI_EXPORT PDI_plugin_no_pretty_name_loader(Context& ctx, PC_tree_t conf)
{
	return unique_ptr<Plugin>{new no_pretty_name_plugin{ctx, conf}};
}

/// The dependencies of the `no_pretty_name` plugin: none
extern "C" pair<unordered_set<string>, unordered_set<string>> PDI_EXPORT PDI_plugin_no_pretty_name_dependencies()
{
	return {};
}

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

/// The tests of plugin loading
struct PluginLoaderTest: PdiTest {};

/*
 * A plugin that provides no pretty name loads, under its own name
 */
TEST_F(PluginLoaderTest, PluginWithoutPrettyNameIsNamedAfterItself)
{
	InitPdi(PC_parse_string("plugins: {no_pretty_name: ~}"));
	EXPECT_EQ(no_pretty_name_logger_name, "no_pretty_name");
}
