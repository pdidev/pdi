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

/// The name of the logger of the last `prelinked` plugin created
string prelinked_logger_name;

/// A plugin that records the name of its logger
struct prelinked_plugin: Plugin {
	prelinked_plugin(Context& ctx, PC_tree_t)
		: Plugin{ctx}
	{
		prelinked_logger_name = ctx.logger().real_logger()->name();
	}
};

} // namespace

/*
 * The symbols of the `prelinked` plugin, written by hand, as PDI_PLUGIN would define them.
 * PDI finds them in the test executable, that exports them, with no plugin library to load.
 */

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-prototypes"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#endif

/// The constructor of the `prelinked` plugin
extern "C" unique_ptr<Plugin> PDI_EXPORT PDI_plugin_prelinked_loader(Context& ctx, PC_tree_t conf)
{
	return unique_ptr<Plugin>{new prelinked_plugin{ctx, conf}};
}

/// The dependencies of the `prelinked` plugin: none
extern "C" pair<unordered_set<string>, unordered_set<string>> PDI_EXPORT PDI_plugin_prelinked_dependencies()
{
	return {};
}

/// The pretty name of the `prelinked` plugin
extern "C" string PDI_EXPORT PDI_plugin_prelinked_pretty_name()
{
	return "Prelinked";
}

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

/// The tests of plugin loading
struct PluginLoaderTest: PdiTest {};

/*
 * A plugin linked into the application loads, with no plugin library to search for
 */
TEST_F(PluginLoaderTest, PrelinkedPluginLoads)
{
	InitPdi(PC_parse_string("plugins: {prelinked: ~}"));
	EXPECT_EQ(prelinked_logger_name, "Prelinked");
}
