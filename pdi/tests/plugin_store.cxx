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

#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>
#include <pdi/testing.h>

using PDI::Array_datatype;
using PDI::Context;
using PDI::Logger;
using PDI::PdiTest;
using PDI::Plugin;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using std::pair;
using std::string;
using std::unique_ptr;
using std::unordered_set;

namespace {

/// The name of the data holding the loaded plugin name
constexpr char PLUGIN_NAME_DATA[] = "LOADED_PLUGIN_NAME";

/// A plugin that reports the name of its logger in the Context
struct prelinked_plugin: Plugin {
	/// The name of the plugin as reported by our logger, this should match our pretty name
	string m_name;

	static const char* pretty_name() { return "prelinked_plugin"; }

	prelinked_plugin(Logger& logger, Context& ctx, PC_tree_t)
		: Plugin{logger, ctx}
		, m_name(logger.real_logger()->name())
	{
		// share a reference to the name with no memory destruction function
		ctx[PLUGIN_NAME_DATA].share(
			PDI::Ref(
				m_name.data(),
				[](void*) {},
				Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), m_name.size() + 1),
				true,
				false
			),
			true,
			false
		);
	}

	~prelinked_plugin() { context()[PLUGIN_NAME_DATA].reclaim(); }
};

/// The name of the logger of the last `no_pretty_name` plugin created
string no_pretty_name_logger_name;

/// A plugin that records the name of its logger
struct no_pretty_name_plugin: Plugin {
	no_pretty_name_plugin(Logger& logger, Context& ctx)
		: Plugin{logger, ctx}
	{
		no_pretty_name_logger_name = logger.real_logger()->name();
	}
};

} // namespace

/// The `prelinked` plugin, to be found by PDI in the test executable, with no plugin library to load.
PDI_PLUGIN(prelinked)

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
extern "C" unique_ptr<Plugin> PDI_EXPORT PDI_plugin_no_pretty_name_loader(Logger& logger, Context& ctx, PC_tree_t conf)
{
	return unique_ptr<Plugin>{new no_pretty_name_plugin{logger, ctx}};
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
 * A plugin linked into the application loads, with no plugin library to search for
 */
TEST_F(PluginLoaderTest, PrelinkedPluginLoads)
{
	InitPdi(PC_parse_string("plugins: {prelinked: ~}"));
	void* name = nullptr;
	ASSERT_EQ(PDI_OK, PDI_access(PLUGIN_NAME_DATA, &name, PDI_IN));
	EXPECT_STREQ(prelinked_plugin::pretty_name(), static_cast<const char*>(name));
	EXPECT_EQ(PDI_OK, PDI_release(PLUGIN_NAME_DATA));
}

/*
 * A plugin that provides no pretty name loads, under its own name
 */
TEST_F(PluginLoaderTest, PluginWithoutPrettyNameIsNamedAfterItself)
{
	InitPdi(PC_parse_string("plugins: {no_pretty_name: ~}"));
	EXPECT_EQ(no_pretty_name_logger_name, "no_pretty_name");
}
