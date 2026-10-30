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
using PDI::PdiTest;
using PDI::Plugin;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using std::string;

namespace {

/// The name of the data holding the loaded plugin name
constexpr char PLUGIN_NAME_DATA[] = "LOADED_PLUGIN_NAME";

/// A plugin that reports the name of its logger in the Context
struct prelinked_plugin: Plugin {
	/// The name of the plugin as reported by our logger, this should match our pretty name
	string m_name;

	static const char* pretty_name() { return "prelinked_plugin"; }

	prelinked_plugin(Context& ctx, PC_tree_t)
		: Plugin{ctx}
		, m_name(ctx.logger().real_logger()->name())
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

} // namespace

/// The `prelinked` plugin, to be found by PDI in the test executable, with no plugin library to load.
PDI_PLUGIN(prelinked)

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
