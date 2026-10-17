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

#include <string>

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <paraconf.h>

#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"

#include "data_store.h"
#include "logging_test.h"

using PDI::Context;
using PDI::Data_store;
using PDI::Logger;
using PDI::Plugin;
using PDI::Spectree_error;
using PDI::State_error;
using PDI::to_string;
using std::string;
using testing::AllOf;
using testing::ElementsAre;
using testing::HasSubstr;
using testing::MatchesRegex;
using testing::ThrowsMessage;

namespace {

/// The logger PDI gave the last `logger_probe' plugin it loaded
Logger* probe_logger = nullptr;

/** A plugin that records the logger PDI gives it, to check it uses the infrastructure of the PDI logger
 *
 * It adds the `block` of its configuration, if any, to the default pattern while it is loaded, as the MPI plugin does.
 * It is linked in the test executable, where PDI finds it before looking for a plugin library.
 */
struct logger_probe_plugin: Plugin {
	logger_probe_plugin(Logger& logger, Context& ctx, PC_tree_t config)
		: Plugin{logger, ctx}
	{
		probe_logger = &logger;
		PC_tree_t block_tree = PC_get(config, ".block");
		if (!PC_status(block_tree)) logger.add_default_pattern_block(to_string(block_tree));
	}

	static string pretty_name() { return "Logger-probe"; }
};

} // namespace

PDI_PLUGIN(logger_probe)

/** The unit tests of the plugin store
 *
 * The plugin store is internal to the PDI library, so the tests reach it through the data store that owns it.
 */
struct PluginStoreTest: public LoggingTest {};

/*
 * The logger of each plugin uses the infrastructure of the PDI logger: its outputs, its pattern and its default level
 */

TEST_F(PluginStoreTest, PluginLoggerUsesPdiLoggerInfrastructure)
{
	probe_logger = nullptr;
	Data_store store(config("{logging: {level: error, pattern: '%n|%l|%v', outputs: {o: {kind: stream}}}, plugins: {logger_probe: ~}}"));
	ASSERT_NE(probe_logger, nullptr);
	use_streams(store.logger());
	probe_logger->warn("hidden");
	probe_logger->error("shown");
	EXPECT_EQ(written(), "PDI/Logger-probe|error|shown\n");
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

TEST_F(PluginStoreTest, PluginLoggerReachesPdiLogger)
{
	probe_logger = nullptr;
	Data_store store(config("{logging: {level: error, pattern: '%n|%v', outputs: {o: {kind: stream}}}, plugins: {logger_probe: ~}}"));
	ASSERT_NE(probe_logger, nullptr);
	use_streams(store.logger());
	probe_logger->default_level("info");
	probe_logger->default_pattern("[%n] %v", store);
	store.logger().info("pdi");
	probe_logger->info("probe");
	EXPECT_EQ(written(), "[PDI] pdi\n[PDI/Logger-probe] probe\n");
}

TEST_F(PluginStoreTest, PluginLoggerSharesDefaultPatternBlocks)
{
	probe_logger = nullptr;
	Data_store store(config("{logging: {outputs: {o: {kind: stream}}}, plugins: {logger_probe: {block: B}}}"));
	ASSERT_NE(probe_logger, nullptr);
	use_streams(store.logger());
	store.logger().info("msg");
	probe_logger->info("msg");
	EXPECT_THAT(
		written_lines(),
		ElementsAre(MatchesRegex(default_pattern_regex(R"(\[B\])", "PDI")), MatchesRegex(default_pattern_regex(R"(\[B\])", "PDI/Logger-probe")))
	);
}

TEST_F(PluginStoreTest, InvalidPluginLoggerNamesThePlugin)
{
	EXPECT_THAT(
		[&] { Data_store store(config("{plugins: {logger_probe: {logging: {level: verbose}}}}")); },
		ThrowsMessage<Spectree_error>(AllOf(HasSubstr("plugin `logger_probe'"), HasSubstr("`verbose'")))
	);
}

TEST_F(PluginStoreTest, PluginLoggerBlocksAfterInitializationThrow)
{
	probe_logger = nullptr;
	Data_store store(config("{logging: error, plugins: {logger_probe: ~}}"));
	ASSERT_NE(probe_logger, nullptr);
	EXPECT_THROW(probe_logger->add_default_pattern_block("B"), State_error);
}

#pragma GCC diagnostic pop
