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

#include <cerrno>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <system_error>
#include <utility>
#include <vector>

#include <unistd.h>

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <paraconf.h>
#include <spdlog/common.h>
#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/sinks/ostream_sink.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "pdi/array_datatype.h"
#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "logging_test.h"
#include "mocks/context_mock.h"
#include "mocks/data_descriptor_mock.h"
#include "pdi_logger.h"
#include "plugin_logger.h"

using PDI::Array_datatype;
using PDI::Invalid_action_error;
using PDI::Logger;
using PDI::Pdi_logger;
using PDI::Plugin_logger;
using PDI::Ref_r;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::Spectree_error;
using PDI::State_error;
using PDI::Value_error;
using spdlog::spdlog_ex;
using spdlog::sinks::basic_file_sink_st;
using spdlog::sinks::ostream_sink_st;
using spdlog::sinks::stderr_color_sink_st;
using spdlog::sinks::stdout_color_sink_st;
using std::dynamic_pointer_cast;
using std::generic_category;
using std::ifstream;
using std::istreambuf_iterator;
using std::optional;
using std::pair;
using std::string;
using std::to_string;
using std::vector;
using std::filesystem::create_directories;
using std::filesystem::current_path;
using std::filesystem::path;
using std::filesystem::remove_all;
using std::filesystem::temp_directory_path;
using testing::AllOf;
using testing::ElementsAre;
using testing::HasSubstr;
using testing::IsEmpty;
using testing::Matcher;
using testing::MatchesRegex;
using testing::NiceMock;
using testing::ReturnRef;
using testing::StrEq;
using testing::ThrowsMessage;
using testing::UnitTest;
using testing::UnorderedElementsAre;
using testing::Values;
using testing::WithParamInterface;

/// The logging levels
using Level = Logger::Level;

/** A logger whose level and outputs the tests can inspect, where reading what it writes would mean reading the console
 *
 * \tparam Logger_type the type of the logger
 */
template <class Logger_type>
struct Inspectable: public Logger_type {
	using Logger_type::Logger_type;

	/** The level of the logger
	 * \return the level
	 */
	Level level() const { return this->m_logger.level(); }

	/** The kinds of the outputs the logger writes to, in order
	 * \return the kind of each output, as a configuration names it
	 */
	vector<string> output_kinds() const
	{
		vector<string> result;
		for (auto&& sink: this->m_logger.sinks()) {
			if (dynamic_pointer_cast<basic_file_sink_st>(sink)) {
				result.emplace_back("file");
			} else if (dynamic_pointer_cast<stdout_color_sink_st>(sink)) {
				result.emplace_back("stdout");
			} else if (dynamic_pointer_cast<stderr_color_sink_st>(sink)) {
				result.emplace_back("stderr");
			} else if (dynamic_pointer_cast<ostream_sink_st>(sink)) {
				result.emplace_back("stream");
			} else {
				result.emplace_back("unknown");
			}
		}
		return result;
	}
};

/** The unit tests of the logger classes
 *
 * The loggers mostly write to outputs of kind `stream`, whose streams belong to the test.
 * Each test also has a directory of its own for the files its loggers write to, and may make it the working directory.
 * The patterns are evaluated in a context where `x` is an integer and `s` a string, whose values the test sets.
 */
struct LoggerTest: public LoggingTest {
	/// The working directory before the test, restored after it
	path m_cwd;

	/// The directory of this test for the log files
	path m_dir;

	/// The context in which to evaluate the patterns
	NiceMock<MockContext> m_ctx;

	/// The descriptor of `x` in m_ctx
	NiceMock<MockDataDescriptor> m_x_desc;

	/// The descriptor of `s` in m_ctx
	NiceMock<MockDataDescriptor> m_s_desc;

	/// The value of `x` in m_ctx
	long m_x = 0;

	/// The value of `s` in m_ctx
	string m_s;

	void SetUp() override
	{
		LoggingTest::SetUp();
		m_cwd = current_path();
		m_dir = temp_directory_path() / ("pdi_" + string(UnitTest::GetInstance()->current_test_info()->name()) + "_" + to_string(getpid()));
		create_directories(m_dir);
		ON_CALL(m_ctx, desc(Matcher<const char*>(StrEq("x")))).WillByDefault(ReturnRef(m_x_desc));
		ON_CALL(m_x_desc, ref()).WillByDefault([this] {
			return Ref_r{&m_x, [](void*) {}, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), true, true};
		});
		ON_CALL(m_ctx, desc(Matcher<const char*>(StrEq("s")))).WillByDefault(ReturnRef(m_s_desc));
		ON_CALL(m_s_desc, ref()).WillByDefault([this] {
			return Ref_r{
				m_s.data(),
				[](void*) {},
				Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), m_s.size()),
				true,
				true
			};
		});
	}

	void TearDown() override
	{
		current_path(m_cwd);
		remove_all(m_dir);
		LoggingTest::TearDown();
	}

	/** A configuration in error, that configures nothing
	 * \return the configuration
	 */
	PC_tree_t no_config() { return PC_get(config("{}"), ".logging"); }

	/** The path of a log file of this test
	 * \param name the name of the file
	 * \return its path
	 */
	string log_file(const string& name) const { return (m_dir / name).string(); }

	/** What a log file of this test holds, to read once the loggers writing to it are destroyed
	 * \param name the name of the file
	 * \return its content, empty when it does not exist
	 */
	string log_content(const string& name) const
	{
		ifstream file(m_dir / name);
		return string(istreambuf_iterator<char>(file), {});
	}
};

/*
 * The configuration of the PDI logger
 */

TEST_F(LoggerTest, NoConfiguration)
{
	Inspectable<Pdi_logger> pdi(no_config());
	EXPECT_EQ(pdi.level(), Level::info);
	EXPECT_THAT(pdi.output_kinds(), ElementsAre("stdout"));
}

TEST_F(LoggerTest, DefaultPatternAndLevel)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.debug("hidden");
	pdi.info("msg");
	EXPECT_THAT(written_lines(), ElementsAre(MatchesRegex(default_pattern_regex("", "PDI"))));
}

TEST_F(LoggerTest, LevelAsScalar)
{
	Inspectable<Pdi_logger> pdi(config("debug"));
	EXPECT_EQ(pdi.level(), Level::debug);
}

TEST_F(LoggerTest, LevelInMapping)
{
	Pdi_logger pdi(config("{level: warn, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("hidden");
	pdi.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, InvalidLevelThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{level: verbose}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`verbose'")));
}

TEST_F(LoggerTest, UnknownKeyThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{levle: info}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`levle'")));
}

TEST_F(LoggerTest, SequenceThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("[info]")); }, ThrowsMessage<Spectree_error>(HasSubstr("sequence")));
}

TEST_F(LoggerTest, CoreSequenceThrows)
{
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{core: [info]}")); },
		ThrowsMessage<Spectree_error>(AllOf(HasSubstr("logger of the core"), HasSubstr("sequence")))
	);
}

TEST_F(LoggerTest, LogAtLevel)
{
	Pdi_logger pdi(config("{level: warn, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.log(Level::info, "hidden");
	pdi.log(Level::warn, "shown");
	EXPECT_EQ(written(), "shown\n");
}

/// The tests of the level names: a name, and the messages a logger at that level lets through
struct LoggerLevelTest
	: public LoggerTest
	, public WithParamInterface<pair<const char*, const char*>> {};

TEST_P(LoggerLevelTest, LevelName)
{
	Pdi_logger pdi(config(string("{level: ") + GetParam().first + ", pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.trace("trace");
	pdi.debug("debug");
	pdi.info("info");
	pdi.warn("warn");
	pdi.error("error");
	pdi.log(Level::critical, "critical");
	EXPECT_EQ(written(), GetParam().second);
}

INSTANTIATE_TEST_SUITE_P(
	,
	LoggerLevelTest,
	Values(
		pair{"trace", "trace\ndebug\ninfo\nwarn\nerror\ncritical\n"},
		pair{"TRACE", "trace\ndebug\ninfo\nwarn\nerror\ncritical\n"},
		pair{"debug", "debug\ninfo\nwarn\nerror\ncritical\n"},
		pair{"info", "info\nwarn\nerror\ncritical\n"},
		pair{"Info", "info\nwarn\nerror\ncritical\n"},
		pair{"warn", "warn\nerror\ncritical\n"},
		pair{"warning", "warn\nerror\ncritical\n"},
		pair{"WARNING", "warn\nerror\ncritical\n"},
		pair{"err", "error\ncritical\n"},
		pair{"error", "error\ncritical\n"},
		pair{"critical", "critical\n"},
		pair{"off", ""},
		pair{"OFF", ""}
	)
);

TEST_F(LoggerTest, DefaultLevel)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.default_level("warn");
	pdi.info("hidden");
	pdi.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, InvalidDefaultLevelThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { pdi.default_level("verbose"); }, ThrowsMessage<Value_error>(HasSubstr("`verbose'")));
}

/*
 * Patterns
 */

TEST_F(LoggerTest, PatternFromConfiguration)
{
	Pdi_logger pdi(config("{pattern: '%n|%l|%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("msg");
	EXPECT_EQ(written(), "PDI|info|msg\n");
}

TEST_F(LoggerTest, PatternAfterOutputs)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream}}, pattern: '%v'}"));
	use_streams(pdi);
	pdi.info("msg");
	EXPECT_EQ(written(), "msg\n");
}

TEST_F(LoggerTest, PatternEndingWithPercentThrows)
{
	EXPECT_THROW({ Pdi_logger pdi(config("{pattern: '%v %'}")); }, Value_error);
}

TEST_F(LoggerTest, PatternEscapedPercent)
{
	Pdi_logger pdi(config("{pattern: '%%{ %%%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("msg");
	EXPECT_EQ(written(), "%{ %msg\n");
}

TEST_F(LoggerTest, PatternColorFlags)
{
	Pdi_logger pdi(config("{pattern: '%^%l%$: %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("msg");
	EXPECT_EQ(written(), "info: msg\n");
}

TEST_F(LoggerTest, PatternDollarAndBackslash)
{
	Pdi_logger pdi(config(R"({pattern: '$1 \ %v', outputs: {o: {kind: stream}}})"));
	use_streams(pdi);
	pdi.info("msg");
	EXPECT_EQ(written(), "$1 \\ msg\n");
}

TEST_F(LoggerTest, PatternReference)
{
	m_x = 42;
	Pdi_logger pdi(config("{pattern: '[%{x}] %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("before");
	pdi.evaluate_patterns(m_ctx);
	pdi.info("after");
	EXPECT_EQ(written(), "[] before\n[42] after\n");
}

TEST_F(LoggerTest, PatternFormattedReference)
{
	m_x = 7;
	Pdi_logger pdi(config("{pattern: '[%{x:03d}] %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("before");
	pdi.evaluate_patterns(m_ctx);
	pdi.info("after");
	EXPECT_EQ(written(), "[] before\n[007] after\n");
}

TEST_F(LoggerTest, PatternStringReference)
{
	m_s = "abc";
	Pdi_logger pdi(config("{pattern: '[%{s}] %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written(), "[abc] msg\n");
}

TEST_F(LoggerTest, PatternReferenceToPercentBrace)
{
	m_s = "%{s}";
	Pdi_logger pdi(config("{pattern: '[%{s}] %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written(), "[%{s}] msg\n");
}

TEST_F(LoggerTest, PatternReferenceToPercent)
{
	m_s = "50%n";
	Pdi_logger pdi(config("{pattern: '[%{s}] %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written(), "[50%n] msg\n");
}

TEST_F(LoggerTest, EvaluatePatternsTwiceThrows)
{
	Pdi_logger pdi(config("{pattern: '[%{x}] %v'}"));
	pdi.evaluate_patterns(m_ctx);
	EXPECT_THROW(pdi.evaluate_patterns(m_ctx), State_error);
}

TEST_F(LoggerTest, DefaultPattern)
{
	Pdi_logger pdi(config("{pattern: 'A %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	pdi.info("before");
	pdi.default_pattern("B %v", m_ctx);
	pdi.info("after");
	EXPECT_EQ(written(), "A before\nB after\n");
}

TEST_F(LoggerTest, DefaultPatternReference)
{
	m_x = 3;
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	pdi.default_pattern("[%{x}] %v", m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written(), "[3] msg\n");
}

TEST_F(LoggerTest, DefaultPatternSparesOutputWithItsOwnPattern)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream, pattern: 'O %v'}, p: {kind: stream}}}"));
	use_streams(pdi, {"o", "p"});
	pdi.evaluate_patterns(m_ctx);
	pdi.default_pattern("D %v", m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written("o"), "O msg\n");
	EXPECT_EQ(written("p"), "D msg\n");
}

TEST_F(LoggerTest, DefaultPatternEndingWithPercentThrows)
{
	Pdi_logger pdi(no_config());
	pdi.evaluate_patterns(m_ctx);
	EXPECT_THROW(pdi.default_pattern("%v %", m_ctx), Value_error);
}

TEST_F(LoggerTest, DefaultPatternBeforeEvaluation)
{
	m_x = 3;
	Pdi_logger pdi(config("{pattern: 'A %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.default_pattern("[%{x}] %v", m_ctx);
	pdi.info("before");
	pdi.evaluate_patterns(m_ctx);
	pdi.info("after");
	EXPECT_EQ(written(), "[] before\n[3] after\n");
}

TEST_F(LoggerTest, DefaultPatternBeforeEvaluationEndingWithPercentThrows)
{
	Pdi_logger pdi(config("{pattern: 'A %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	EXPECT_THROW(pdi.default_pattern("%v %", m_ctx), Value_error);
	pdi.evaluate_patterns(m_ctx);
	pdi.info("msg");
	EXPECT_EQ(written(), "A msg\n");
}

/*
 * Blocks of the default pattern
 */

TEST_F(LoggerTest, DefaultPatternBlocks)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.add_default_pattern_block("B1");
	pdi.add_default_pattern_block("B2");
	pdi.info("msg");
	EXPECT_THAT(written_lines(), ElementsAre(MatchesRegex(default_pattern_regex(R"(\[B1\]\[B2\])", "PDI"))));
}

TEST_F(LoggerTest, BlocksAfterEvaluationThrow)
{
	Pdi_logger pdi(no_config());
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.evaluate_patterns(m_ctx);
	EXPECT_THROW(pdi.add_default_pattern_block("B"), State_error);
	EXPECT_THROW(plugin.add_default_pattern_block("B"), State_error);
}

TEST_F(LoggerTest, BlocksSpareConfiguredPattern)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.add_default_pattern_block("B");
	pdi.info("msg");
	EXPECT_EQ(written(), "msg\n");
}

TEST_F(LoggerTest, BlocksSpareOutputWithItsOwnPattern)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream, pattern: 'O %v'}}}"));
	use_streams(pdi);
	pdi.add_default_pattern_block("B");
	pdi.info("msg");
	EXPECT_EQ(written(), "O msg\n");
}

TEST_F(LoggerTest, BlocksThroughPluginLogger)
{
	Pdi_logger pdi(config("{outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	plugin.add_default_pattern_block("B");
	pdi.info("msg");
	plugin.info("msg");
	EXPECT_THAT(
		written_lines(),
		ElementsAre(MatchesRegex(default_pattern_regex(R"(\[B\])", "PDI")), MatchesRegex(default_pattern_regex(R"(\[B\])", "PDI/plugin")))
	);
}

/*
 * Plugin loggers
 */

TEST_F(LoggerTest, PluginLoggerName)
{
	Pdi_logger pdi(config("{pattern: '%n', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	plugin.info("msg");
	EXPECT_EQ(written(), "PDI/plugin\n");
}

TEST_F(LoggerTest, PluginLoggerFollowsDefaultLevel)
{
	Pdi_logger pdi(config("{level: warn, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	plugin.info("hidden");
	plugin.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, DefaultLevelReachesPluginLoggers)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.default_level("warn");
	plugin.info("hidden");
	plugin.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, PluginLevelAsScalar)
{
	Pdi_logger pdi(config("{level: debug, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", config("warn"));
	pdi.info("pdi");
	plugin.info("hidden");
	plugin.warn("plugin");
	EXPECT_EQ(written(), "pdi\nplugin\n");
}

TEST_F(LoggerTest, PluginLevelInMapping)
{
	Pdi_logger pdi(config("{level: debug, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", config("{level: warn}"));
	pdi.info("pdi");
	plugin.info("hidden");
	plugin.warn("plugin");
	EXPECT_EQ(written(), "pdi\nplugin\n");
}

TEST_F(LoggerTest, PluginLevelBelowDefaultLevel)
{
	Pdi_logger pdi(config("{level: warn, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", config("debug"));
	pdi.info("hidden");
	plugin.trace("hidden");
	plugin.debug("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, DefaultLevelSparesLoggersWithTheirOwnLevel)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", config("{level: debug}"));
	pdi.default_level("error");
	pdi.warn("hidden");
	plugin.debug("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, PluginInvalidLevelThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { Plugin_logger plugin(pdi, "plugin", config("{level: verbose}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`verbose'")));
}

TEST_F(LoggerTest, PluginUnknownKeyThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { Plugin_logger plugin(pdi, "plugin", config("{levle: info}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`levle'")));
}

TEST_F(LoggerTest, PluginPatternThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { Plugin_logger plugin(pdi, "plugin", config("{pattern: '%v'}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`pattern'")));
}

TEST_F(LoggerTest, PluginSequenceThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { Plugin_logger plugin(pdi, "plugin", config("[info]")); }, ThrowsMessage<Spectree_error>(HasSubstr("sequence")));
}

TEST_F(LoggerTest, InvalidPluginLoggerLeavesNoDanglingPointer)
{
	// where the plugin logger that fails lies, then a plugin logger of another infrastructure, that the first one must not reach
	alignas(Plugin_logger) unsigned char storage[sizeof(Plugin_logger)];
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	Pdi_logger other_pdi(config("{pattern: '%v', outputs: {p: {kind: stream}}}"));
	use_streams(other_pdi, {"p"});
	EXPECT_THROW((void)new (storage) Plugin_logger(pdi, "plugin", config("{level: verbose}")), Spectree_error);
	Plugin_logger* other = new (storage) Plugin_logger(other_pdi, "other", no_config());
	pdi.default_level("warn");
	other->info("shown");
	other->~Plugin_logger();
	EXPECT_EQ(written("p"), "shown\n");
}

TEST_F(LoggerTest, DestroyedPluginLoggerLeavesTheInfrastructure)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	optional<Plugin_logger> plugin;
	plugin.emplace(pdi, "plugin", no_config());
	plugin.reset();
	pdi.default_level("warn");
	pdi.add_default_pattern_block("B");
	pdi.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

TEST_F(LoggerTest, DefaultLevelThroughPluginLogger)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	plugin.default_level("warn");
	pdi.info("hidden");
	plugin.info("hidden");
	pdi.warn("pdi");
	plugin.warn("plugin");
	EXPECT_EQ(written(), "pdi\nplugin\n");
}

TEST_F(LoggerTest, DefaultPatternThroughPluginLogger)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.evaluate_patterns(m_ctx);
	Plugin_logger plugin(pdi, "plugin", no_config());
	plugin.default_pattern("[%n] %v", m_ctx);
	pdi.info("pdi");
	plugin.info("plugin");
	EXPECT_EQ(written(), "[PDI] pdi\n[PDI/plugin] plugin\n");
}

#pragma GCC diagnostic pop

/*
 * Outputs of kind `stream`
 */

TEST_F(LoggerTest, StreamWritesNothingUntilSet)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream}}}"));
	pdi.info("hidden");
	use_streams(pdi);
	pdi.info("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, StreamReachesExistingPluginLoggers)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}}}"));
	Plugin_logger first(pdi, "first", no_config());
	Plugin_logger second(pdi, "second", no_config());
	use_streams(pdi);
	Plugin_logger third(pdi, "third", no_config());
	pdi.info("msg");
	first.info("msg");
	second.info("msg");
	third.info("msg");
	EXPECT_EQ(written(), "PDI msg\nPDI/first msg\nPDI/second msg\nPDI/third msg\n");
}

TEST_F(LoggerTest, StreamSetAgain)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}}}"));
	Plugin_logger plugin(pdi, "plugin", no_config());
	use_streams(pdi);
	plugin.info("first");
	pdi.set_output_stream("o", m_streams["again"]);
	plugin.info("second");
	EXPECT_EQ(written(), "PDI/plugin first\n");
	EXPECT_EQ(written("again"), "PDI/plugin second\n");
}

TEST_F(LoggerTest, StreamSparesLoggersThatDisableIt)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}, p: {kind: stream}}}"));
	Plugin_logger plugin(pdi, "plugin", config("{disabled_outputs: [o]}"));
	use_streams(pdi, {"o", "p"});
	pdi.info("a");
	plugin.info("b");
	EXPECT_EQ(written("o"), "PDI a\n");
	EXPECT_EQ(written("p"), "PDI a\nPDI/plugin b\n");
}

TEST_F(LoggerTest, StreamLevelAndPattern)
{
	Pdi_logger pdi(config("{pattern: 'D %v', outputs: {o: {kind: stream, level: warn, pattern: 'O %v'}}}"));
	use_streams(pdi);
	pdi.info("hidden");
	pdi.warn("shown");
	EXPECT_EQ(written(), "O shown\n");
}

TEST_F(LoggerTest, StreamSetAfterEvaluation)
{
	m_x = 3;
	Pdi_logger pdi(config("{pattern: '[%{x}] %v', outputs: {o: {kind: stream, pattern: '<%{x}> %v'}, p: {kind: stream}}}"));
	pdi.evaluate_patterns(m_ctx);
	use_streams(pdi, {"o", "p"});
	pdi.info("msg");
	EXPECT_EQ(written("o"), "<3> msg\n");
	EXPECT_EQ(written("p"), "[3] msg\n");
}

TEST_F(LoggerTest, StreamOfUnknownOutputThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { pdi.set_output_stream("nope", m_streams["nope"]); }, ThrowsMessage<Invalid_action_error>(HasSubstr("`nope'")));
}

TEST_F(LoggerTest, StreamOfOtherKindThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT([&] { pdi.set_output_stream("stdout", m_streams["stdout"]); }, ThrowsMessage<Invalid_action_error>(HasSubstr("`stdout'")));
}

/*
 * Outputs of the other kinds
 */

TEST_F(LoggerTest, OutputKinds)
{
	Inspectable<Pdi_logger> pdi(
		config("{outputs: {a: {kind: stdout}, b: {kind: stderr}, c: {kind: file, path: '" + log_file("pdi.log") + "'}, d: {kind: stream}}}")
	);
	EXPECT_THAT(pdi.output_kinds(), UnorderedElementsAre("stdout", "stderr", "file"));
	use_streams(pdi, {"d"});
	EXPECT_THAT(pdi.output_kinds(), UnorderedElementsAre("stdout", "stderr", "file", "stream"));
}

TEST_F(LoggerTest, OutputToFile)
{
	{
		Inspectable<Pdi_logger> pdi(config("{pattern: '%v', outputs: {f: {kind: file, path: '" + log_file("pdi.log") + "'}}}"));
		EXPECT_THAT(pdi.output_kinds(), ElementsAre("file"));
		pdi.info("msg");
	}
	EXPECT_EQ(log_content("pdi.log"), "msg\n");
}

TEST_F(LoggerTest, SeveralOutputs)
{
	{
		Pdi_logger pdi(
			config("{pattern: '%v', outputs: {f: {kind: file, path: '" + log_file("pdi.log") + "'}, o: {kind: stream}, p: {kind: stream}}}")
		);
		use_streams(pdi, {"o", "p"});
		pdi.info("msg");
	}
	EXPECT_EQ(written("o"), "msg\n");
	EXPECT_EQ(written("p"), "msg\n");
	EXPECT_EQ(log_content("pdi.log"), "msg\n");
}

TEST_F(LoggerTest, OutputLevel)
{
	Pdi_logger pdi(config("{pattern: '%v', outputs: {o: {kind: stream, level: warn}, p: {kind: stream}}}"));
	use_streams(pdi, {"o", "p"});
	pdi.info("info");
	pdi.warn("warn");
	EXPECT_EQ(written("o"), "warn\n");
	EXPECT_EQ(written("p"), "info\nwarn\n");
}

TEST_F(LoggerTest, OutputPattern)
{
	Pdi_logger pdi(config("{pattern: 'D %v', outputs: {o: {kind: stream, pattern: 'O %v'}, p: {kind: stream}}}"));
	use_streams(pdi, {"o", "p"});
	pdi.info("msg");
	EXPECT_EQ(written("o"), "O msg\n");
	EXPECT_EQ(written("p"), "D msg\n");
}

TEST_F(LoggerTest, OutputPatternReference)
{
	m_x = 3;
	Pdi_logger pdi(config("{outputs: {o: {kind: stream, pattern: '[%{x}] %v'}}}"));
	use_streams(pdi);
	pdi.info("before");
	pdi.evaluate_patterns(m_ctx);
	pdi.info("after");
	EXPECT_EQ(written(), "[] before\n[3] after\n");
}

TEST_F(LoggerTest, OutputPatternEndingWithPercentThrows)
{
	EXPECT_THROW({ Pdi_logger pdi(config("{outputs: {o: {kind: stream, pattern: '%v %'}}}")); }, Value_error);
}

TEST_F(LoggerTest, OutputDuplicateNameThrows)
{
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{outputs: {o: {kind: stdout}, o: {kind: stderr}}}")); },
		ThrowsMessage<Spectree_error>(HasSubstr("duplicate key `o'"))
	);
}

TEST_F(LoggerTest, OutputUnknownKindThrows)
{
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{outputs: {o: {kind: socket}}}")); },
		ThrowsMessage<Spectree_error>(AllOf(HasSubstr("output `o'"), HasSubstr("`socket'")))
	);
}

TEST_F(LoggerTest, OutputWithoutKindThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{outputs: {o: {level: info}}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("output `o'")));
}

TEST_F(LoggerTest, OutputUnknownKeyThrows)
{
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{outputs: {o: {kind: stdout, colour: red}}}")); },
		ThrowsMessage<Spectree_error>(AllOf(HasSubstr("`colour'"), HasSubstr("`kind', `level', `pattern'")))
	);
}

TEST_F(LoggerTest, OutputPathOnlyForFiles)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{outputs: {o: {kind: stdout, path: x}}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`path'")));
}

TEST_F(LoggerTest, FileOutputWithoutPathThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{outputs: {f: {kind: file}}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("output `f'")));
}

TEST_F(LoggerTest, PluginLoggerSharesOutputsInOrder)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.info("first");
	plugin.info("second");
	pdi.info("third");
	EXPECT_EQ(written(), "PDI first\nPDI/plugin second\nPDI third\n");
}

/*
 * The logger of the core
 */

TEST_F(LoggerTest, CoreLevel)
{
	Pdi_logger pdi(config("{core: {level: warn}, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.info("hidden");
	plugin.info("plugin");
	pdi.warn("pdi");
	EXPECT_EQ(written(), "plugin\npdi\n");
}

TEST_F(LoggerTest, CoreLevelBelowDefaultLevel)
{
	Pdi_logger pdi(config("{level: warn, core: debug, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.debug("pdi");
	plugin.info("hidden");
	EXPECT_EQ(written(), "pdi\n");
}

TEST_F(LoggerTest, CoreLevelAsScalar)
{
	Pdi_logger pdi(config("{core: warn, pattern: '%v', outputs: {o: {kind: stream}}}"));
	use_streams(pdi);
	pdi.info("hidden");
	pdi.warn("shown");
	EXPECT_EQ(written(), "shown\n");
}

TEST_F(LoggerTest, CoreDisabledOutputs)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}, p: {kind: stream}}, core: {disabled_outputs: [o]}}"));
	use_streams(pdi, {"o", "p"});
	Plugin_logger plugin(pdi, "plugin", no_config());
	pdi.info("a");
	plugin.info("b");
	EXPECT_EQ(written("o"), "PDI/plugin b\n");
	EXPECT_EQ(written("p"), "PDI a\nPDI/plugin b\n");
}

TEST_F(LoggerTest, PluginDisabledOutputs)
{
	Pdi_logger pdi(config("{pattern: '%n %v', outputs: {o: {kind: stream}, p: {kind: stream}}}"));
	use_streams(pdi, {"o", "p"});
	Plugin_logger plugin(pdi, "plugin", config("{disabled_outputs: [p]}"));
	pdi.info("a");
	plugin.info("b");
	EXPECT_EQ(written("o"), "PDI a\nPDI/plugin b\n");
	EXPECT_EQ(written("p"), "PDI a\n");
}

TEST_F(LoggerTest, CoreUnknownKeyThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{core: {foo: 1}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`foo'")));
}

TEST_F(LoggerTest, UnknownDisabledOutputThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{core: {disabled_outputs: [nope]}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`nope'")));
}

TEST_F(LoggerTest, PluginUnknownDisabledOutputThrows)
{
	Pdi_logger pdi(no_config());
	EXPECT_THAT(
		[&] { Plugin_logger plugin(pdi, "plugin", config("{disabled_outputs: [nope]}")); },
		ThrowsMessage<Spectree_error>(HasSubstr("`nope'"))
	);
}

/*
 * The deprecated output
 */

TEST_F(LoggerTest, DeprecatedOutputToFile)
{
	{
		Inspectable<Pdi_logger> pdi(config("{pattern: '%v', output: {file: '" + log_file("pdi.log") + "'}}"));
		EXPECT_THAT(pdi.output_kinds(), ElementsAre("file"));
		pdi.info("msg");
	}
	EXPECT_EQ(log_content("pdi.log"), "`logging.output' is deprecated in favor of the new `logging.outputs' (plural)\nmsg\n");
}

TEST_F(LoggerTest, DeprecatedOutputToFileAndConsole)
{
	Inspectable<Pdi_logger> pdi(config("{level: error, output: {file: '" + log_file("pdi.log") + "', console: on}}"));
	EXPECT_THAT(pdi.output_kinds(), UnorderedElementsAre("file", "stdout"));
}

TEST_F(LoggerTest, DeprecatedOutputWithoutFile)
{
	Inspectable<Pdi_logger> pdi(config("{level: error, output: {}}"));
	EXPECT_THAT(pdi.output_kinds(), ElementsAre("stdout"));
}

TEST_F(LoggerTest, DeprecatedOutputNames)
{
	{
		Inspectable<Pdi_logger> pdi(
			config("{pattern: '%n %v', output: {file: '" + log_file("pdi.log") + "', console: on}, core: {disabled_outputs: [stdout]}}")
		);
		Inspectable<Plugin_logger> plugin(pdi, "plugin", config("{disabled_outputs: [file]}"));
		EXPECT_THAT(pdi.output_kinds(), ElementsAre("file"));
		EXPECT_THAT(plugin.output_kinds(), ElementsAre("stdout"));
		pdi.info("a");
	}
	EXPECT_EQ(log_content("pdi.log"), "PDI `logging.output' is deprecated in favor of the new `logging.outputs' (plural)\nPDI a\n");
}

TEST_F(LoggerTest, DeprecatedOutputWithOutputsThrows)
{
	EXPECT_THROW({ Pdi_logger pdi(config("{output: {console: on}, outputs: {o: {kind: stdout}}}")); }, Spectree_error);
}

TEST_F(LoggerTest, DeprecatedConsoleNotABooleanThrows)
{
	EXPECT_THAT([&] { Pdi_logger pdi(config("{output: {console: maybe}}")); }, ThrowsMessage<Spectree_error>(HasSubstr("`maybe'")));
}

/// The tests of the spellings of false that turn the console off
struct LoggerConsoleOffTest
	: public LoggerTest
	, public WithParamInterface<const char*> {};

TEST_P(LoggerConsoleOffTest, ConsoleOff)
{
	Inspectable<Pdi_logger> pdi(config(string("{output: {console: ") + GetParam() + "}}"));
	EXPECT_THAT(pdi.output_kinds(), IsEmpty());
}

INSTANTIATE_TEST_SUITE_P(, LoggerConsoleOffTest, Values("off", "Off", "OFF", "false", "False", "FALSE", "no", "No", "NO", "n", "N"));

/*
 * Output files
 */

TEST_F(LoggerTest, OutputFileCreatesDirectories)
{
	{
		Pdi_logger pdi(config("{pattern: '%v', outputs: {f: {kind: file, path: '" + log_file("sub/dir/pdi.log") + "'}}}"));
		pdi.info("msg");
	}
	EXPECT_EQ(log_content("sub/dir/pdi.log"), "msg\n");
}

TEST_F(LoggerTest, OutputFileInWorkingDirectory)
{
	current_path(m_dir);
	{
		Pdi_logger pdi(config("{pattern: '%v', outputs: {f: {kind: file, path: pdi.log}}}"));
		pdi.info("msg");
	}
	EXPECT_EQ(log_content("pdi.log"), "msg\n");
}

TEST_F(LoggerTest, UnopenableOutputFileThrows)
{
	{
		Pdi_logger pdi(config("{outputs: {f: {kind: file, path: '" + log_file("file.log") + "'}}}"));
	}
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{outputs: {f: {kind: file, path: '" + log_file("file.log/pdi.log") + "'}}}")); },
		ThrowsMessage<spdlog_ex>(HasSubstr(log_file("file.log/pdi.log")))
	);
}

TEST_F(LoggerTest, OutputFileThatIsADirectoryThrows)
{
	create_directories(log_file("dir.log"));
	EXPECT_THAT(
		[&] { Pdi_logger pdi(config("{outputs: {f: {kind: file, path: '" + log_file("dir.log") + "'}}}")); },
		ThrowsMessage<spdlog_ex>(HasSubstr(generic_category().message(EISDIR)))
	);
}
