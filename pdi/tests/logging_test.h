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

#ifndef PDI_LOGGING_TEST_H_
#define PDI_LOGGING_TEST_H_

#include <initializer_list>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include <gtest/gtest.h>
#include <paraconf.h>

#include "pdi_logger.h"

/// The default pattern of the PDI loggers, as a regex, with the given blocks and logger name, for a message `msg` at info level
inline std::string default_pattern_regex(const std::string& blocks, const std::string& name)
{
	return R"(\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\])" + blocks + R"(\[)" + name + R"(\] \*\*\* info: msg)";
}

/** A test of the logging infrastructure, that builds its configurations and reads what the loggers write to its outputs of kind `stream`
 */
struct LoggingTest: public ::testing::Test {
	/// The paraconf error handler to restore
	PC_errhandler_t m_previous_handler;

	/// The configuration trees to destroy
	std::vector<PC_tree_t> m_trees;

	/// The streams the outputs of kind `stream` write to, by name of output
	std::map<std::string, std::ostringstream> m_streams;

	void SetUp() override
	{
		// a logging configuration usually misses most of the keys the loggers look for
		m_previous_handler = PC_errhandler(PC_NULL_HANDLER);
	}

	void TearDown() override
	{
		for (auto&& tree: m_trees) {
			PC_tree_destroy(&tree);
		}
		PC_errhandler(m_previous_handler);
	}

	/** Parse a configuration that lives as long as the test
	 * \param yaml the configuration
	 * \return the parsed configuration
	 */
	PC_tree_t config(const std::string& yaml)
	{
		m_trees.push_back(PC_parse_string(yaml.c_str()));
		return m_trees.back();
	}

	/** Make outputs of kind `stream` of a PDI logger write to the streams of this test
	 * \param pdi the PDI logger
	 * \param names the names of the outputs
	 */
	void use_streams(PDI::Pdi_logger& pdi, std::initializer_list<const char*> names = {"o"})
	{
		for (auto&& name: names) {
			pdi.set_output_stream(name, m_streams[name]);
		}
	}

	/** What the loggers wrote to an output of kind `stream`
	 * \param name the name of the output
	 * \return what they wrote
	 */
	std::string written(const std::string& name = "o") { return m_streams[name].str(); }

	/** What the loggers wrote to an output of kind `stream`, line by line
	 * \param name the name of the output
	 * \return what they wrote, line by line
	 */
	std::vector<std::string> written_lines(const std::string& name = "o")
	{
		std::vector<std::string> result;
		std::istringstream text(written(name));
		for (std::string line; std::getline(text, line);) {
			result.push_back(line);
		}
		return result;
	}
};

#endif // PDI_LOGGING_TEST_H_
