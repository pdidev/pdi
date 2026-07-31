/*******************************************************************************
 * Copyright (C) 2018-2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <unordered_map>
#include <vector>

#include <spdlog/sinks/ansicolor_sink.h>
#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>

#include "pdi/logger.h"

namespace {

using PDI::Context;
using PDI::Error;
using PDI::Expression;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using spdlog::logger;
using spdlog::sink_ptr;
using spdlog::level::debug;
using spdlog::level::err;
using spdlog::level::info;
using spdlog::level::level_enum;
using spdlog::level::off;
using spdlog::level::trace;
using spdlog::level::warn;
using spdlog::sinks::basic_file_sink_st;
#if defined _WIN32 && !defined(__cplusplus_winrt)
using spdlog::sinks::wincolor_stdout_sink_st;
#else
using spdlog::sinks::ansicolor_stdout_sink_st;
#endif
using std::make_shared;
using std::shared_ptr;
using std::string;
using std::stringstream;
using std::unordered_map;
using std::vector;

/**
 * Creates logger with sinks defined in config
 *
 * \param[in] logging_tree configuration tree from config file
 * 
 * \return created logger
 */
shared_ptr<logger> select_log_sinks(const string& logger_name, PC_tree_t logging_tree)
{
	vector<sink_ptr> sinks;
	PC_tree_t output_tree = PC_get(logging_tree, ".output");

	//configure file sink
	if (!PC_status(PC_get(output_tree, ".file"))) {
		string filename{to_string(PC_get(output_tree, ".file"))};
		auto file_sink = make_shared<basic_file_sink_st>(filename);
		sinks.emplace_back(file_sink);
	}

	//configure console sink
	if ((!PC_status(PC_get(output_tree, ".console")) || sinks.empty()) // either there is a console sink specified or no other
	    && to_string(PC_get(output_tree, ".console"), "on") != "off" // the console sink is not specifically disabled
	)
	{
		//logging to console is turned on
#if defined _WIN32 && !defined(__cplusplus_winrt)
		sinks.push_back(make_shared<wincolor_stdout_sink_st>());
#else
		sinks.push_back(make_shared<ansicolor_stdout_sink_st>());
#endif
	}

	return make_shared<logger>(logger_name, sinks.begin(), sinks.end());
}

/**
 * Sets logger verbosity level from PC_tree
 *
 * \param[in] logger logger to set up
 * \param[in] logging_tree configuration tree from config file
 */
void read_log_level(shared_ptr<logger> logger, PC_tree_t logging_tree)
{
	string level_str;

	PC_tree_t level_tree = PC_get(logging_tree, ".level");
	if (!PC_status(level_tree)) {
		level_str = to_string(level_tree);
	} else {
		try {
			level_str = to_string(logging_tree);
		} catch (const Error& e) {
			// level is not defined
			return;
		}
	}

	const unordered_map<string, level_enum> level_map
		= {{"trace", trace}, {"debug", debug}, {"info", info}, {"warn", warn}, {"error", err}, {"off", off}};
	auto level_it = level_map.find(level_str);
	if (level_it != level_map.end()) {
		logger->set_level(level_map.find(level_str)->second);
	} else {
		logger->warn("Invalid logging level: {}. Available: 'trace', 'debug', 'info', 'warn', 'error', 'off'.", level_str);
	}
}

std::string evaluate_refs_in_pattern(string pattern)
{
	for (size_t start_pos = pattern.find("%{"); start_pos != string::npos; start_pos = pattern.find("%{")) {
		// transform `%{ref}' to `${ref}'
		pattern[start_pos] = '$';
		std::pair<Expression, long> parse_result = Expression::parse_reference(pattern.substr(start_pos).c_str());

		// don't have context to evaluate -> remove whole expression
		pattern = pattern.substr(0, start_pos) + pattern.substr(start_pos + parse_result.second);
	}
	return pattern;
}

std::string evaluate_refs_in_pattern(PDI::Context& ctx, string pattern)
{
	for (size_t start_pos = pattern.find("%{"); start_pos != string::npos; start_pos = pattern.find("%{")) {
		// transform `%{ref}' to `${ref}'
		pattern[start_pos] = '$';
		std::pair<Expression, long> parse_result = Expression::parse_reference(pattern.substr(start_pos).c_str());
		pattern = pattern.substr(0, start_pos) + parse_result.first.to_string(ctx) + pattern.substr(start_pos + parse_result.second);
	}
	return pattern;
}

} // namespace

namespace PDI {

void Logger::build_pattern()
{
	stringstream s_pattern;
	for (auto&& plugin_block: m_pattern_blocks) {
		s_pattern << plugin_block;
	}
	s_pattern << "[%n] *** %^%l%$: %v";
	m_pattern = s_pattern.str();
}

Logger::Logger(const string& logger_name, PC_tree_t config, level_enum level)
{
	setup(logger_name, config, level);
}

Logger::Logger(Logger& parent_logger, const std::string& logger_name, PC_tree_t config)
	: Logger(logger_name, config, parent_logger.level())
{
	m_parent_logger = &parent_logger;
	parent_logger.m_default_pattern_observers.emplace_back(*this);
	default_pattern(parent_logger.pattern());
}

void Logger::setup(const string& logger_name, PC_tree_t config, level_enum level)
{
	m_logger = select_log_sinks(logger_name, config);
	m_logger->set_level(level);

	// try to read log level from yaml
	read_log_level(m_logger, config);

	add_pattern_block("%T");

	// overwrite pattern if defined in yaml
	PC_tree_t pattern_tree = PC_get(config, ".pattern");
	if (!PC_status(pattern_tree)) {
		m_pattern = to_string(pattern_tree);
		for (auto&& observer: m_default_pattern_observers) {
			observer.get().default_pattern(m_pattern);
		}
		m_pattern_from_config = true;
	}
	m_logger->set_pattern(evaluate_refs_in_pattern(m_pattern));
}

void Logger::setup(Logger& parent_logger, const string& logger_name, PC_tree_t config)
{
	m_parent_logger = &parent_logger;
	setup(logger_name, config, parent_logger.level());
	parent_logger.m_default_pattern_observers.emplace_back(*this);
	default_pattern(parent_logger.pattern());
}

void Logger::pattern(const string& pattern_str)
{
	m_pattern = pattern_str;
	m_logger->set_pattern(m_pattern);
	for (auto&& observer: m_default_pattern_observers) {
		observer.get().default_pattern(m_pattern);
	}
	m_logger->set_pattern(evaluate_refs_in_pattern(m_pattern));
}

void Logger::global_pattern(const string& pattern_str)
{
	if (m_parent_logger != nullptr) {
		m_parent_logger->global_pattern(pattern_str);
	} else {
		pattern(pattern_str);
	}
}

void Logger::default_pattern(const string& pattern)
{
	if (!m_pattern_from_config) {
		m_pattern = pattern;
		m_logger->set_pattern(evaluate_refs_in_pattern(m_pattern));
		for (auto&& observer: m_default_pattern_observers) {
			observer.get().default_pattern(m_pattern);
		}
	}
}

void Logger::add_pattern_block(const string& block)
{
	m_pattern_blocks.push_back("[" + block + "]");
	build_pattern();
	default_pattern(m_pattern);
}

void Logger::add_pattern_global_block(const string& block)
{
	if (m_parent_logger != nullptr) {
		m_parent_logger->add_pattern_global_block(block);

	} else {
		add_pattern_block(block);
	}
}

const string& Logger::pattern() const
{
	return m_pattern;
}

void Logger::level(level_enum log_level)
{
	m_logger->set_level(log_level);
}

level_enum Logger::level() const
{
	return m_logger->level();
}

void Logger::evaluate_pattern(Context& ctx) const
{
	m_logger->set_pattern(evaluate_refs_in_pattern(ctx, m_pattern));
	for (auto&& observer: m_default_pattern_observers) {
		observer.get().evaluate_pattern(ctx);
	}
}

void Logger::evaluate_global_pattern(Context& ctx) const
{
	if (m_parent_logger != nullptr) {
		m_parent_logger->evaluate_global_pattern(ctx);
	} else {
		evaluate_pattern(ctx);
	}
}

std::shared_ptr<spdlog::logger> Logger::real_logger()
{
	return m_logger;
}

} // namespace PDI
