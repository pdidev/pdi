/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <spdlog/spdlog.h>
#include <spdlog/sinks/ansicolor_sink.h>
#include <spdlog/sinks/basic_file_sink.h>

#include <pdi/error.h>

#include "pdi/logger.h"

namespace {

using PDI::Logger_sptr;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using spdlog::logger;
using spdlog::level::level_enum;
using spdlog::level::trace;
using spdlog::level::debug;
using spdlog::level::info;
using spdlog::level::warn;
using spdlog::level::err;
using spdlog::level::off;
using spdlog::sink_ptr;
using spdlog::sinks::basic_file_sink_st;
#if defined _WIN32 && !defined(__cplusplus_winrt)
	using spdlog::sinks::wincolor_stdout_sink_st;
#else
	using spdlog::sinks::ansicolor_stdout_sink_st;
#endif
using std::make_shared;
using std::string;
using std::unordered_map;
using std::vector;


Logger_sptr select_log_sinks(PC_tree_t logging_tree)
{
	vector<sink_ptr> sinks;
	PC_tree_t output_tree = PC_get(logging_tree, ".output");
	
	//configure file sink
	if (!PC_status(PC_get(output_tree, ".file"))) {
		string filename {to_string(PC_get(output_tree, ".file"))};
		auto file_sink = make_shared<basic_file_sink_st>(filename);
		sinks.emplace_back(file_sink);
	}
	
	//configure console sink
	if (
	    (!PC_status(PC_get(output_tree, ".console")) || sinks.empty() ) // either there is a console sink specified or no other
	    && to_string(PC_get(output_tree, ".console"), "on") != "off" // the console sink is not specifically disabled
	) {
		//logging to console is turned on
#if defined _WIN32 && !defined(__cplusplus_winrt)
		sinks.push_back(make_shared<wincolor_stdout_sink_st>());
#else
		sinks.push_back(make_shared<ansicolor_stdout_sink_st>());
#endif
	}
	
	return make_shared<logger>("PDI_logger", sinks.begin(), sinks.end());
}

/**
 * Sets logger verbosity level from PC_tree
 *
 * \param[in] logger logger to set up
 * \param[in] logging_tree configuration tree from config file
 * \param[in] name name
 */
void read_log_level(Logger_sptr logger, PC_tree_t config, const string& name)
{
	string level_str = "info";
	PC_tree_t level_tree = PC_get(config, ".level");
	
	if (!PC_status(PC_get(level_tree, "{0}"))) {
		PC_tree_t module_level = PC_get(level_tree, ".%s", name.c_str());
		if (!PC_status(module_level)) {
			level_str = to_string(module_level);
		} else {
			PC_tree_t global_level = PC_get(level_tree, ".global");
			if (!PC_status(global_level)) {
				level_str = to_string(global_level);
			}
		}
	} else {
		if (!PC_status(level_tree)) {
			level_str = to_string(level_tree);
		} else {
			try {
				level_str = to_string(config);
			} catch (const PDI::Error& e) {
				if (e.status() != PDI_ERR_CONFIG) {
					throw;
				}
			}
		}
	}
	
	const unordered_map<string, level_enum> level_map = {
		{"trace", trace},
		{"debug", debug},
		{"info", info},
		{"warn", warn},
		{"error", err},
		{"off", off}
	};
	auto level_it = level_map.find(level_str);
	if (level_it != level_map.end()) {
		logger->set_level(level_map.find(level_str)->second);
	} else {
		logger->warn("Invalid logging level: {}. Available: 'trace', 'debug', 'info', 'warn', 'error', 'off'.", level_str);
	}
}

} // namespace <anonymous>

namespace PDI {

Logger_sptr configure_logger(PC_tree_t config, const string& name)
{
	//select default sinks
	Logger_sptr logger = select_log_sinks(config);
	
	//read default log level for logger
	read_log_level(logger, config, name);
	
	//set up default format of logger
	logger->set_pattern("[PDI][%T] *** %^%l%$: %v");
	
	return logger;
}

}
