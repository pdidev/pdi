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

#include <mpi.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <spdlog/spdlog.h>
#include <spdlog/sinks/ansicolor_sink.h>
#include <spdlog/sinks/basic_file_sink.h>

#include "pdi/logger.h"

namespace {

using PDI::Logger_sptr;
using PDI::len;
using PDI::to_long;
using PDI::to_string;
using spdlog::logger;
using spdlog::level::level_enum;
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

void set_up_log_format(Logger_sptr logger)
{
	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	char format[64];
	snprintf(format, 64, "[PDI][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
	logger->set_pattern(string(format));
}

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

void read_log_level(Logger_sptr logger, PC_tree_t logging_tree)
{
	if (!PC_status(PC_get(logging_tree, ".level"))) {
		string level_str {to_string(PC_get(logging_tree, ".level"))};
		
		unordered_map<string, level_enum> level_map = {
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
			logger->warn("Invalid logging level: {}. Available: 'debug', 'info', 'warn', 'error', 'off'.", level_str);
		}
	}
}

void configure_single_rank(Logger_sptr logger, PC_tree_t logging_tree)
{
	PC_tree_t single_tree = PC_get(logging_tree, ".single");
	if (!PC_status(single_tree)) {
		int world_rank;
		MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
		
		int nb_key = len(single_tree);
		for (int key_id = 0; key_id < nb_key; ++key_id) {
			PC_tree_t rank_tree = PC_get(single_tree, "[%d]", key_id);
			int selected_rank = to_long(PC_get(rank_tree, ".rank"), -1);
			if (selected_rank == world_rank) {
				read_log_level(logger, rank_tree);
				break;
			}
		}
	}
}

} // namespace <anonymous>

namespace PDI {

Logger_sptr configure_logger(PC_tree_t config)
{
	PC_tree_t logging_tree = PC_get(config, ".logging");
	
	//select default sinks
	Logger_sptr logger = select_log_sinks(logging_tree);
	
	//read default log level
	read_log_level(logger, logging_tree);
	
	//configure log level of single ranks
	configure_single_rank(logger, logging_tree);
	
	//set up final format of logger
	set_up_log_format(logger);
	
	return logger;
}

}
