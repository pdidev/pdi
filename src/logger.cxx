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

#include "pdi/logger.h"

namespace PDI {

static void set_up_log_format(Logger& logger)
{
	int world_rank;
	MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
	char format[64];
	snprintf(format, 64, "[PDI][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
	logger->set_pattern(std::string(format));
}

static void select_log_sinks(PC_tree_t logging_tree, Logger& logger)
{
	PC_tree_t output_tree = PC_get(logging_tree, ".output");
	//check if there is output_tree and is not empty
	if (!PC_status(output_tree)) {
		std::vector<spdlog::sink_ptr> sinks;
		
		//configure file sink
		if (!PC_status(PC_get(output_tree, ".file"))) {
			std::string filename {PDI::to_string(PC_get(output_tree, ".file"))};
			auto file_sink = std::make_shared<spdlog::sinks::simple_file_sink_st>(filename);
			sinks.push_back(file_sink);
		}
		
		//configure console sink
		if (!PC_status(PC_get(output_tree, ".console"))) {
			std::string console_switch {PDI::to_string(PC_get(output_tree, ".console"))};
			if (console_switch.compare("off")) {
				//logging to console is turned on
				if (sinks.size() > 0) {
#if defined _WIN32 && !defined(__cplusplus_winrt)
					auto console_sink = std::make_shared<spdlog::sinks::wincolor_stdout_sink_st>();
#else
					auto console_sink = std::make_shared<spdlog::sinks::ansicolor_stdout_sink_st>();
#endif
					sinks.push_back(console_sink);
				} else {
					//if console is turned on and no output file
					logger = spdlog::stdout_color_st("logger");
					return;
				}
			}
		}
		
		if (sinks.size() > 0) {
			logger = std::make_shared<spdlog::logger>("logger", begin(sinks), end(sinks));
		}
	} else {
		//if no output tree set console
		logger = spdlog::stdout_color_st("logger");
	}
}


static void read_log_level(PC_tree_t logging_tree, Logger& logger)
{
	if (!PC_status(PC_get(logging_tree, ".level"))) {
		std::string level_str {PDI::to_string(PC_get(logging_tree, ".level"))};
		
		std::unordered_map<std::string, spdlog::level::level_enum> level_map = {
			{"debug", spdlog::level::debug},
			{"info", spdlog::level::info},
			{"warn", spdlog::level::warn},
			{"error", spdlog::level::err},
			{"off", spdlog::level::off}
		};
		
		auto level_it = level_map.find(level_str);
		if (level_it != level_map.end()) {
			logger->set_level(level_map.find(level_str)->second);
		} else {
			logger->warn("Invalid logging level: {}. Available: 'debug', 'info', 'warn', 'error', 'off'.", level_str);
		}
	}
}

static void configure_single_rank(PC_tree_t logging_tree, Logger& logger)
{
	PC_tree_t single_tree = PC_get(logging_tree, ".single");
	if (!PC_status(single_tree)) {
		int world_rank;
		MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
		
		int nb_key = len(single_tree);
		for (int key_id = 0; key_id < nb_key; ++key_id) {
			PC_tree_t rank_tree = PC_get(single_tree, "[%d]", key_id);
			int selected_rank = PDI::to_long(PC_get(rank_tree, ".rank"), -1);
			if (selected_rank == world_rank) {
				read_log_level(rank_tree, logger);
				break;
			}
		}
	}
}

void configure_logger(Logger& logger, PC_tree_t config)
{
	PC_tree_t logging_tree = PC_get(config, ".logging");
	if (PC_status(logging_tree)) {
		//didn't found logging tree, set default
		logger = spdlog::stdout_color_st("logger");
		set_up_log_format(logger);
		return;
	}
	//select default sinks
	select_log_sinks(logging_tree, logger);
	
	//read default log level
	read_log_level(logging_tree, logger);
	
	//configure log level of single ranks
	configure_single_rank(logging_tree, logger);
	
	//set up final format of logger
	set_up_log_format(logger);
}

}
