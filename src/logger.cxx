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

#include "pdi/logger.h"

namespace PDI {

static void read_defaul_level(PC_tree_t logging_tree)
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
			Logger::write()->set_level(level_map.find(level_str)->second);
		} else {
			Logger::write()->warn("Invalid logging level: {}. Available: 'debug', 'info', 'warn', 'error', 'off'.", level_str);
		}
	}
}

void Logger::configure_logger(PC_tree_t config)
{
	//set default settings
	write() = spdlog::stdout_color_mt("console");
	write()->set_level(spdlog::level::info);
	
	PC_tree_t logging_tree = PC_get(config, ".logging");
	if (PC_status(logging_tree)) {
		write()->debug("Didn't found logging tree");
		return;
	}
	
	//read a default log level
	read_defaul_level(logging_tree);
}

std::shared_ptr<spdlog::logger>& Logger::write()
{
	static std::shared_ptr<spdlog::logger> logger;
	return logger;
}

}