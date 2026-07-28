/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <unordered_map>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/ref_any.h>

#include "global_context.h"

#include "logger_operation.h"

namespace set_value {

Logger_operation::Logger_operation(PDI::Logger& logger, PC_tree_t logger_node)
{
	logger.debug("Logger operation:");
	PC_tree_t level_node = PC_get(logger_node, ".level");
	if (!PC_status(level_node)) {
		logger.debug("\tlevel");
		m_level = level_node;
	}

	PC_tree_t pattern_node = PC_get(logger_node, ".pattern");
	if (!PC_status(pattern_node)) {
		logger.debug("\tpattern");
		m_pattern = PDI::to_string(pattern_node);
	}

	PC_tree_t evaluate_node = PC_get(logger_node, ".evaluate");
	if (!PC_status(evaluate_node)) {
		logger.debug("\tevaluate");
		m_evaluate = evaluate_node;
	}
}

void Logger_operation::execute(PDI::Logger& logger, PDI::Context& ctx)
{
	try {
		PDI::Global_context& global_ctx = dynamic_cast<PDI::Global_context&>(ctx);
		if (m_level) {
			static const std::unordered_map<std::string, spdlog::level::level_enum> level_map
				= {{"trace", spdlog::level::level_enum::trace},
			       {"debug", spdlog::level::level_enum::debug},
			       {"info", spdlog::level::level_enum::info},
			       {"warn", spdlog::level::level_enum::warn},
			       {"error", spdlog::level::level_enum::err},
			       {"off", spdlog::level::level_enum::off}};
			std::string level_str = m_level.to_string(ctx);
			auto level_it = level_map.find(level_str);
			if (level_it != level_map.end()) {
				logger.warn("Changing level to {}", level_str);
				global_ctx.logger().level(level_map.find(level_str)->second);
			} else {
				logger.warn("Invalid logging level: {}. Available: 'trace', 'debug', 'info', 'warn', 'error', 'off'.", level_str);
			}
		}
		if (!m_pattern.empty()) {
			logger.global_pattern(m_pattern);
			logger.evaluate_global_pattern(ctx);
		}
		if (m_evaluate.to_long(ctx)) {
			logger.evaluate_global_pattern(ctx);
		}

	} catch (std::bad_cast&) {
		logger.warn("Cannot cast Context to Global_context");
	}
}

} // namespace set_value
