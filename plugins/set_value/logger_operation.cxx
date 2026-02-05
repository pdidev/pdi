// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <string>
#include <unordered_map>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>
#include <pdi/ref_any.h>

#include "logger_operation.h"

namespace set_value {

Logger_operation::Logger_operation(PDI::Context& ctx, PC_tree_t logger_node)
	: Operation{ctx}
{
	context().logger().debug("Logger operation:");
	PC_tree_t level_node = PC_get(logger_node, ".level");
	if (!PC_status(level_node)) {
		context().logger().debug("\tlevel");
		m_level = level_node;
	}

	PC_tree_t pattern_node = PC_get(logger_node, ".pattern");
	if (!PC_status(pattern_node)) {
		context().logger().debug("\tpattern");
		m_pattern = PDI::to_string(pattern_node);
	}

	PC_tree_t evaluate_node = PC_get(logger_node, ".evaluate");
	if (!PC_status(evaluate_node)) {
		context().logger().debug("\tevaluate");
		m_evaluate = static_cast<bool>(PDI::Expression(evaluate_node).to_long(context()));
	}
}

void Logger_operation::execute()
{
	try {
		PDI::Context_proxy& ctx_proxy = dynamic_cast<PDI::Context_proxy&>(context());
		if (m_level) {
			static const std::unordered_map<std::string, spdlog::level::level_enum> level_map
				= {{"trace", spdlog::level::level_enum::trace},
			       {"debug", spdlog::level::level_enum::debug},
			       {"info", spdlog::level::level_enum::info},
			       {"warn", spdlog::level::level_enum::warn},
			       {"error", spdlog::level::level_enum::err},
			       {"off", spdlog::level::level_enum::off}};
			std::string level_str = m_level.to_string(context());
			auto level_it = level_map.find(level_str);
			if (level_it != level_map.end()) {
				context().logger().warn("Changing level to {}", level_str);
				ctx_proxy.pdi_core_logger().level(level_map.find(level_str)->second);
			} else {
				context().logger().warn("Invalid logging level: {}. Available: 'trace', 'debug', 'info', 'warn', 'error', 'off'.", level_str);
			}
		}
		if (!m_pattern.empty()) {
			context().logger().global_pattern(m_pattern);
			context().logger().evaluate_global_pattern(context());
		}
		if (m_evaluate) {
			context().logger().evaluate_global_pattern(context());
		}

	} catch (std::bad_cast&) {
		context().logger().warn("Cannot cast Context to Context_proxy");
	}
}

} // namespace set_value
