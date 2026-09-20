/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/logger.h>

#include "logger_operation.h"

namespace set_value {

using std::string;

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
		throw PDI::Spectree_error(evaluate_node, "the `evaluate' key is not supported, use `pattern' instead");
	}
}

void Logger_operation::execute(PDI::Logger& logger, PDI::Context& ctx)
{
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
	if (m_level) {
		string level_str = m_level.to_string(ctx);
		logger.info("Changing default logging level to `{}'", level_str);
		logger.default_level(std::move(level_str));
	}
	if (!m_pattern.empty()) {
		logger.default_pattern(m_pattern, ctx);
	}
#pragma GCC diagnostic pop
}

} // namespace set_value
