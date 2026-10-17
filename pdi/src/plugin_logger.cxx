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

#include "config.h"

#include <string>
#include <string_view>
#include <utility>

#include <paraconf.h>

#include "pdi/fmt.h"

#include "pdi_logger.h"

#include "plugin_logger.h"

using std::string;
using std::string_view;

namespace PDI {

Plugin_logger::Plugin_logger(Pdi_logger& pdi_logger, string_view name, PC_tree_t logging_tree)
	: Logger_common(fmt::format("PDI/{}", name))
	, m_pdi_logger(&pdi_logger)
{
	setup(logging_tree, pdi_logger.m_outputs);
	update_level(pdi_logger.m_default_logging_level);
	// register last, so that a logger whose configuration is invalid leaves no dangling pointer in the PDI logger
	pdi_logger.m_plugin_loggers.emplace(this);
}

Plugin_logger::~Plugin_logger()
{
	if (m_pdi_logger) {
		m_pdi_logger->m_plugin_loggers.erase(this);
	} else {
		error("plugin logger `{}' destroyed after the logging infrastructure", m_logger.name());
	}
}

void Plugin_logger::add_default_pattern_block(string_view block)
{
	if (m_pdi_logger) m_pdi_logger->add_default_pattern_block(block);
}

void Plugin_logger::default_pattern(string pattern, Context& ctx)
{
	if (m_pdi_logger) m_pdi_logger->default_pattern(std::move(pattern), ctx);
}

void Plugin_logger::default_level(string level_str)
{
	if (m_pdi_logger) m_pdi_logger->default_level(std::move(level_str));
}

} // namespace PDI
