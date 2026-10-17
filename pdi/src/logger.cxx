/*******************************************************************************
 * Copyright (C) 2018-2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <algorithm>
#include <functional>
#include <memory>
#include <ranges>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>

#include <paraconf.h>

#include <spdlog/common.h>
#include <spdlog/sinks/null_sink.h>

#include "pdi/error.h"
#include "pdi/fmt.h"
#include "pdi/paraconf_wrapper.h"

#include "logger_tools.h"

#include "pdi/logger.h"

using fmt::join;
using spdlog::sink_ptr;
using spdlog::sinks::null_sink_st;
using std::dynamic_pointer_cast;
using std::equal_to;
using std::find;
using std::string;
using std::string_view;
using std::unordered_map;
namespace views = std::views;

namespace PDI {

Logger::Logger(string name)
	: m_logger(std::move(name))
{
	m_logger.set_level(Level::trace);
}

void Logger::setup(PC_tree_t logger_tree, const unordered_map<string, Output, Transparent_string_hash, equal_to<>>& outputs)
{
	if (is_scalar(logger_tree)) {
		m_logger.set_level(level_from_str(logger_tree));
		m_explicitly_set_logging_level = true;
	} else if (!PC_status(logger_tree)) {
		each(logger_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
			string key = to_string(key_tree);
			if (key == "level") {
				m_logger.set_level(level_from_str(value_tree));
				m_explicitly_set_logging_level = true;
			} else if (key == "disabled_outputs") {
				each(value_tree, [&](PC_tree_t one_disabled_output_tree) {
					string disabled_name = to_string(one_disabled_output_tree);
					if (!outputs.contains(disabled_name)) {
						throw Spectree_error(
							one_disabled_output_tree,
							"unknown output `{}', valid outputs are: `{}'",
							disabled_name,
							join(outputs | views::keys, "', `")
						);
					}
					m_disabled_outputs.emplace(std::move(disabled_name));
				});
			} else {
				throw Spectree_error(key_tree, "unexpected key `{}', valid logging keys are: `level' and `disabled_outputs'", key);
			}
		});
	}
	for (auto&& [out_name, output]: outputs) {
		// an output of kind `stream' whose stream is not set writes nowhere: output_updated adds it once its stream is set
		if (!m_disabled_outputs.contains(out_name) && !dynamic_pointer_cast<null_sink_st>(output.sink)) {
			m_logger.sinks().emplace_back(output.sink);
		}
	}
}

void Logger::update_level(Level default_logging_level)
{
	if (!m_explicitly_set_logging_level) m_logger.set_level(default_logging_level);
}

void Logger::output_updated(string_view name, const sink_ptr& old_output, const sink_ptr& output)
{
	if (m_disabled_outputs.contains(name)) return;
	// each output has a sink of its own, that a logger holds once at most
	auto&& sink_it = find(m_logger.sinks().begin(), m_logger.sinks().end(), old_output);
	if (sink_it != m_logger.sinks().end()) {
		*sink_it = output;
	} else {
		m_logger.sinks().emplace_back(output);
	}
}

Logger::~Logger() = default;

} // namespace PDI
