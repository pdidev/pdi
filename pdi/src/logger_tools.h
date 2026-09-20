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

#ifndef PDI_LOGGER_TOOLS_H_
#define PDI_LOGGER_TOOLS_H_

#include <algorithm>
#include <cctype>
#include <iterator>
#include <string>
#include <type_traits>

#include <paraconf.h>

#include <spdlog/common.h>

#include "pdi/pdi_fwd.h"
#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"

namespace {

/** Look a level up by its name
 *
 * This matches names as spdlog::level::from_str does since gabime/spdlog@566b2d1 (gabime/spdlog#3535), ignoring case, except that an unknown name is
 * an error instead of spdlog::level::off.
 *
 * \param[in] name_param the name of the level, or a PC_tree_t holding it, case insensitive: one of the names spdlog gives its levels, `warn` or `err`
 *
 * \return the level
 *
 * \throw Spectree_error if `name_param` is a PC_tree_t and names no known level
 * \throw Value_error if `name_param` is a string and names no known level
 */
PDI::Logger::Level level_from_str(auto name_param)
{
	using namespace PDI;
	constexpr bool parsing_spectree = std::is_same_v<std::remove_cvref_t<decltype(name_param)>, PC_tree_t>;
	std::string name = [&] {
		if constexpr (parsing_spectree) {
			return to_string(name_param);
		} else {
			return name_param;
		}
	}();

	std::transform(name.begin(), name.end(), name.begin(), [](unsigned char c) { return std::tolower(c); });

	static constexpr spdlog::string_view_t level_string_views[] SPDLOG_LEVEL_NAMES;
	auto it = std::find(std::begin(level_string_views), std::end(level_string_views), name);
	if (it != std::end(level_string_views)) return static_cast<Logger::Level>(std::distance(std::begin(level_string_views), it));
	if (name == "warn") return Logger::Level::warn;
	if (name == "err") return Logger::Level::err;
	if constexpr (parsing_spectree) {
		throw Spectree_error(
			name_param,
			"invalid value, `{}' does not denote a valid logging level, valid levels are: `trace', `debug', `info', `warn', `error', `off'.",
			name
		);
	} else {
		throw Value_error(
			"invalid value, `{}' does not denote a valid logging level, valid levels are: `trace', `debug', `info', `warn', `error', `off'.",
			name
		);
	}
}

} // namespace

#endif // PDI_LOGGER_TOOLS_H_
