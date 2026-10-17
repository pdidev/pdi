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

#ifndef PDI_LOGGER_COMMON_H_
#define PDI_LOGGER_COMMON_H_

#include <algorithm>
#include <cctype>
#include <functional>
#include <iterator>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>

#include <paraconf.h>

#include <spdlog/common.h>

#include "pdi/pdi_fwd.h"
#include "pdi/error.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"

namespace PDI {

/** The part of the implementation of a logger that the PDI logger and the plugin loggers share
 *
 * It reads the configuration of a logger, and follows the changes of the infrastructure: of its default logging level, and of its outputs of kind
 * `stream`.
 *
 * It is exported for the unit tests only, until they can link to the objects of the library, see https://github.com/pdidev/pdi/issues/834
 */
class PDI_EXPORT Logger_common: public Logger
{
protected:
	/// An output of the infrastructure
	struct Output {
		/** Where the output writes, with its level and pattern set
		 *
		 * An output of kind `stream` writes nowhere until its stream is set, it holds a spdlog::sinks::null_sink_st until then.
		 */
		spdlog::sink_ptr sink;

		/** The pattern of the output, empty to use the default pattern
		 *
		 * It holds references to evaluate until the infrastructure is sealed, and is evaluated from then on.
		 */
		std::optional<std::string> pattern;
	};

	/// A hash of strings that hashes string views alike, so that a container keyed by strings can be searched with a string view
	struct Transparent_string_hash: std::hash<std::string_view> {
		/// Tell the containers that this hash accepts any type that converts to a string view
		using is_transparent = void;
	};

	/// Whether this logger has a level explicitly set
	bool m_explicitly_set_logging_level = false;

	/// The names of the outputs of the infrastructure this logger does not write to
	std::unordered_set<std::string, Transparent_string_hash, std::equal_to<>> m_disabled_outputs;

	/** Look a level up by its name
	 *
	 * This matches names as spdlog::level::from_str does since gabime/spdlog@566b2d1 (gabime/spdlog#3535), ignoring case, except that an unknown name
	 * is an error instead of spdlog::level::off.
	 *
	 * \param[in] name_param the name of the level, or a PC_tree_t holding it, case insensitive: one of the names spdlog gives its levels, `warn` or
	 *                       `err`
	 *
	 * \return the level
	 *
	 * \throw Spectree_error if `name_param` is a PC_tree_t and names no known level
	 * \throw Value_error if `name_param` is a string and names no known level
	 */
	static Level level_from_str(auto name_param)
	{
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
		if (it != std::end(level_string_views)) return static_cast<Level>(std::distance(std::begin(level_string_views), it));
		if (name == "warn") return Level::warn;
		if (name == "err") return Level::err;
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

	/** Create a logger that writes to no output
	 *
	 * \param[in] name the name of the logger, shown by the `%n` flag of the pattern
	 */
	Logger_common(std::string name);

	/** Read the configuration of a logger: its level, and the outputs it writes to
	 *
	 * It adds to the spdlog logger every output of the infrastructure, except those the configuration disables and the outputs of kind `stream` whose
	 * stream is not set yet.
	 * It is meant to be called once, by the constructor of a derived class.
	 *
	 * \param[in] logger_tree the configuration: a level, or a mapping with optional `level` and `disabled_outputs` keys; a tree in error configures
	 *                        nothing
	 * \param[in] outputs the outputs of the infrastructure, by name
	 *
	 * \throw Spectree_error if the configuration is neither a level nor a mapping, names no known level or output, or holds an unexpected key
	 */
	void setup(PC_tree_t logger_tree, const std::unordered_map<std::string, Output, Transparent_string_hash, std::equal_to<>>& outputs);

	/** Set the level of the spdlog logger to the default logging level, unless this logger has a level set explicitly
	 *
	 * \param[in] default_logging_level the default logging level of the infrastructure
	 */
	void update_level(Level default_logging_level);

	/** Replace the sink of an output of kind `stream` by a new one, unless this logger does not write to that output
	 *
	 * The new sink is added if this logger does not hold the old one, as for an output whose stream was not set.
	 *
	 * \param[in] name the name of the output
	 * \param[in] old_output the output to replace
	 * \param[in] output the output to replace it with
	 */
	void output_updated(std::string_view name, const spdlog::sink_ptr& old_output, const spdlog::sink_ptr& output);
};

} // namespace PDI

#endif // PDI_LOGGER_COMMON_H_
