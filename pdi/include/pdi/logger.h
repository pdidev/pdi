/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_LOGGER_H_
#define PDI_LOGGER_H_

#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

#include <paraconf.h>

#include <spdlog/logger.h>

#include <pdi/pdi_fwd.h>
#include <pdi/paraconf_wrapper.h>

namespace PDI {

/** A logger of the PDI logging infrastructure, that can be neither copied nor moved
 *
 * The infrastructure has a default logging level, a default pattern, and a set of named outputs (sinks in spdlog parlance).
 * Each logger has a name and a logging level, and writes to a subset of these outputs.
 * Each output has a logging level, and a pattern.
 *
 * Unless set explicitly, there is a single output named `stdout`, that writes to the standard output.
 *
 * Unless set explicitly, the default pattern is `[%T][<block 1>]...[<block N>][%n] *** %^%l%$: %v`, where any logger can add blocks with
 * add_default_pattern_block().
 * When set explicitly, a pattern may hold references: `%{<REF>}` stands for the value of the $-expression `${<REF>}`, once the infrastructure is
 * sealed; until then, it stands for nothing.
 * An output whose pattern has not been set explicitly uses the default pattern.
 * When logging through a logger, each of its outputs formats the message with its pattern.
 *
 * Unless set explicitly, the default logging level is `info`, and the level of each output is `trace`.
 * A logger whose logging level has not been set explicitly uses the default logging level.
 * When logging a message through a logger, it is first filtered by the level of the logger, and dismissed if its level is lower than that of the
 * logger.
 * Otherwise, it is forwarded to every output of the logger, and filtered again by the level of the output.
 *
 * The infrastructure goes through the following steps, in order:
 * 1. its construction, from the configuration,
 * 2. the addition of blocks to the default pattern, with add_default_pattern_block(),
 * 3. its sealing, when PDI evaluates the references of the patterns, once all the plugins are loaded.
 *
 * Adding a block after the sealing is an error.
 *
 * Doing the following through a standard logger is deprecated:
 * - setting the default logging level with default_level(),
 * - setting the default pattern with default_pattern().
 */
class PDI_EXPORT Logger
{
public:
	/// The logging levels
	using Level = spdlog::level::level_enum;

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

	/// The spdlog logger, that holds the name, level and outputs of this logger
	spdlog::logger m_logger;

	/// Whether this logger has a level explicitly set
	bool m_explicitly_set_logging_level = false;

	/// The names of the outputs of the infrastructure this logger does not write to
	std::unordered_set<std::string, Transparent_string_hash, std::equal_to<>> m_disabled_outputs;

	/** Create a logger that writes to no output
	 *
	 * \param[in] name the name of the logger, shown by the `%n` flag of the pattern
	 */
	Logger(std::string name);

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

public:
	Logger(const Logger&) = delete;

	Logger(Logger&&) = delete;

	virtual ~Logger();

	Logger& operator= (const Logger&) = delete;

	Logger& operator= (Logger&& moved_from) = delete;

	/** Add a block to the default pattern of the infrastructure, unless the default pattern has been set explicitly
	 *
	 * The block goes between square brackets, after the blocks added before and before the name of the logger.
	 *
	 * \param[in] block the content of the block, without its enclosing brackets, in spdlog pattern syntax
	 *
	 * \throw State_error if the infrastructure is sealed, its patterns evaluated
	 */
	virtual void add_default_pattern_block(std::string_view block) = 0;

	/** Write a message at trace level, if the level of the logger lets it through
	 *
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void trace(fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.trace(std::move(fmt), std::forward<Args>(args)...);
	}

	/** Write a message at debug level, if the level of the logger lets it through
	 *
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void debug(fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.debug(std::move(fmt), std::forward<Args>(args)...);
	}

	/** Write a message at info level, if the level of the logger lets it through
	 *
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void info(fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.info(std::move(fmt), std::forward<Args>(args)...);
	}

	/** Write a message at warning level, if the level of the logger lets it through
	 *
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void warn(fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.warn(std::move(fmt), std::forward<Args>(args)...);
	}

	/** Write a message at error level, if the level of the logger lets it through
	 *
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void error(fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.error(std::move(fmt), std::forward<Args>(args)...);
	}

	/** Write a message at a given level, if the level of the logger lets it through
	 *
	 * \param[in] lvl the level of the message
	 * \param[in] fmt the fmt format string of the message
	 * \param[in] args the values to format in `fmt`
	 */
	template <typename... Args>
	void log(Level lvl, fmt::format_string<Args...> fmt, Args&&... args)
	{
		m_logger.log(lvl, std::move(fmt), std::forward<Args>(args)...);
	}

	/** Set the default logging level of the infrastructure
	 *
	 * \param[in] level the name of the new level, case insensitive: `trace`, `debug`, `info`, `warning`/`warn`, `error`/`err`, `critical` or `off`
	 *
	 * \throw Value_error if `level` names no known level
	 *
	 * \deprecated this will be removed in an upcoming release. The global logging infrastructure should not be modified through loggers.
	 */
	[[deprecated("this will be removed in an upcoming release. The global logging infrastructure should not be modified through loggers.")]]
	virtual void default_level(std::string level)
		= 0;

	/** Set the default pattern of the infrastructure
	 *
	 * Once the infrastructure is sealed, the references of the pattern are evaluated right away.
	 * Before, they stand for nothing until the infrastructure is sealed, and are evaluated then.
	 *
	 * \param[in] pattern the new pattern, in spdlog syntax, where `%{<REF>}` stands for the value of the $-expression `${<REF>}`
	 * \param[in] ctx the context in which to evaluate the references, once the infrastructure is sealed
	 *
	 * \throw Value_error if `pattern` ends with a lone `%`, or holds a reference that does not parse
	 * \throw Error as Expression::to_string does, if a reference cannot be evaluated
	 *
	 * \deprecated this will be removed in an upcoming release. The global logging infrastructure should not be modified through loggers.
	 */
	[[deprecated("this will be removed in an upcoming release. The global logging infrastructure should not be modified through loggers.")]]
	virtual void default_pattern(std::string pattern, Context& ctx)
		= 0;
};

} // namespace PDI

#endif // PDI_LOGGER_H_
