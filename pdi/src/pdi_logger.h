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

#ifndef PDI_PDI_LOGGER_H_
#define PDI_PDI_LOGGER_H_

#include <functional>
#include <iosfwd>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>

#include <paraconf.h>

#include <spdlog/common.h>

#include "pdi/pdi_fwd.h"
#include "pdi/logger.h"

#include "logger_common.h"

namespace PDI {

class Plugin_logger;

/** The logger of PDI core, that holds the PDI logging infrastructure
 *
 * It holds the infrastructure: the default logging level, the default pattern and the outputs, that the plugin loggers use.
 *
 * Unlike through a Logger or a Plugin_logger, it is not deprecated to do the following through it:
 * - setting the default logging level with default_level(),
 * - setting the default pattern with default_pattern().
 *
 * It also seals the infrastructure, with evaluate_patterns(), that evaluates the references in the patterns once all the plugins are loaded.
 *
 * It is exported for the unit tests only, until they can link to the objects of the library, see https://github.com/pdidev/pdi/issues/834
 */
class PDI_EXPORT Pdi_logger: public Logger_common
{
	friend class Plugin_logger;

	/// Where the default pattern comes from
	enum class Pattern_source {
		/// the built-in pattern, with the blocks added to it, nothing to evaluate
		BLOCK_LIST,
		/// set explicitly, with references to evaluate
		UNEVALUATED_EXPLICIT,
		/// fully evaluated, the infrastructure is sealed: no block can be added any more, but the default pattern can still be set
		EVALUATED
	};

	/// The plugin loggers that use the infrastructure
	std::unordered_set<Plugin_logger*> m_plugin_loggers;

	/// The outputs of the infrastructure, by name
	std::unordered_map<std::string, Output, Transparent_string_hash, std::equal_to<>> m_outputs;

	/// The default pattern, as m_default_pattern_source describes it
	std::string m_default_pattern = "[%T][%n] *** %^%l%$: %v";

	/// Where m_default_pattern comes from
	Pattern_source m_default_pattern_source = Pattern_source::BLOCK_LIST;

	/// The default logging level of the infrastructure
	Level m_default_logging_level = Level::info;

	/** Set the default logging level, and update the level of each logger of the infrastructure accordingly
	 *
	 * \param[in] level the new default logging level
	 */
	void default_level(Level level);

	/** Build the spdlog pattern of a pattern
	 *
	 * Unless the pattern is already evaluated, or is the built-in one, each `%{<REF>}` it holds is replaced by the value of `${<REF>}`, and every
	 * other flag is kept.
	 *
	 * \param[in] pattern the pattern, in spdlog syntax, where `%{<REF>}` stands for the value of the $-expression `${<REF>}`, or nullopt for the
	 *                    default pattern
	 * \param[in] src where `pattern` comes from, ignored for the default pattern, where m_default_pattern_source applies: a pattern is evaluated
	 *                if `src` is EVALUATED, and holds references to evaluate otherwise
	 * \param[in] ctx the context in which to evaluate the references, or nullopt to drop them
	 *
	 * \return the spdlog pattern
	 *
	 * \throw Value_error if the pattern ends with a lone `%`, or holds a reference that does not parse
	 * \throw Error as Expression::to_string does, if a reference cannot be evaluated
	 */
	std::string
	build_pattern(std::optional<std::string> pattern, Pattern_source src, std::optional<std::reference_wrapper<Context>> ctx = std::nullopt);

	/** Add an output to the infrastructure, this is a helper for the constructor and should only be used in it
	 *
	 * \tparam Sink_type the type of the spdlog sink of the output
	 * \tparam Args the types of the arguments of the constructor of the sink
	 *
	 * \param[in] name the name of the output, by which the loggers can disable it, that no other output may have: the new output is ignored otherwise
	 * \param[in] lvl the level of the output
	 * \param[in] pattern the pattern of the output, empty to use the default pattern
	 * \param[in] args the arguments of the constructor of the sink
	 */
	template <class Sink_type, class... Args>
	void add_output(std::string name, Level lvl, std::optional<std::string> pattern, Args&&... args);

public:
	/** Create a PDI logger, and the infrastructure it holds
	 *
	 * \param[in] logging_tree the `logging` subtree of the specification tree root: a level, or a mapping with optional `level`, `pattern`, `outputs`
	 *                         and `core` keys, or the deprecated `output` key in place of `outputs`; a tree in error configures nothing
	 *
	 * \throw Spectree_error if the configuration is neither a level nor a mapping, names no known level, kind of output, or output to disable,
	 *                       holds an unexpected key, two outputs by the same name, an output with no `kind` or a file one with no `path`, both the
	 *                       `output` and `outputs` keys, or a `console` that is not a boolean
	 * \throw Value_error if a pattern of the configuration ends with a lone `%`, or holds a reference that does not parse
	 * \throw spdlog::spdlog_ex if a file of the configuration cannot be opened
	 */
	Pdi_logger(PC_tree_t logging_tree);

	/** Destroy the logger
	 *
	 * All plugin loggers should be destroyed before this.
	 */
	~Pdi_logger();

	/** Seal the infrastructure: evaluate the references in its patterns, the default one and those of the outputs, and use the result
	 *
	 * It is meant to be called once, when all the plugins are loaded, when the data the references name can be available.
	 * From then on, the default pattern can be set, but no block can be added to it.
	 *
	 * \param[in] ctx the context in which to evaluate the references
	 *
	 * \throw State_error if the infrastructure is already sealed
	 * \throw Error as Expression::to_string does, if a reference cannot be evaluated
	 */
	void evaluate_patterns(Context& ctx);

	/** Set the stream an output of kind `stream` writes to
	 *
	 * The loggers that write to the output write to the stream from then on, with the level and the pattern of the output.
	 *
	 * \note This is meant for the tests of PDI for now, the application has no way to reach the PDI logger.
	 *
	 * \param[in] name the name of the output
	 * \param[in] output the stream to write to, that must outlive the loggers that write to it
	 *
	 * \throw Invalid_action_error if the infrastructure has no output by that name, or if that output is not of kind `stream`
	 */
	void set_output_stream(std::string_view name, std::ostream& output);

	void add_default_pattern_block(std::string_view) override;

	/** Set the default logging level of the infrastructure
	 *
	 * \note Unlike Logger::default_level(), this is not deprecated.
	 *
	 * \param[in] level the name of the new level, case insensitive: `trace`, `debug`, `info`, `warning`/`warn`, `error`/`err`, `critical` or `off`
	 *
	 * \throw Value_error if `level` names no known level
	 */
	void default_level(std::string level) override;

	/** Set the default pattern of the infrastructure
	 *
	 * Once the infrastructure is sealed, the references of the pattern are evaluated right away.
	 * Before, they stand for nothing until the infrastructure is sealed, and are evaluated then.
	 *
	 * \note Unlike Logger::default_pattern(), this is not deprecated.
	 *
	 * \param[in] pattern the new pattern, in spdlog syntax, where `%{<REF>}` stands for the value of the $-expression `${<REF>}`
	 * \param[in] ctx the context in which to evaluate the references, once the infrastructure is sealed
	 *
	 * \throw Value_error if `pattern` ends with a lone `%`, or holds a reference that does not parse
	 * \throw Error as Expression::to_string does, if a reference cannot be evaluated
	 */
	void default_pattern(std::string pattern, Context& ctx) override;
};

} // namespace PDI

#endif // PDI_PDI_LOGGER_H_
