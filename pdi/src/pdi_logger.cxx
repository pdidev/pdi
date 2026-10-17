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

#include <algorithm>
#include <functional>
#include <memory>
#include <optional>
#include <ostream>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

#include <paraconf.h>

#include <spdlog/sinks/basic_file_sink.h>
#include <spdlog/sinks/null_sink.h>
#include <spdlog/sinks/ostream_sink.h>
#include <spdlog/sinks/stdout_color_sinks.h>

#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/fmt.h"
#include "pdi/paraconf_wrapper.h"

#include "plugin_logger.h"

#include "pdi_logger.h"

using fmt::join;
using spdlog::sink_ptr;
using spdlog::sinks::basic_file_sink_st;
using spdlog::sinks::null_sink_st;
using spdlog::sinks::ostream_sink_st;
using spdlog::sinks::stderr_color_sink_st;
using spdlog::sinks::stdout_color_sink_st;
using std::current_exception;
using std::dynamic_pointer_cast;
using std::make_shared;
using std::min;
using std::nullopt;
using std::optional;
using std::ostream;
using std::ostringstream;
using std::reference_wrapper;
using std::set;
using std::string;
using std::string_view;
using std::swap;

namespace PDI {

void Pdi_logger::default_level(Level level)
{
	m_default_logging_level = level;
	update_level(m_default_logging_level);
	for (auto&& plugin_logger: m_plugin_loggers) {
		plugin_logger->update_level(m_default_logging_level);
	}
}

string Pdi_logger::build_pattern(optional<string> specific_pattern, Pattern_source specific_src, optional<reference_wrapper<Context>> ctx)
{
	// a copy, that the parsing of the references modifies
	string pattern;
	Pattern_source src;
	if (specific_pattern) {
		pattern = std::move(*specific_pattern);
		src = (specific_src == Pattern_source::EVALUATED ? Pattern_source::EVALUATED : Pattern_source::UNEVALUATED_EXPLICIT);
	} else {
		pattern = m_default_pattern;
		src = m_default_pattern_source;
	}

	// the built-in pattern, with its blocks, holds no reference
	if (src != Pattern_source::UNEVALUATED_EXPLICIT) return pattern;

	ostringstream pattern_builder;
	// copy the text up to the first flag
	size_t start_pos = min(pattern.find('%'), pattern.size());
	pattern_builder << string_view(pattern.begin(), pattern.begin() + start_pos);
	while (start_pos < pattern.size()) {
		// a `%' must be followed by its flag
		if (start_pos == pattern.size() - 1) {
			throw Value_error("logger pattern can not end with a `%': `{}'", pattern);
		}
		switch (pattern[start_pos + 1]) {
		case '{': {
			// `%{<REF>}' is the reference `${<REF>}', parse it to find where it ends
			pattern[start_pos] = '$';
			auto [expr, expr_len] = Expression::parse_reference(pattern.c_str() + start_pos);
			// restore the pattern as written, for the error messages
			pattern[start_pos] = '%';
			// write its value in place of it, or nothing until there is a context to evaluate it in
			if (ctx) {
				// double each `%' of the value, so that spdlog prints it as is instead of reading a flag
				for (char value_char: expr.to_string(*ctx)) {
					if (value_char == '%') pattern_builder << '%';
					pattern_builder << value_char;
				}
			}
			start_pos += expr_len;
		} break;
		default: {
			// copy any other flag, `%%' included
			pattern_builder << string_view(pattern.begin() + start_pos, pattern.begin() + start_pos + 2);
			start_pos += 2;
		} break;
		}
		// copy the text up to the next flag
		size_t end_pos = min(pattern.find('%', start_pos), pattern.size());
		pattern_builder << string_view(pattern.begin() + start_pos, pattern.begin() + end_pos);
		start_pos = end_pos;
	}
	return pattern_builder.str();
}

template <class Sink_type, class... Args>
void Pdi_logger::add_output(string name, Level lvl, optional<string> pattern, Args&&... args)
{
	sink_ptr sink = make_shared<Sink_type>(std::forward<Args>(args)...);
	sink->set_level(lvl);
	m_outputs.emplace(std::move(name), Output{std::move(sink), std::move(pattern)});
}

Pdi_logger::Pdi_logger(PC_tree_t logging_tree)
	: Logger_common("PDI")
{
	bool log_to_stdout = true; // unless outputs are configured, write to stdout
	PC_tree_t warn_about_deprecated_output{PC_NODE_NOT_FOUND, nullptr, nullptr};
	if (is_scalar(logging_tree)) {
		m_default_logging_level = level_from_str(logging_tree);
	} else if (!PC_status(logging_tree)) {
		each(logging_tree, [&](PC_tree_t key_tree, PC_tree_t value_tree) {
			string key = to_string(key_tree);
			if (key == "level") {
				m_default_logging_level = level_from_str(value_tree);
			} else if (key == "pattern") {
				m_default_pattern = to_string(value_tree);
				m_default_pattern_source = Pattern_source::UNEVALUATED_EXPLICIT;
			} else if (key == "core" || key == "output" || key == "outputs") {
				// configures the outputs that need the default level and patterns, and the core logger, that needs the outputs: read after them, below
			} else {
				throw Spectree_error(key_tree, "unexpected key `{}', valid logging keys are: `level', `pattern', `outputs', and `core' (the deprecated `output' is also accepted)", key);
			}
		});
		PC_tree_t output_tree = PC_get(logging_tree, ".output");
		PC_tree_t outputs_tree = PC_get(logging_tree, ".outputs");
		if (!PC_status(output_tree)) {
			if (!PC_status(outputs_tree)) {
				throw Spectree_error(output_tree, "cannot specify the deprecated `output' key when the `outputs' key is present");
			}
			// warn that `output' is deprecated once the logger is built, to write the warning where the configuration says
			warn_about_deprecated_output = output_tree;
			// the deprecated output: a file if `file' is set
			PC_tree_t file_subtree = PC_get(output_tree, ".file");
			if (!PC_status(file_subtree)) {
				log_to_stdout = false; // the file replaces stdout, unless `console' says otherwise
				add_output<basic_file_sink_st>("file", Level::trace, nullopt, to_string(file_subtree));
			}
			// and stdout if `console' is true, or if it is not set and `file' is not either
			PC_tree_t console_subtree = PC_get(output_tree, ".console");
			if (!PC_status(console_subtree)) log_to_stdout = to_bool(console_subtree);
		}
		if (!PC_status(outputs_tree)) {
			log_to_stdout = false;
			each(outputs_tree, [&](PC_tree_t output_name_tree, PC_tree_t output_tree) {
				string name = to_string(output_name_tree);
				if (m_outputs.contains(name)) {
					throw Spectree_error(output_name_tree, "invalid YAML, duplicate key `{}' in `outputs'", name);
				}
				Level level = Level::trace;
				optional<string> pattern;
				string kind;
				string path;
				// read the whole configuration of the output first, with its name as context for the errors
				try {
					PC_tree_t level_tree = PC_get(output_tree, ".level");
					if (!PC_status(level_tree)) level = level_from_str(level_tree);
					PC_tree_t pattern_tree = PC_get(output_tree, ".pattern");
					if (!PC_status(pattern_tree)) pattern = to_string(pattern_tree);
					set<string> allowed_key{"kind", "level", "pattern"};
					kind = to_string(PC_get(output_tree, ".kind"));
					if (kind == "file") {
						path = to_string(PC_get(output_tree, ".path"));
						allowed_key.emplace("path");
					} else if (kind != "stdout" && kind != "stderr" && kind != "stream") {
						throw Spectree_error(output_tree, "unexpected kind `{}', valid kinds are: `file', `stdout', `stderr', and `stream'", kind);
					}
					each(output_tree, [&](PC_tree_t key_tree, PC_tree_t) {
						string key = to_string(key_tree);
						if (!allowed_key.contains(key)) {
							throw Spectree_error(
								key_tree,
								"unexpected key `{}', valid keys for kind `{}' are: `{}'",
								key,
								kind,
								join(allowed_key, "', `")
							);
						}
					});
				} catch (...) {
					rethrow_with_context(current_exception(), "while reading output `{}', ", name);
				}
				// then build it, out of the context, so that a file that cannot be opened is reported as spdlog does, with its path
				if (kind == "file") {
					add_output<basic_file_sink_st>(std::move(name), level, std::move(pattern), path);
				} else if (kind == "stdout") {
					add_output<stdout_color_sink_st>(std::move(name), level, std::move(pattern));
				} else if (kind == "stderr") {
					add_output<stderr_color_sink_st>(std::move(name), level, std::move(pattern));
				} else {
					// an output of kind `stream' writes nowhere until set_output_stream sets its stream
					add_output<null_sink_st>(std::move(name), level, std::move(pattern));
				}
			});
		}
	}
	if (log_to_stdout) {
		add_output<stdout_color_sink_st>("stdout", Level::trace, nullopt);
	}
	// now that we have the default and outputs loaded, load the logger
	try {
		setup(PC_get(logging_tree, ".core"), m_outputs);
	} catch (...) {
		rethrow_with_context(current_exception(), "while reading the logger of the core, ");
	}
	// and propagate the defaults
	default_level(m_default_logging_level);
	string default_pattern = build_pattern(nullopt, m_default_pattern_source);
	for (auto&& [out_name, output]: m_outputs) {
		if (output.pattern) {
			output.sink->set_pattern(build_pattern(output.pattern, Pattern_source::UNEVALUATED_EXPLICIT));
		} else {
			output.sink->set_pattern(default_pattern);
		}
	}
	if (!PC_status(warn_about_deprecated_output)) {
		warn("`logging.output' is deprecated in favor of the new `logging.outputs' (plural)");
	}
}

Pdi_logger::~Pdi_logger()
{
	for (auto&& plugin_logger: m_plugin_loggers) {
		error("plugin logger `{}' destroyed after the logging infrastructure", plugin_logger->m_logger.name());
		plugin_logger->m_pdi_logger = nullptr;
	}
	assert(m_plugin_loggers.empty() && "The PDI logger should be destroyed after all the plugin loggers");
}

void Pdi_logger::add_default_pattern_block(string_view block)
{
	if (m_default_pattern_source == Pattern_source::EVALUATED) {
		throw State_error("unable to add a block to the default logging pattern once the patterns are evaluated");
	}
	if (m_default_pattern_source != Pattern_source::BLOCK_LIST) return;
	// insert the block before the name of the logger, that ends the built-in pattern with the message
	constexpr string_view built_in_end = "[%n] *** %^%l%$: %v";
	m_default_pattern
		= fmt::format("{}[{}]{}", string_view(m_default_pattern).substr(0, m_default_pattern.size() - built_in_end.size()), block, built_in_end);
	for (auto&& [out_name, output]: m_outputs) {
		if (!output.pattern) output.sink->set_pattern(m_default_pattern);
	}
}

void Pdi_logger::evaluate_patterns(Context& ctx)
{
	if (m_default_pattern_source == Pattern_source::EVALUATED) {
		throw State_error("the logging patterns can only be evaluated once");
	}

	// we compute the default pattern
	if (m_default_pattern_source == Pattern_source::UNEVALUATED_EXPLICIT) {
		m_default_pattern = build_pattern(m_default_pattern, m_default_pattern_source, ctx);
	}
	m_default_pattern_source = Pattern_source::EVALUATED;

	for (auto&& [out_name, output]: m_outputs) {
		if (output.pattern) {
			// keep the evaluated pattern, for the stream that may later be set
			output.pattern = build_pattern(output.pattern, Pattern_source::UNEVALUATED_EXPLICIT, ctx);
			output.sink->set_pattern(*output.pattern);
		} else {
			output.sink->set_pattern(m_default_pattern);
		}
	}
}

void Pdi_logger::default_pattern(string pattern, Context& ctx)
{
	string local_default_pattern;
	string& default_pattern = [&]() -> string& {
		if (m_default_pattern_source == Pattern_source::EVALUATED) {
			// after sealing, use the provided context, evaluate the pattern and store and use the evaluated pattern
			m_default_pattern = build_pattern(std::move(pattern), Pattern_source::UNEVALUATED_EXPLICIT, ctx);
			return m_default_pattern;
		}
		// before sealing, store the unevaluated pattern, and work with a local evaluation without context
		local_default_pattern = build_pattern(pattern, Pattern_source::UNEVALUATED_EXPLICIT, nullopt);
		m_default_pattern = std::move(pattern);
		m_default_pattern_source = Pattern_source::UNEVALUATED_EXPLICIT;
		return local_default_pattern;
	}();
	for (auto&& [out_name, output]: m_outputs) {
		if (!output.pattern) output.sink->set_pattern(default_pattern);
	}
}

void Pdi_logger::default_level(string level_str)
{
	default_level(level_from_str(level_str));
}

void Pdi_logger::set_output_stream(string_view name, ostream& output)
{
	auto out_it = m_outputs.find(name);
	if (out_it == m_outputs.end()) {
		throw Invalid_action_error("unable to set the stream of output `{}', there is no output by that name", name);
	}
	sink_ptr& sink = out_it->second.sink;
	if (!dynamic_pointer_cast<null_sink_st>(sink) && !dynamic_pointer_cast<ostream_sink_st>(sink)) {
		throw Invalid_action_error("unable to set the stream of output `{}', it is not of kind `stream'", name);
	}
	sink_ptr new_sink = make_shared<ostream_sink_st>(output);
	new_sink->set_pattern(build_pattern(out_it->second.pattern, m_default_pattern_source));
	new_sink->set_level(sink->level());
	swap(new_sink, sink);
	// new_sink now holds the sink to replace
	for (auto&& plugin_logger: m_plugin_loggers) {
		plugin_logger->output_updated(name, new_sink, sink);
	}
	output_updated(name, new_sink, sink);
}

} // namespace PDI
