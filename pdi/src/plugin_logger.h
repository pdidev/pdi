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

#ifndef PDI_PLUGIN_LOGGER_H_
#define PDI_PLUGIN_LOGGER_H_

#include <string>
#include <string_view>

#include <paraconf.h>

#include "pdi/pdi_fwd.h"
#include "pdi/logger.h"

namespace PDI {

class Pdi_logger;

/** The logger of a plugin, that uses the logging infrastructure the PDI logger holds
 *
 * It has a name and a logging level of its own, and writes to the outputs of the infrastructure, except those its configuration disables.
 *
 * Setting the default logging level or the default pattern through such a logger is deprecated.
 *
 * It is exported for the unit tests only, until they can link to the objects of the library, see https://github.com/pdidev/pdi/issues/834
 */
class PDI_EXPORT Plugin_logger: public Logger
{
	friend class Pdi_logger;

	/// The PDI logger that holds the infrastructure this logger uses
	Pdi_logger* m_pdi_logger = nullptr;

public:
	/** Create a plugin logger
	 *
	 * \param[in] pdi_logger the PDI logger
	 * \param[in] name the name of the plugin, the logger is named `PDI/<name>`, as the `%n` flag of the pattern shows
	 * \param[in] logging_tree the `logging` subtree that configures the logger: a level, or a mapping with optional `level` and `disabled_outputs`
	 *                         keys; a tree in error configures nothing
	 *
	 * \throw Spectree_error if the configuration is invalid
	 */
	Plugin_logger(Pdi_logger& pdi_logger, std::string_view name, PC_tree_t logging_tree);

	/** Destroy the logger
	 *
	 * It must be destroyed before the PDI logger.
	 */
	~Plugin_logger();

	void add_default_pattern_block(std::string_view block) override;

	[[deprecated("this will be removed in an upcoming release. Plugins should not change the global PDI behaviour.")]]
	void default_level(std::string level) override;

	[[deprecated("this will be removed in an upcoming release. Plugins should not change the global PDI behaviour.")]]
	void default_pattern(std::string pattern, Context& ctx) override;
};

} // namespace PDI

#endif // PDI_PLUGIN_LOGGER_H_
