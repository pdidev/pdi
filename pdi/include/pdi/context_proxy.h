/*******************************************************************************
 * Copyright (C) 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2019-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_CONTEXT_PROXY_H_
#define PDI_CONTEXT_PROXY_H_

#include <pdi/pdi_fwd.h>
#include <pdi/callbacks.h>
#include <pdi/context.h>
#include <pdi/logger.h>

#include <functional>
#include <string>

namespace PDI {

class PDI_EXPORT Context_proxy: public Context
{
	/// Real context of this proxy
	Context& m_real_context;

	/// Logger of the plugin
	Logger m_plugin_logger;

public:
	/** Creates Context proxy without plugin logger
	 * \param[in] ctx context to make a proxy
	 */
	Context_proxy(Context& ctx);

	/** Creates Context proxy
	 * \param[in] ctx context to make a proxy
	 * \param[in] logger_name name of the logger (will be used in logger pattern)
	 * \param[in] logging_tree logging yaml tree of the plugin
	 */
	Context_proxy(Context& ctx, const std::string& logger_name, PC_tree_t logging_tree);

	/** Sets up logger
	 * \param[in] logger_name name of the logger (will be used in logger pattern)
	 * \param[in] logging_tree logging yaml tree of the plugin
	 */
	void setup_logger(const std::string& logger_name, PC_tree_t logging_tree);

	/** Context::desc proxy for plugins
	 */
	Data_descriptor& desc(const std::string& name) override;

	/** Context::desc proxy for plugins
	 */
	Data_descriptor& desc(const char* name) override;

	/** Context::operator[] proxy for plugins
	 */
	Data_descriptor& operator[] (const std::string& name) override;

	/** Context::operator[] proxy for plugins
	 */
	Data_descriptor& operator[] (const char* name) override;

	/** Context::begin proxy for plugins
	 */
	Iterator begin() override;

	/** Context::end proxy for plugins
	 */
	Iterator end() override;

	Iterator find(const std::string& name) override;

	/** Context::event proxy for plugins
	 *
	 *  \param[in] name name of the event
	 */
	void event(const char* name) override;

	/** Returns plugin logger
	 *
	 *  \return plugin logger
	 */
	Logger& logger() override;

	/** Returns pdi core logger
	 *
	 * \return pdi core logger
	 */
	Logger& pdi_core_logger();

	/** Context::datatype proxy for plugins
	 */
	Datatype_template_sptr datatype(PC_tree_t node) override;

	/** Context::add_datatype proxy for plugins
	 */
	void add_datatype(const std::string& name, Datatype_template_parser parser) override;

	/** Context::callbacks proxy for plugins
	 */
	Callbacks& callbacks() override;

	void finalize_and_exit() override;
};

} //namespace PDI

#endif // PDI_CONTEXT_PROXY_H_
