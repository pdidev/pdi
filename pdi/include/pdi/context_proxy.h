/*
 * SPDX-FileCopyrightText: 2019-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

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
