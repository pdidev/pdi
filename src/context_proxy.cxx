/*******************************************************************************
 * Copyright (C) 2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <spdlog/spdlog.h>

#include "pdi/context.h"
#include "pdi/context_proxy.h"
#include "pdi/logger.h"

namespace PDI {

Context_proxy::Context_proxy(Context& ctx, std::string plugin_name, PC_tree_t plugin_logging):
	m_real_context{ctx},
	m_plugin_logger{ctx.logger()->clone(plugin_name)}
{
	// check if logger level was specified for plugin
	if (!PC_status(PC_get(plugin_logging, ".level"))) {
		read_log_level(m_plugin_logger, plugin_logging);
	}
}

Data_descriptor& Context_proxy::desc(const std::string& name)
{
	return m_real_context.desc(name);
}

Data_descriptor& Context_proxy::desc(const char* name)
{
	return m_real_context.desc(name);
}

Data_descriptor& Context_proxy::operator[](const std::string& name)
{
	return m_real_context[name];
}

Data_descriptor& Context_proxy::operator[](const char* name)
{
	return m_real_context[name];
}

Context::Iterator Context_proxy::begin()
{
	return m_real_context.begin();
}

Context::Iterator Context_proxy::end()
{
	return m_real_context.end();
}

void Context_proxy::event(const char* name)
{
	m_real_context.event(name);
}

Logger_sptr Context_proxy::logger() const
{
	return m_plugin_logger;
}

Logger_sptr Context_proxy::pdi_core_logger() const
{
	return m_real_context.logger();
}

Datatype_template_uptr Context_proxy::datatype(PC_tree_t node)
{
	return m_real_context.datatype(std::move(node));
}

void Context_proxy::add_datatype(const std::string& name, Datatype_template_parser parser)
{
	m_real_context.add_datatype(name, std::move(parser));
}

std::function<void()> Context_proxy::add_init_callback(const std::function<void()>& callback)
{
	return m_real_context.add_init_callback(callback);
}

std::function<void()> Context_proxy::add_data_callback(const std::function<void(const std::string&, Ref)>& callback, const std::string& name)
{
	return m_real_context.add_data_callback(callback, name);
}

std::function<void()> Context_proxy::add_event_callback(const std::function<void(const std::string&)>& callback, const std::string& name)
{
	return m_real_context.add_event_callback(callback, name);
}

std::function<void()> Context_proxy::add_empty_desc_access_callback(const std::function<void(const std::string&)>& callback, const std::string& name)
{
	return m_real_context.add_empty_desc_access_callback(callback, name);
}

} //namespace PDI
