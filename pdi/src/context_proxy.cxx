/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "pdi/context.h"
#include "pdi/context_proxy.h"
#include "pdi/logger.h"

using std::move;
using std::string;

namespace PDI {


Context_proxy::Context_proxy(Context& ctx)
    : m_real_context{ctx}
{}

Context_proxy::Context_proxy(Context& ctx, const string& logger_name, PC_tree_t logging_tree)
    : m_real_context{ctx}
    , m_plugin_logger{m_real_context.logger(), logger_name, logging_tree}
{}

void Context_proxy::setup_logger(const string& logger_name, PC_tree_t logging_tree)
{
	m_plugin_logger.setup(m_real_context.logger(), logger_name, logging_tree);
}

Data_descriptor& Context_proxy::desc(const string& name)
{
	return m_real_context.desc(name);
}

Data_descriptor& Context_proxy::desc(const char* name)
{
	return m_real_context.desc(name);
}

Data_descriptor& Context_proxy::operator[] (const string& name)
{
	return m_real_context[name];
}

Data_descriptor& Context_proxy::operator[] (const char* name)
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

Logger& Context_proxy::logger()
{
	return m_plugin_logger;
}

Logger& Context_proxy::pdi_core_logger()
{
	return m_real_context.logger();
}

Datatype_template_ptr Context_proxy::datatype(PC_tree_t node)
{
	return m_real_context.datatype(move(node));
}

void Context_proxy::add_datatype(const string& name, Datatype_template_parser parser)
{
	m_real_context.add_datatype(name, move(parser));
}

Callbacks& Context_proxy::callbacks()
{
	return m_real_context.callbacks();
}

void Context_proxy::finalize_and_exit()
{
	m_real_context.finalize_and_exit();
}

} //namespace PDI
