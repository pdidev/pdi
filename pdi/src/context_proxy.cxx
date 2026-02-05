// SPDX-FileCopyrightText: 2019-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

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

Context::Iterator Context_proxy::find(const string& name)
{
	return m_real_context.find(name);
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

Datatype_template_sptr Context_proxy::datatype(PC_tree_t node)
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
