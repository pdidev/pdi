/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <string>
#include <vector>

#include <spdlog/spdlog.h>

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/fmt.h>

#include <data_descriptor_impl.h>

#include "pdi/delayed_data_callbacks.h"

namespace PDI {

Delayed_data_callbacks::Delayed_data_callbacks(Global_context& ctx)
	: m_datanames{}
	, m_context{ctx}
{}

// In the destructor, we need to throw an error message in case of the callback on the data doesn't work (trigger function)
//  (example: error in the config.yml for a plugin, error due to external library incompatibility)
Delayed_data_callbacks::~Delayed_data_callbacks() noexcept(false)
{
	try {
		m_context.logger().info("### call trigger in the destructor");
		this->trigger();
	} catch (const std::exception& e) {
		if (std::uncaught_exceptions()) {
			// An exception is throwing before. Print simple message to avoid std::terminate.
			m_context.logger().error("Error in the destructor of Delayed_data_callbacks, {}", e.what());
		} else {
			throw;
		}
	} catch (...) {
		if (std::uncaught_exceptions()) {
			// An exception is throwing before. Print simple message to avoid std::terminate.
			m_context.logger().error("Error in the destructor of Delayed_data_callbacks.");
		} else {
			throw;
		}
	}
}

void Delayed_data_callbacks::add_dataname(const std::string& name)
{
	m_datanames.emplace_back(name);
}

void Delayed_data_callbacks::trigger()
{
	int i = 0;
	size_t number_of_elements = m_datanames.size();
	std::vector<std::string> msg_data_error;

	for (auto&& element_name: m_datanames) {
		try {
			m_context.logger().trace("Trigger data callback `{}' ({}/{})", element_name.c_str(), ++i, number_of_elements);
			m_context.callbacks().call_data_callbacks(element_name, m_context[element_name].ref());
		} catch (const std::exception& e) {
			std::string ewhat = e.what();
			msg_data_error.emplace_back("data=" + element_name + ", exception error=" + ewhat);
		} catch (...) {
			msg_data_error.emplace_back("data=" + element_name + ", no exception error");
		}
	}

	m_datanames.clear();

	// throw a message in case of error
	if (msg_data_error.size()) {
		const char* msg_system_error
			= "Found `{0}' error(s) in trigger the data callback  "
			  "This is the list of error for a each data: \n"
			  " - {1}\n";
		throw System_error{msg_system_error, msg_data_error.size(), fmt::join(msg_data_error, "\n - ")};
	}
}

void Delayed_data_callbacks::cancel()
{
	m_datanames.clear();
}

} // namespace PDI
