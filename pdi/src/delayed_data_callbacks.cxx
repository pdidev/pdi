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

#include "pdi/delayed_data_callbacks.h"

namespace PDI {

Delayed_data_callbacks::Delayed_data_callbacks(Global_context& ctx)
	: m_context{ctx}
	, m_datanames{}
{}

// In the destructor, we need to throw an error message in case the callback doesn't work on a data(trigger function)
//  (example: error in the config.yml for a plugin, error due to external library incompatibility)
Delayed_data_callbacks::~Delayed_data_callbacks() noexcept(false)
{
	try {
		this->trigger();
	} catch (const std::exception& e) {
		if (std::uncaught_exceptions()) {
			// An exception is throwing before. Print simple message to avoid std::terminate.
			m_context.logger().error("Error in the destructor of Delayed_data_callbacks, {}", e.what());
		} else {
			m_context.logger().info("{}", "Error in the destructor of Delayed_data_callbacks");
			throw;
		}
	} catch (...) {
		if (std::uncaught_exceptions()) {
			// An exception is throwing before. Print simple message to avoid std::terminate.
			m_context.logger().error("Error (no std::exception) in the destructor of Delayed_data_callbacks.");
		} else {
			m_context.logger().info("{}", "Error (no std::exception) in the destructor of Delayed_data_callbacks.");
			throw;
		}
	}
}

void Delayed_data_callbacks::add_dataname(const std::string& name)
{
	// Comment: In case of a multi_expose, if the data is defined twice then the callback "on_data" are called twice also (in PDI v1.10)
	//         Therefore, we need to add the name to have the same behaviour.
	m_datanames.emplace_back(name);
}

void Delayed_data_callbacks::trigger()
{
	int i = 0;
	size_t number_of_elements = m_datanames.size();
	std::vector<std::exception_ptr> trigger_data_errors;

	for (auto&& element_name: m_datanames) {
		try {
			m_context.logger().trace("Trigger data callback `{}' ({}/{})", element_name.c_str(), ++i, number_of_elements);
			m_context.callbacks().call_data_callbacks(element_name, m_context[element_name].ref());
		} catch (...) {
			trigger_data_errors.emplace_back(std::current_exception());
		}
	}

	m_datanames.clear();

	rethrow_with_context(trigger_data_errors, "`{}' error(s) while triggering data callbacks, ", number_of_elements);
}

void Delayed_data_callbacks::cancel()
{
	m_datanames.clear();
}

} // namespace PDI
