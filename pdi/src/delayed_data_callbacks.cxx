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
	// Comment: In case of a multi_expose, if the data is defined twice then the callback "on_data" are called twice also in PDI v1.10
	//         Therefore, we need to add the name to have the same behaviour.
	m_datanames.emplace_back(name);
}

void Delayed_data_callbacks::trigger()
{
	int i = 0;
	size_t number_of_elements = m_datanames.size();
	std::vector<Error> trigger_data_errors;

	for (auto&& element_name: m_datanames) {
		try {
			m_context.logger().trace("Trigger data callback `{}' ({}/{})", element_name.c_str(), ++i, number_of_elements);
			m_context.callbacks().call_data_callbacks(element_name, m_context[element_name].ref());
		} catch (const Error& e) {
			trigger_data_errors.emplace_back(e.status(), "Error in data callbacks for " + element_name + ": " + e.what());
		} catch (const std::exception& e) {
			trigger_data_errors.emplace_back(PDI_ERR_SYSTEM, "Error in data callbacks for " + element_name + ": " + e.what());
		} catch (...) {
			trigger_data_errors.emplace_back(PDI_ERR_SYSTEM, "Error in data callbacks for " + element_name + ": Not a std::exception based error.");
		}
	}

	m_datanames.clear();

	if (!trigger_data_errors.empty()) {
		if (1 == trigger_data_errors.size()) {
			if (std::uncaught_exceptions()) {
				Global_context::context().logger().error("Error while triggering data callbacks: {}", trigger_data_errors.front().what());
			} else {
				throw Error{trigger_data_errors.front().status(), trigger_data_errors.front().what()};
			}
		} else {
			Multiple_error trigger_multiple_error{
				trigger_data_errors,
				std::to_string(trigger_data_errors.size()) + " error(s) while triggering data callbacks"
			};
			if (std::uncaught_exceptions()) {
				Global_context::context().logger().error(trigger_multiple_error.what());
			} else {
				throw trigger_multiple_error;
			}
		}
	}
}

void Delayed_data_callbacks::cancel()
{
	m_datanames.clear();
}

} // namespace PDI
