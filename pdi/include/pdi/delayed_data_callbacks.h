/*******************************************************************************
 * Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_DELAYED_DATA_CALLBACK_H_
#define PDI_DELAYED_DATA_CALLBACK_H_

#include <string>
#include <vector>

#include <pdi/pdi_fwd.h>
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>
#include <pdi/ref_any.h>

#include <data_descriptor_impl.h>

#include "global_context.h"

namespace PDI {

class PDI_EXPORT Delayed_data_callbacks
{
	/// friend class Global_context;
	std::vector<std::string> m_datanames;

	/// The context this descriptor is part of
	Global_context& m_context;

public:
	/// constructor used in:
	///     - the case of PDI_multi_expose
	///     - when a user want to define is own "Delayed_data_callbacks" object
	/// Remark: This constructor doesn't work if PDI_init is not called.
	Delayed_data_callbacks(Global_context& ctx)
		: m_datanames{}
		, m_context{ctx}
	{}

	// In the destructor, we need to throw an error message in case of the callback on the data doesn't work (trigger function)
	//  (example: error in the config.yml for a plugin, error due to external library incompatibility)
	~Delayed_data_callbacks() noexcept(false)
	{
		try {
			this->trigger();
		} catch (Error& e) {
			if (std::uncaught_exceptions()) {
				// An exception is throwing. Print simple message to avoid std::terminate.
				m_context.logger().error("Error in the destructor of Delayed_data_callbacks, {}", e.what());
			} else {
				// No exception is throwing. Throw an error.
				throw Error(e.status(), "Error in the destructor of Delayed_data_callbacks, {}", e.what());
			}
		} catch (const std::exception& e) {
			if (std::uncaught_exceptions()) {
				// An exception is throwing. Print simple message to avoid std::terminate.
				m_context.logger().error("Error in the destructor of Delayed_data_callbacks, {}", e.what());
			} else {
				// No exception is throwing. Throw an error.
				m_context.logger().error("Error in the destructor of Delayed_data_callbacks.");
				throw;
			}
		} catch (...) {
			if (std::uncaught_exceptions()) {
				// An exception is throwing. Print simple message to avoid std::terminate.
				m_context.logger().error("Error in the destructor of Delayed_data_callbacks.");
			} else {
				// No exception is throwing. Throw an error.
				m_context.logger().error("Error in the destructor of Delayed_data_callbacks.");
				throw;
			}
		}
	}

	// add element to the list
	void add_dataname(const std::string& name) { m_datanames.emplace_back(name); }

	// trigger_delayed_data_callbacks()
	void trigger()
	try {
		int i = 0;
		size_t number_of_elements = m_datanames.size();
		for (auto element_name: m_datanames) {
			m_context.logger().trace("Trigger data callback `{}' ({}/{})", element_name.c_str(), ++i, number_of_elements);
			m_context[element_name.c_str()].trigger_delayed_data_callbacks();
		}
		this->cancel(); // clear the m_datanames
	} catch (Error& e) {
		this->cancel(); // clear the m_datanames
		throw Error(e.status(), "Error in trigger data callback: `{}'", e.what());
	} catch (...) {
		this->cancel(); // clear the m_datanames
		throw;
	}

	// clear the element in the list
	void cancel() { m_datanames.clear(); }
}; // class Delayed_data_callbacks

} // namespace PDI
#endif // PDI_DELAYED_DATA_CALLBACK_H_
