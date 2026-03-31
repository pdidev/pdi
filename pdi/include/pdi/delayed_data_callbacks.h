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

#ifndef PDI_DELAYED_DATA_CALLBACK_H_
#define PDI_DELAYED_DATA_CALLBACK_H_

#include <string>
#include <vector>

#include <pdi/pdi_fwd.h>
#include "pdi/context.h"

#include "global_context.h"

namespace PDI {

class PDI_EXPORT Delayed_data_callbacks
{
	/// list of names of the data
	std::vector<std::string> m_datanames;

	/// The context where the list of data is a part of
	Global_context& m_context;

public:
	/// constructor
	Delayed_data_callbacks(Global_context& ctx);

	/// In the destructor, we need to throw an error message in case of the callback on the data doesn't work (trigger function)
	///  (example: error in the config.yml for a plugin, error due to external library incompatibility)
	~Delayed_data_callbacks() noexcept(false);

	/// add element "name" to  "m_datanames"
	void add_dataname(const std::string& name);

	/// Trigger data callback for all elements in "m_datanames"
	void trigger();

	/// clear m_datanames
	void cancel();

}; // class Delayed_data_callbacks

} // namespace PDI
#endif // PDI_DELAYED_DATA_CALLBACK_H_
