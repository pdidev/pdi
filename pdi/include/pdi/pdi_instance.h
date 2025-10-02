/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_PDI_INSTANCE_H_
#define PDI_PDI_INSTANCE_H_

#include <functional>
#include <list>
#include <memory>
#include <string>
#include <unordered_map>

#include <pdi/pdi_fwd.h>
#include <pdi/data_store.h>
#include <pdi/logger.h>

namespace PDI {

class PDI_EXPORT Pdi_instance
{
public:
	Pdi_instance(PC_tree_t conf);

	~Pdi_instance();

	Data_store& data_store();

	/** 
	 * \return the main logger of PDI
	 */
	Logger& logger();

	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	void event(const char* name);

	/** Add an event callback
	 *
	 * \param[in] callback function to call when event is triggered
	 * \param[in] name the name of the event on which call the callback, if not specified it's called on any event
	 *
	 * \return a function that removes the callback
	 */
	std::function<void()> on_event(const std::function<void(const std::string&)>& callback, const std::string& name = {});

private:
	/**
	 *  Callbacks called after init
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void()>> m_init_callbacks;

	Data_store m_data_store;
};

} // namespace PDI

#endif // PDI_PDI_INSTANCE_H_
