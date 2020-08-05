/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_CALLBACKS_H_
#define PDI_CALLBACKS_H_

#include <functional>
#include <map>
#include <list>
#include <string>

#include "pdi/pdi_fwd.h"
#include "pdi/ref_any.h"

namespace PDI {

class PDI_EXPORT Callbacks
{

	/// Context of callbacks
	Context& m_context;
	
	/**
	 *  Callbacks called after init
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void()>> m_init_callbacks;
	
	/**
	 *  Callbacks called when any data is available
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void(const std::string&, Ref)>> m_data_callbacks;
	
	/**
	 *  Callbacks called when specified data is available.
	 *
	 *  This must be an ordered multimap, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::multimap<std::string, std::function<void(const std::string&, Ref)>> m_named_data_callbacks;
	
	/**
	 *  Callbacks called on any event
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void(const std::string&)>> m_event_callbacks;
	
	/**
	 *  Callbacks called on specified event
	 *
	 *  This must be an ordered multimap, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::multimap<std::string, std::function<void(const std::string&)>> m_named_event_callbacks;
	
	/**
	 *  Callbacks called on any empty desc access
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void(const std::string&)>> m_empty_desc_access_callbacks;
	
	/**
	 *  Callbacks called on specified empty desc access
	 *
	 *  This must be an ordered multimap, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::multimap<std::string, std::function<void(const std::string&)>> m_named_empty_desc_access_callbacks;
	
public:

	Callbacks(Context& ctx);
	
	/** Adds new init callback to context
	 *
	 * \param[in] callback function to call when data and metadata are loaded
	 *
	 * \return function that removes callback
	 */
	std::function<void()> add_init_callback(const std::function<void()>& callback);
	
	/** Adds new data callback to context
	 *
	 * \param[in] callback function to call when data is being available
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	std::function<void()> add_data_callback(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {});
	
	/** Adds new event callback to context
	 *
	 * \param[in] callback function to call when event is called
	 * \param[in] name the name of the event on which call the callback, if not specified it's called on any event
	 *
	 * \return function that removes callback
	 */
	std::function<void()> add_event_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {});
	
	/** Adds new empty desc access callback to context
	 *
	 * \param[in] callback function to call when event is called
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	std::function<void()> add_empty_desc_access_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {});
	
	/// Calls init callbacks
	void call_init_callbacks() const;
	
	/** Calls data callbacks
	 *  \param name name of the shared descriptor
	 *  \param ref shared reference
	 */
	void call_data_callbacks(const std::string& name, Ref ref) const;
	
	/** Calls event callbacks
	 *  \param name name of the event
	 */
	void call_event_callbacks(const std::string& name) const;
	
	/** Calls empty desc callbacks
	 *  \param name name of the accessed descriptor
	 */
	void call_empty_desc_access_callbacks(const std::string& name) const;
	
};

} // namespace PDI


#endif //PDI_CALLBACKS_H_