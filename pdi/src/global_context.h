/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_GLOBAL_CONTEXT_H_
#define PDI_GLOBAL_CONTEXT_H_

#include <list>
#include <map>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>

#include "pdi/context.h"
#include "pdi/context_proxy.h"
#include "pdi/pdi_fwd.h"
#include "pdi/data_descriptor.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"


namespace PDI {

class PDI_EXPORT Global_context : public Context
{
private:
	friend class Data_descriptor_impl;
	
	/// Global logger of PDI, should be constructed first, destroyed last
	Logger_sptr m_logger;
	
	/// Context proxy map, should be destroyed after m_plugins
	std::unordered_map<std::string, Context_proxy> m_context_proxy;

	/// The singleton Context instance
	static std::unique_ptr<Global_context> s_context;
	
	/// Datatype_template constructors available in PDI
	std::unordered_map<std::string, Datatype_template_parser> m_datatype_parsers;
	
	/// Descriptors of the data
	std::unordered_map<std::string, std::unique_ptr<Data_descriptor>> m_descriptors;

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

    /// The loaded plugins - need to be last (to guarantee proper destroy order)
	std::unordered_map<std::string, std::unique_ptr<Plugin>> m_plugins;
    
	Global_context(const Global_context&) = delete;
	
	Global_context(Global_context&&) = delete;
	
public:
	static void init(PC_tree_t conf);
	
	static bool initialized();
	
	static Global_context& context();
	
	static void finalize();
	
	Global_context(PC_tree_t conf);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const std::string& name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& desc(const char* name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[](const std::string& name) override;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Data_descriptor& operator[](const char* name) override;
	
	/** Returns an iterator on the first descriptor
	 */
	Iterator begin() override;
	
	/** Returns an iterator past the last descriptor
	 */
	Iterator end() override;
	
	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	void event(const char* name) override;
	
	Logger_sptr logger() const override;
	
	Datatype_template_uptr datatype(PC_tree_t node) override;
	
	void add_datatype(const std::string&, Datatype_template_parser) override;
	
	std::function<void()> add_init_callback(const std::function<void()>& callback) override;
	
	std::function<void()> add_data_callback(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {}) override;
	
	std::function<void()> add_event_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {}) override;
	
	std::function<void()> add_empty_desc_access_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {}) override;
};

} // namespace PDI

#endif // PDI_GLOBAL_CONTEXT_H_