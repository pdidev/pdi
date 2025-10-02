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

#ifndef PDI_TYPING_CONTEXT_H
#define PDI_TYPING_CONTEXT_H

#include <functional>
#include <memory>
#include <stack>
#include <string_view>
#include <unordered_map>

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/logger.h>

namespace PDI {

/** This is a typing frontend to a data store that adds a type before sharing data
 */
class PDI_EXPORT Data_store: public Context
{
public:
	Data_store(Logger& logger);

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Ref data(std::string_view name) const override;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Ref local_data(std::string_view name) const override { return {}; }

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Ref global_data(std::string_view name) const override;

	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	Datatype_template_sptr datatype(std::string_view name) const override;

	/** Shares some data with PDI
	 * \param ref a reference to the shared data
	 * \param read whether the stored reference should have read access
	 * \param write whether the stored reference should have write access
	 * \return the just shared buffer
	 */
	void* share(std::string_view name, Ref ref, bool read, bool write);

	/** Releases ownership of a data shared with PDI. PDI is then responsible to
	 * free the associated memory whenever necessary.
	 */
	void release();

	/** Reclaims ownership of a data buffer shared with PDI. PDI does not manage
	 * the buffer memory anymore.
	 * \return the address of the buffer
	 */
	void* reclaim();

	/** Adds new datatype to the context
	 *
	 * \param[in] name name of the datatype to add
	 * \param[in] parser function that creates new datatype_template from PC_tree_t
	 */
	void add_datatype(const std::string& name, Datatype_template_sptr type);

	/** Access the datatype template used to type raw pointers shared
	 *  through this descriptor
	 *
	 * \param data_name the name of the data 
	 * 
	 * \return the datatype template attached to the descriptor
	 */
	Datatype_template_sptr get_default_type(std::string_view data_name) const;

	/** Checks whether the data is a metadata
	 * 
	 * \param data_name the name of the data 
	 * 
	 */
	bool is_metadata(std::string_view data_name) const;

	/** Sets whether this describes a metadata or not
	 * \param data_name the name of the data 
	 * \param metadata whether data shared through this descriptor should
	 *        behave as a metadata
	 */
	void set_as_metadata(std::string_view data_name, bool metadata = false);

	/** Checks whether a given data has a default type set
	 *
	 * \returns true if empty.
	 */
	bool is_typed(std::string_view data_name);

	/** Store an untyped data with its registered datatype
	 * \param data_name the name of the data to store
	 * \param[in,out] data the data to store
	 * \param read whether read access is granted to other references
	 * \param write whether write access is granted to other references
	 */
	void store(std::string_view data_name, void* data, bool read, bool write);

	/** Registers a callback to be notified at initialization
	 *
	 * \param[in] callback function to call when data and metadata are loaded
	 *
	 * \return function that removes callback
	 */
	std::function<void()> on_init(const std::function<void()>& callback);

	/** Registers a callback to be notified on named event
	 *
	 * \param[in] callback function to call when event is called
	 * \param[in] name the name of the event on which call the callback, if not specified it's called on any event
	 *
	 * \return function that removes callback
	 */
	std::function<void()> on_event(const std::function<void(const std::string&)>& callback, const std::string& name = {});

	/** Registers a callback to be notified when new data is added to this context
	 *
	 * \param[in] callback function to call when data is added
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	std::function<void()> on_new_data(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {});

	/** Registers a callback to be notified when data is reclaimed/released
	 *
	 * \param[in] callback function to call when data is reclaimed/released
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	std::function<void()> on_data_remove(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {});

	/** Registers a callback to be notified when missing data is accessed
	 *
	 * \param[in] callback function to call when 
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	std::function<void()> on_missing_data(const std::function<void(const std::string&)>& callback, const std::string& name = {});

private:
	struct PDI_NO_EXPORT Data_descriptor {
		struct PDI_NO_EXPORT Ref_holder;

		/// References to the of this descriptor
		std::unique_ptr<Ref_holder> m_ref;

		Datatype_template_sptr m_type;

		std::string m_name;

		bool m_metadata;
		
		Data_descriptor(Data_descriptor&&);
		
		Data_descriptor(const Data_descriptor&);
		
		Data_descriptor& operator= (const Data_descriptor&);
		
		Data_descriptor& operator= (Data_descriptor&&);
		
		~Data_descriptor();
	};

	/// Calls init callbacks
	void call_init_callbacks() const;

	/** Calls data callbacks
	 *  \param name name of the shared descriptor
	 *  \param ref shared reference
	 */
	void call_data_callbacks(const std::string& name, Ref ref) const;

	/** Calls data remove callbacks
	 *  \param name name of the descriptor that will be reclaimed/released
	 *  \param ref reference that will be reclaimed/released
	 */
	void call_data_remove_callbacks(const std::string& name, Ref ref) const;

	/** Calls event callbacks
	 *  \param name name of the event
	 */
	void call_event_callbacks(const std::string& name) const;

	/** Calls empty desc callbacks
	 *  \param name name of the accessed descriptor
	 */
	void call_empty_desc_access_callbacks(const std::string& name) const;

	std::unique_ptr<Logger> m_logger;

	std::unordered_set<std::string, Data_descriptor> m_global_data;

	std::unordered_set<std::string, Datatype_template_sptr> m_datatypes;

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

	/**
	 *  Callbacks called when any data is available
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void(const std::string&, Ref)>> m_data_callbacks;

	/**
	 */
	std::list<std::function<void()>> m_init_callbacks;

	/**
	 *  Callbacks called when specified data is available.
	 *
	 *  This must be an ordered multimap, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::multimap<std::string, std::function<void(const std::string&, Ref)>> m_named_data_callbacks;

	/**
	 *  Callbacks called when any data is reclaimed/released.
	 *
	 *  This must be a list, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::list<std::function<void(const std::string&, Ref)>> m_data_remove_callbacks;

	/**
	 *  Callbacks called when specified data is reclaimed/released.
	 *
	 *  This must be an ordered multimap, because valid iterators are needed to properly remove the callback by plugin
	 */
	std::multimap<std::string, std::function<void(const std::string&, Ref)>> m_named_data_remove_callbacks;

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
};

} // namespace PDI

#endif // PDI_TYPING_CONTEXT_H
