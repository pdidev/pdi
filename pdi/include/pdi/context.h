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

#ifndef PDI_CONTEXT_H_
#define PDI_CONTEXT_H_

#include <functional>
#include <memory>
#include <string>
#include <unordered_map>

#include <pdi/pdi_fwd.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>
#include <pdi/ref_any.h>


namespace PDI {

class PDI_EXPORT Context
{
public:
	/** An iterator used to go through the descriptor store.
	 */
	class Iterator
	{
		friend class Context;
		/// The iterator this wraps
		std::unordered_map<std::string, std::unique_ptr<Data_descriptor>>::iterator m_data;
		Iterator(const std::unordered_map<std::string, std::unique_ptr<Data_descriptor>>::iterator& data);
		Iterator(std::unordered_map<std::string, std::unique_ptr<Data_descriptor>>::iterator&& data);
	public:
		Data_descriptor* operator-> ();
		Data_descriptor& operator* ();
		Iterator& operator++ ();
		bool operator!= (const Iterator&);
	};
	
	/** A function that parses a PC_tree_t to create a datatype_template
	 */
	typedef std::function<Datatype_template_uptr(Context&, PC_tree_t)> Datatype_template_parser;
	
protected:
	Iterator get_iterator(const std::unordered_map<std::string, std::unique_ptr<Data_descriptor>>::iterator& data);
	
	Iterator get_iterator(std::unordered_map<std::string, std::unique_ptr<Data_descriptor>>::iterator&& data);
	
public:
	virtual ~Context();
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& desc(const std::string& name) = 0;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& desc(const char* name) = 0;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& operator[](const std::string& name) = 0;
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	 */
	virtual Data_descriptor& operator[](const char* name) = 0;
	
	/** Returns an iterator on the first descriptor
	 */
	virtual Iterator begin() = 0;
	
	/** Returns an iterator past the last descriptor
	 */
	virtual Iterator end() = 0;
	
	/** Triggers a PDI "event"
	 * \param[in] name the event name
	 */
	virtual void event(const char* name) = 0;
	
	virtual Logger_sptr logger() const = 0;
	
	/** Creates a new datatype template from a paraconf-style config
	 * \param[in] node the configuration to read
	 *
	 * \return the type generated
	 */
	virtual Datatype_template_uptr datatype(PC_tree_t node) = 0;
	
	/** Adds new datatype parser to the context
	 *
	 * \param[in] name name of the datatype to add
	 * \param[in] parser function that creates new datatype_template from PC_tree_t
	 */
	virtual void add_datatype(const std::string& name, Datatype_template_parser parser) = 0;
	
	/** Adds new init callback to context
	 *
	 * \param[in] callback function to call when data and metadata are loaded
	 *
	 * \return function that removes callback
	 */
	virtual std::function<void()> add_init_callback(const std::function<void()>& callback) = 0;
	
	/** Adds new data callback to context
	 *
	 * \param[in] callback function to call when data is being available
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	virtual std::function<void()> add_data_callback(const std::function<void(const std::string&, Ref)>& callback, const std::string& name = {}) = 0;
	
	/** Adds new event callback to context
	 *
	 * \param[in] callback function to call when event is called
	 * \param[in] name the name of the event on which call the callback, if not specified it's called on any event
	 *
	 * \return function that removes callback
	 */
	virtual std::function<void()> add_event_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {}) = 0;
	
	/** Adds new empty desc access callback to context
	 *
	 * \param[in] callback function to call when event is called
	 * \param[in] name the name of the data on which call the callback, if not specified it's called on any data
	 *
	 * \return function that removes callback
	 */
	virtual std::function<void()> add_empty_desc_access_callback(const std::function<void(const std::string&)>& callback, const std::string& name = {}) = 0;
	
	virtual void finalize_and_exit() = 0;
};

} // namespace PDI

#endif // PDI_CONTEXT_H_
