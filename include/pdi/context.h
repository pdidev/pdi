/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/pdi_fwd.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>
#include <pdi/ref_any.h>
#include <pdi/logger.h>


namespace PDI {

typedef std::function<Datatype_template_uptr(const PC_tree_t&)> Datatype_template_func;

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
	
	/**
	 *  Returns the datatype_template stored in context
	 *
	 * \param[in] name the datatype name
	 */
	virtual Datatype_template_func& datatype(const std::string& name) = 0;
	
	/**
	 *  Adds new datatype to the context
	 *
	 * \param[in] datatype_name name of the datatype to add
	 * \param[in] datatype_func function that creates new datatype_template from PC_tree_t
	 */
	virtual void add_datatype(const std::string& datatype_name, Datatype_template_func datatype_func) = 0;
	
};

} // namespace PDI

#endif // PDI_CONTEXT_H_
