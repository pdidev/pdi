/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

//The following is used for doxygen documentation:
/**
* \file state.h
* \brief details of the strucutures that store data, metadata, ...
* \author Julien Bigot (CEA) <julien.bigot@cea.fr>
*/

#ifndef PDI_STATE_H_
#define PDI_STATE_H_

#include <mpi.h>

#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/fwd.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>


class PDI_EXPORT PDI_state_t
{
public:
	/** The store containing all data descriptors
	 *
	 * Implemented as a wrapper for a map
	 */
	class PDI_EXPORT Descriptors_store
	{
	public:
		/** An iterator used to go through the descriptor store.
		 *
		 * Implemented as a wrapper for a map iterator that hides the key part.
		 */
		class Descriptor_iterator
		{
		public:
			PDI::Data_descriptor &operator-> ()
			{
				return m_data->second;
			}
			PDI::Data_descriptor &operator* ()
			{
				return m_data->second;
			}
			Descriptor_iterator &operator++ ()
			{
				++m_data;
				return *this;
			}
			bool operator!= (const Descriptor_iterator &o)
			{
				return (m_data != o.m_data);
			}
			friend class Descriptors_store;
		private:
			std::unordered_map<std::string, PDI::Data_descriptor>::iterator m_data;
			Descriptor_iterator(const std::unordered_map<std::string, PDI::Data_descriptor>::iterator &data): m_data(data) {}
			Descriptor_iterator(std::unordered_map<std::string, PDI::Data_descriptor>::iterator &&data): m_data(std::move(data)) {}
		};
		
		typedef std::unordered_map<std::string, PDI::Data_descriptor> Mapped_type;
		
		Descriptor_iterator begin()
		{
			return m_data.begin();
		}
		
		Descriptor_iterator end()
		{
			return m_data.end();
		}
		
		Descriptors_store(Mapped_type &data): m_data(data) {}
		
	private:
		Mapped_type &m_data;
		
	};
	
	Descriptors_store descriptors()
	{
		return m_descriptors;
	}
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	*/
	PDI::Data_descriptor &desc(const std::string &name);
	
	/** Accesses the descriptor for a specific name. Might be uninitialized
	*/
	PDI::Data_descriptor &desc(const char *name);
	
	/// The name of the ongoing transaction or "" if none
	std::string transaction;
	
	/// List of data that are
	std::unordered_set<std::string> transaction_data;
	
	/// The actual loaded plugins
	std::unordered_map<std::string, std::shared_ptr<PDI_plugin_t>> plugins;
	
private:
	/// Descriptors of the data
	std::unordered_map<std::string, PDI::Data_descriptor> m_descriptors;
	
};


/// The main state of the PDI implementation
extern PDI_state_t PDI_EXPORT PDI_state;


#endif // PDI_STATE_H_
