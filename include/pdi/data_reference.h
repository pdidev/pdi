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

#ifndef DATA_REF_H__
#define DATA_REF_H__

#include <cassert>
#include <functional>
#include <memory>
#include <unordered_map>

#include "pdi.h"

#include "pdi/datatype.h"
#include "pdi/data_reference_fwd.h"

namespace PDI
{

/** A dynamically typed reference to data with automatic memory management and
 * read/write locking semantic.
 *
 * Data_ref_base is a smart pointer that features:
 * - a dynamic type system,
 * - garbage collection mechanism similar to std::shared_ptr,
 * - a read/write locking mechanism similar to std::shared_mutex,
 * - a release system that nullifies all existing references to the raw data,
 * - a notification system to be notified when a reference is going to be nullified.
 *
 * \warning As of now, and unlike std::shared_ptr, the lock system can not be
 * relied upon in a multithreaded environment.
 *
 * \author Corentin Roussel (CEA) <corentin.roussel@cea.fr>
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 */
class Data_ref_base
{
public:
	/** Constructs a null ref
	 */
	Data_ref_base() noexcept:
		m_content(nullptr)
	{
	}
	
	/** Offers access to the referenced raw data
	 * \return a pointer to the referenced raw data
	 */
	operator void *() const
	{
		return get();
	}
	
	/** Offers access to the referenced raw data
	 * \return a pointer to the referenced raw data
	 */
	void *get() const
	{
		if (!m_content) return nullptr;
		return m_content->m_buffer;
	}
	
	/** Checks whether this is a null reference
	 * \return whether this reference is non-null
	 */
	operator bool () const
	{
		return get();
	}
	
	/** accesses the type of the referenced raw data
	 */
	const PDI_datatype_t &type() const
	{
		if (!m_content || !m_content->m_buffer) return PDI_UNDEF_TYPE;
		return m_content->m_type;
	}
	
	/** Releases ownership of the referenced raw data by replacing all existing
	 *  references by references to a copy.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void *copy_release()
	{
		if (!m_content || !m_content->m_buffer) return nullptr;
		
		//TODO: error handling if data is not readable
		
		//TODO: handle errors
		PDI_datatype_t newtype; PDI_datatype_densify(&newtype, &m_content->m_type);
		
		//TODO: handle errors
		size_t dsize; PDI_datatype_buffersize(&m_content->m_type, &dsize);
		void *newbuffer = operator new(dsize, std::nothrow);
		PDI_buffer_copy(newbuffer,
										&newtype,
										m_content->m_buffer,
										&m_content->m_type);
										
		// replace the buffer
		void *oldbuffer = m_content->m_buffer;
		m_content->m_buffer = newbuffer;
		
		// replace the destroyer
		m_content->m_delete = [](void* d){operator delete(d);};
		
		// replace the type
		//TODO: handle errors
		PDI_datatype_destroy(&m_content->m_type);
		m_content->m_type = newtype;
		
		return oldbuffer;
	}
	
	/** Registers a nullification callback
	 */
	void on_nullify(std::function<void(Data_ref)> notifier)
	{
		m_content->m_notifications[this] = move(notifier);
	}
	
protected:
	/** Manipulate and grant access to a buffer depending on the remaining right access (read/write).
	 */
	class Data_content
	{
	public:
		/// buffer that contains data
		void *m_buffer;
		
		std::function<void(void*)> m_delete;
		
		/// type of the data inside the buffer
		PDI_datatype_t m_type;
		
		/// number of references to this content
		int m_owners;
		
		/// number of locks preventing read access
		int m_read_locks;
		
		/// number of locks preventing write access (should always remain between 0 & 2 inclusive w. current implem.)
		int m_write_locks;
		
		/// references to this instance
		std::unordered_map< Data_ref_base*, std::function<void(Data_ref)> > m_notifications;
		
		Data_content() = delete;
		
		Data_content(const Data_content &) = delete;
		
		Data_content(Data_content &&) = delete;
		
		virtual ~Data_content()
		{
			assert(m_notifications.empty());
			assert(!m_owners);
			PDI_datatype_destroy(&m_type);
			assert(m_read_locks == 0 || m_read_locks == 1);
			assert(m_write_locks == 0 || m_write_locks == 1);
			m_delete(this->m_buffer);
		}
		
		/** Constrcuts a referenceable version of the content
		* \param buffer the actual content
		* \param type the content type, this takes ownership of the type
		* \param readable whether it is allowed to read the content
		* \param writable whether it is allowed to write the content
		*/
		Data_content(void *buffer, std::function<void(void*)> deleter, const PDI_datatype_t &type, bool readable, bool writable):
			m_buffer(buffer),
			m_delete(deleter),
			m_type(type),
			m_owners(0),
			m_read_locks(readable ? 0 : 1),
			m_write_locks(writable ? 0 : 1)
		{
			assert(buffer);
		}
		
	}; // class Data_content

	/** Tries to link this reference to a content
	 * \param content the content to link to
	 */
	template<bool R, bool W>
	void link(Data_content *content)
	{
		assert(!m_content);
		if ( !content || !content->m_buffer) return;
		if ( R && content->m_read_locks ) return;
		if ( W && content->m_write_locks ) return;
		m_content = content;
		++m_content->m_owners;
		if ( R || W ) ++m_content->m_write_locks;
		if ( W ) ++m_content->m_read_locks;
	}
	
	/** Tries to link this reference to the same content as another ref
	 */
	template<bool R, bool W>
	void link(const Data_ref_base& other)
	{
		link<R,W>(other.m_content);
	}
	
	/** Unlinks this reference from a content (nullifies it)
	 */
	template<bool R, bool W>
	void unlink()
	{
		if (!m_content) return;
		if ( R || W ) --m_content->m_write_locks;
		if ( W ) --m_content->m_read_locks;
		++m_content->m_owners;
		m_content->m_notifications.erase(this);
		if ( !m_content->m_owners ) delete m_content;
		m_content = nullptr;
	}
	
	/** Pointer on the data content, null if the ref is null
	 */
	Data_content *m_content;
	
}; // class Data_ref_base

template<bool R, bool W>
class Data_A_ref:
		public Data_ref_base
{
public:
	Data_A_ref() = default;
	
	/** Copies an existing reference
	 * \param other the ref to copy
	 */
	Data_A_ref(const Data_ref_base &other):
			Data_ref_base()
	{
		link<R,W>(other);
	}
	
	/** Copies an existing reference
	 * \param other the ref to copy
	 */
	Data_A_ref(const Data_A_ref &other):
			Data_ref_base()
	{
		link<R,W>(other);
	}
	
	/** Creates a reference to currently unreferenced data
	 * \param data the raw data to reference
	 * \param freefunc the function to use to free the data buffer
	 * \param type the type of the referenced data, ownership will be taken
	 * \param readable the maximum allowed access to the underlying content
	 * \param writable the maximum allowed access to the underlying content
	 */
	Data_A_ref(void *data, std::function<void(void*)> freefunc, const PDI_datatype_t &type, bool readable, bool writable) noexcept:
			Data_ref_base()
	{
		if (data) link<R,W>(new Data_content(data, freefunc, type, readable, writable));
	}
	
	/** Copies an existing reference into this one
	 * \param other the ref to copy
	 * \return *this
	 */
	Data_A_ref &operator= (const Data_ref_base &other)
	{
		if (this == &other) return *this;
		
		unlink<R,W>();
		link<R,W>(other);
		
		return *this;
	}
	
	/** Destructor
	 */
	~Data_A_ref()
	{
		unlink<R,W>();
	}
	
	/** Releases ownership of the referenced raw data by nullifying all existing
	 *  references.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void *null_release()
	{
		if (!m_content || !m_content->m_buffer) return nullptr;
		
		// notify everybody of the nullification
		while ( !m_content->m_notifications.empty() ) {
			Data_ref_base* key = m_content->m_notifications.begin()->first;
			m_content->m_notifications.begin()->second(*this);
			m_content->m_notifications.erase(key);
		}
		
		void *result = m_content->m_buffer;
		m_content->m_buffer = nullptr;
		
		unlink<R,W>();
		
		return result;
	}
	
};

} // namespace PDI

#endif //  DATA_REF_H__
