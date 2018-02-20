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

#ifndef PDI_DATA_REF_H_
#define PDI_DATA_REF_H_

#include <cassert>
#include <functional>
#include <memory>
#include <unordered_map>

#include <pdi/fwd.h>
#include <pdi/data_type.h>
#include <pdi/status.h>


namespace PDI
{

/** A common base for all references, whatever their access privileges in
 * order to ensure they share the same Data_content and can access each others.
 */
class PDI_EXPORT Data_ref_base
{
public:
	/** Constructs a null reference
	 */
	Data_ref_base():
		m_content(nullptr)
	{
	}
	
	Data_ref_base(const Data_ref_base &) = delete;
	
	Data_ref_base(Data_ref_base &&) = delete;
	
	Data_ref_base &operator = (const Data_ref_base &) = delete;
	
	Data_ref_base &operator = (Data_ref_base &&) = delete;
	
	/** accesses the type of the referenced raw data
	 */
	const Data_type &type() const;
	
protected:
	/** Manipulate and grant access to a buffer depending on the remaining right access (read/write).
	 */
	class PDI_EXPORT Ref_count
	{
	public:
		/// buffer that contains data
		void *m_buffer;
		
		/// The function to call to deallocate the buffer memory
		std::function<void(void *)> m_delete;
		
		/// type of the data inside the buffer
		Data_type_uptr m_type;
		
		/// number of references to this content
		int m_owners;
		
		/// number of locks preventing read access
		int m_read_locks;
		
		/// number of locks preventing write access (should always remain between 0 & 2 inclusive w. current implem.)
		int m_write_locks;
		
		/// Nullification notifications registered on this instance
		std::unordered_map<const Data_ref_base *, std::function<void(Data_ref)> > m_notifications;
		
		Ref_count() = delete;
		
		Ref_count (const Ref_count &) = delete;
		
		Ref_count (Ref_count &&) = delete;
		
		/** Constructs the content
		 * \param buffer the actual content
		 * \param deleter the function to use to deallocate the buffer memory
		 * \param type the content type, this takes ownership of the type
		 * \param readable whether it is allowed to read the content
		 * \param writable whether it is allowed to write the content
		 */
		Ref_count (void *buffer, std::function<void(void *)> deleter, Data_type_uptr type, bool readable, bool writable):
			m_buffer{buffer},
			m_delete{deleter},
			m_type{std::move(type)},
			m_owners{0},
			m_read_locks{readable ? 0 : 1},
			m_write_locks{writable ? 0 : 1}
		{
			assert(buffer);
		}
		
		~Ref_count()
		{
			if (m_buffer) m_delete(this->m_buffer);
			assert(!m_owners);
			assert(m_read_locks == 0 || m_read_locks == 1);
			assert(m_write_locks == 0 || m_write_locks == 1);
			assert(m_notifications.empty());
		}
		
	}; // class Data_content
	
	/** Function to access the content from a reference with different access right
	 */
	static Ref_count *get_content(const Data_ref_base &other)
	{
		return other.m_content;
	}
	
	static Data_ref do_copy(Data_r_ref ref);
	
	/** Pointer on the data content, can be null if the ref is null
	 */
	mutable Ref_count *m_content;
	
}; // class Data_ref_base


template<bool R, bool W>
struct Ref_access
{
	typedef void type;
};

template<bool R>
struct Ref_access<R, true>
{
	typedef void* type;
};

template<>
struct Ref_access<true, false>
{
	typedef const void* type;
};


/** A dynamically typed reference to data with automatic memory management and
 * read/write locking semantic.
 *
 * Data_A_ref is a smart pointer that features:
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
template<bool R, bool W>
class PDI_EXPORT Data_A_ref:
	public Data_ref_base
{
public:
	/** Constructs a null reference
	 */
	Data_A_ref() = default;
	
	/** Copies an existing reference
	 *
	 * if the requested rights can not be granted, the reference is made null
	 *
	 * \param other the ref to copy
	 */
	Data_A_ref(const Data_A_ref &other):
		Data_ref_base()
	{
		link(get_content(other));
	}
	
	/** Copies an existing reference with different privileges
	 *
	 * if the requested rights can not be granted, the reference is made null
	 *
	 * \param other the ref to copy
	 */
	template<bool OR, bool OW>
	Data_A_ref(const Data_A_ref<OR, OW> &other):
		Data_ref_base()
	{
		link(get_content(other));
	}
	
	/** Moves an existing reference
	 * \param other the ref to copy
	 */
	Data_A_ref(Data_A_ref &&other):
		Data_ref_base()
	{
		if (!other.m_content || !other.m_content->m_buffer) return;
		// the other ref notification disappears
		other.m_content->m_notifications.erase(&other);
		// since we get the same privileges as those we release we can just steal the content
		m_content = other.m_content;
		other.m_content = nullptr;
	}
	
	/** Creates a reference to currently unreferenced data
	 *
	 * \param data the raw data to reference
	 * \param freefunc the function to use to free the data buffer
	 * \param type the type of the referenced data, ownership will be taken
	 * \param readable the maximum allowed access to the underlying content
	 * \param writable the maximum allowed access to the underlying content
	 */
	Data_A_ref(void *data, std::function<void(void *)> freefunc, Data_type_uptr type, bool readable, bool writable):
		Data_ref_base()
	{
		if (data) link(new Ref_count (data, freefunc, std::move(type), readable, writable));
	}
	
	/** Destructor
	 */
	~Data_A_ref()
	{
		reset();
	}
	
	/** Offers access to the referenced raw data
	 *
	 * \return a pointer to the referenced raw data
	 */
	operator typename Ref_access<R,W>::type() const
	{
		return get();
	}
	
	/** Offers access to the referenced raw data
	 *
	 * \return a pointer to the referenced raw data
	 */
	typename Ref_access<R,W>::type get() const
	{
		if (is_null()) throw Error{PDI_ERR_RIGHT, "Trying to dereference a null reference"};
		return m_content->m_buffer;
	}
	
	/** Checks whether this is a null reference
	 *
	 * \return whether this reference is non-null
	 */
	operator bool () const
	{
		return !is_null();
	}
	
	/** Nullify the reference
	 */
	void reset()
	{
		if (m_content) unlink();
	}
	
	/** Makes a copy of the raw content behind this reference and returns a new 
	 * reference
	 *
	 * \return a new reference to a copy of the raw data this references
	 */
	Data_ref copy() {
		return do_copy(*this);
	}
	
	/** Releases ownership of the referenced raw data by nullifying all existing
	 *  references.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void *release()
	{
		if (is_null()) return nullptr;
		
		// notify everybody of the nullification
		while (!m_content->m_notifications.empty()) {
			// get the key of a notification
			const Data_ref_base *key = m_content->m_notifications.begin()->first;
			// call this notification, this might invalidate any iterator
			m_content->m_notifications.begin()->second(*this);
			// remove the notification we just called
			m_content->m_notifications.erase(key);
		}
		
		void *result = m_content->m_buffer;
		m_content->m_buffer = nullptr;
		
		unlink();
		
		return result;
	}
	
	/** Registers a nullification callback
	 *
	 * \param notifier the function to call when this reference becomes null
	 */
	void on_nullify(std::function<void(Data_ref)> notifier)
	{
		if (!is_null()) m_content->m_notifications[this] = notifier;
	}
	
private:
	/** Tests if the reference is null.
	 *
	 * Ensures m_content is null if the reference is to fasten deletion of the
	 * shared Data_content
	 *
	 * \return Whether the reference is null
	 */
	bool is_null() const
	{
		if (!m_content) return true;
		if (!m_content->m_buffer) {
			unlink();
			return true;
		}
		return false;
	}
	
	/** Unlink this reference from its content
	 *
	 * Can only be done on a reference with content
	 */
	void unlink() const
	{
		assert(m_content);
		m_content->m_notifications.erase(this);
		if (R || W) --m_content->m_write_locks;
		if (W) --m_content->m_read_locks;
		--m_content->m_owners;
		if (!m_content->m_owners) delete m_content;
		m_content = nullptr;
	}
	
	/** Tries to link this reference to a content, leaves it null if privileges
	 *  can not be granted
	 *
	 * Can only be done on a null reference
	 *
	 * \param content the content to link to
	 */
	void link(Ref_count *content)
	{
		assert(!m_content);
		if (!content || !content->m_buffer) return; // null ref
		if (R && content->m_read_locks) return;
		if (W && content->m_write_locks) return;
		m_content = content;
		++m_content->m_owners;
		if (R || W) ++m_content->m_write_locks;
		if (W) ++m_content->m_read_locks;
	}
	
};

} // namespace PDI

#endif //  PDI_DATA_REF_H_
