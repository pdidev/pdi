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

#ifndef PDI_REF_ANY_H_
#define PDI_REF_ANY_H_

#include <cassert>
#include <functional>
#include <memory>
#include <new>
#include <unordered_map>

#include <pdi/pdi_fwd.h>
#include <pdi/datatype.h>
#include <pdi/error.h>


namespace PDI {

/** A common base for all references, whatever their access privileges in
 * order to ensure they share the same Data_content and can access each others.
 */
class PDI_EXPORT Ref_base
{
protected:
	/** Manipulate and grant access to a buffer depending on the remaining right access (read/write).
	 */
	struct PDI_NO_EXPORT Referenced {
		/// buffer that contains data
		void* m_buffer;
		
		/// The function to call to deallocate the buffer memory
		std::function<void(void*)> m_delete;
		
		/// type of the data inside the buffer
		Datatype_uptr m_type;
		
		/// number of references to this content
		int m_owners;
		
		/// number of locks preventing read access
		int m_read_locks;
		
		/// number of locks preventing write access (should always remain between 0 & 2 inclusive w. current implem.)
		int m_write_locks;
		
		/// Nullification notifications registered on this instance
		std::unordered_map<const Ref_base*, std::function<void(Ref)> > m_notifications;
		
		
		Referenced() = delete;
		
		Referenced(const Referenced&) = delete;
		
		Referenced(Referenced&&) = delete;
		
		/** Constructs the content
		 * \param buffer the actual content
		 * \param deleter the function to use to deallocate the buffer memory
		 * \param type the content type, this takes ownership of the type
		 * \param readable whether it is allowed to read the content
		 * \param writable whether it is allowed to write the content
		 */
		Referenced(void* buffer, std::function<void(void*)> deleter, Datatype_uptr type, bool readable, bool writable) noexcept:
			m_buffer{buffer},
		         m_delete{std::move(deleter)},
		         m_type{std::move(type)},
		         m_owners{0},
		         m_read_locks{readable ? 0 : 1},
		         m_write_locks{writable ? 0 : 1}
		{
			assert(buffer);
		}
		
		~Referenced()
		{
			if (m_buffer) m_delete(this->m_buffer);
			assert(!m_owners);
			assert(m_read_locks == 0 || m_read_locks == 1);
			assert(m_write_locks == 0 || m_write_locks == 1);
			assert(m_notifications.empty());
		}
		
	}; // class Ref_count
	
	
	/** Pointer on the data content, can be null if the ref is null
	 */
	mutable Referenced* m_content;
	
	
	/** Function to access the content from a reference with different access right
	 */
	static Referenced PDI_NO_EXPORT* get_content(const Ref_base& other) noexcept
	{
		if ( !other.m_content ) return NULL;
		if ( !other.m_content->m_buffer ) return NULL;
		return other.m_content;
	}
	
	/** Symbol should not be exported, but it required to force
	 *  generation of all 4 variants of `Ref_any::copy`
	 */
	static Ref do_copy(Ref_r ref);
	
	/** Constructs a null reference
	 */
	Ref_base() noexcept:
		m_content(nullptr)
	{}
	
	Ref_base(const Ref_base&) = delete;
	
	Ref_base(Ref_base&&) = delete;
	
	Ref_base& operator = (const Ref_base&) = delete;
	
	Ref_base& operator = (Ref_base&&) = delete;
	
public:
	/** accesses the type of the referenced raw data
	 */
	const Datatype& type() const noexcept;
	
	size_t hash() const noexcept
	{
		return std::hash<Referenced*>()(get_content(*this));
	}
	
}; // class Data_ref_base


template<bool R, bool W>
struct Ref_access {
	typedef void type;
};

template<bool R>
struct Ref_access<R, true> {
	typedef void* type;
};

template<>
struct Ref_access<true, false> {
	typedef const void* type;
};


/** A dynamically typed reference to data with automatic memory management and
 * read/write locking semantic.
 *
 * Ref_any is a smart pointer that features:
 * - a dynamic type system,
 * - garbage collection mechanism similar to std::shared_ptr,
 * - a read/write locking mechanism similar to std::shared_mutex,
 * - a release system that nullifies all existing references to the raw data,
 * - a notification system to be notified when a reference is going to be nullified.
 *
 * \warning As of now, and unlike std::shared_ptr, the lock system can not be
 * relied upon in a multithreaded environment.
 */
template<bool R, bool W>
class PDI_EXPORT Ref_any:
	public Ref_base
{
public:
	/** Constructs a null reference
	 */
	Ref_any() = default;
	
	/** Copies an existing reference
	 *
	 * if the requested rights can not be granted, the reference is made null
	 *
	 * \param other the ref to copy
	 */
	Ref_any(const Ref_any& other) noexcept:
		Ref_base()
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
	Ref_any(const Ref_any<OR, OW>& other) noexcept:
		Ref_base()
	{
		link(get_content(other));
	}
	
	/** Moves an existing reference
	 * \param other the ref to copy
	 */
	Ref_any(Ref_any&& other) noexcept:
		Ref_base()
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
	Ref_any(void* data, std::function<void(void*)> freefunc, Datatype_uptr type, bool readable, bool writable):
		Ref_base()
	{
		if (type->datasize() && !data && (readable||writable)) {
			throw Error{PDI_ERR_TYPE, "Referencing null data with non-null size"};
		}
		if (data) link(new Referenced(data, freefunc, std::move(type), readable, writable));
	}
	
	/** Destructor
	 */
	~Ref_any()
	{
		reset();
	}
	
	bool operator== (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content == get_content(o);
	}
	
	bool operator!= (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content != get_content(o);
	}
	
	bool operator<  (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content < get_content(o);
	}
	
	bool operator>  (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content > get_content(o);
	}
	
	bool operator<= (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content <= get_content(o);
	}
	
	bool operator>= (const Ref_base& o) const noexcept
	{
		is_null();
		return m_content >= get_content(o);
	}
	
	/** Offers access to the referenced raw data
	 *
	 * \return a pointer to the referenced raw data
	 */
	operator typename Ref_access<R, W>::type() const
	{
		return get();
	}
	
	/** Offers access to the referenced raw data, throws on null references
	 *
	 * \return a pointer to the referenced raw data
	 */
	typename Ref_access<R, W>::type get() const
	{
		if (is_null()) throw Error{PDI_ERR_RIGHT, "Trying to dereference a null reference"};
		return m_content->m_buffer;
	}
	
	/** Offers access to the referenced raw data, returns null for null references
	 *
	 * \return a pointer to the referenced raw data
	 */
	typename Ref_access<R, W>::type get(std::nothrow_t) const noexcept
	{
		if (is_null()) return nullptr;
		return m_content->m_buffer;
	}
	
	/** Checks whether this is a null reference
	 *
	 * \return whether this reference is non-null
	 */
	operator bool () const noexcept
	{
		return !is_null();
	}
	
	/** Nullify the reference
	 */
	void reset() noexcept
	{
		if (m_content) unlink();
	}
	
	/** Makes a copy of the raw content behind this reference and returns a new
	 * reference
	 *
	 * \return a new reference to a copy of the raw data this references
	 */
	Ref copy() const
	{
		return do_copy(*this);
	}
	
	/** Releases ownership of the referenced raw data by nullifying all existing
	 *  references.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void* release() noexcept
	{
		if (is_null()) return nullptr;
		
		// notify everybody of the nullification
		while (!m_content->m_notifications.empty()) {
			// get the key of a notification
			const Ref_base* key = m_content->m_notifications.begin()->first;
			// call this notification, this might invalidate any iterator
			m_content->m_notifications.begin()->second(*this);
			// remove the notification we just called
			m_content->m_notifications.erase(key);
		}
		
		void* result = m_content->m_buffer;
		m_content->m_buffer = nullptr;
		
		unlink();
		
		return result;
	}
	
	/** Registers a nullification callback
	 *
	 * \param notifier the function to call when this reference becomes null
	 */
	void on_nullify(std::function<void(Ref)> notifier) const noexcept
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
	bool PDI_NO_EXPORT is_null() const noexcept
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
	void PDI_NO_EXPORT unlink() const noexcept
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
	void PDI_NO_EXPORT link(Referenced* content) noexcept
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

namespace std {

template<bool R, bool W>
struct hash<PDI::Ref_any<R,W>> {
	size_t operator() (const PDI::Ref_any<R,W>& r) const noexcept
	{
		return r.hash();
	}
};

} // namespace std

#endif //  PDI_REF_ANY_H_
