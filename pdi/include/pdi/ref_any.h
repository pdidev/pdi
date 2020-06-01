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

namespace {

/** The type returned by a reference with R/W access
 */
template<bool R, bool W>
struct Ref_access {
	/// No access (void) by default
	typedef void type;
};

/** The type returned by a reference with R/W access where W=true
 */
template<bool R>
struct Ref_access<R, true> {
	/// Full (void*) access
	typedef void* type;
};

/** The type returned by a reference with R/W access where R=true, W=false
 */
template<>
struct Ref_access<true, false> {
	/// read-only access (const void*)
	typedef const void* type;
};

} //namespace <anonymous>


/** A common base for all references, whatever their access privileges.
 *
 * This ensure all reference, even with different type because of differing
 * access privileges, can access the content at this level in each other.
 */
class PDI_EXPORT Reference_base
{
protected:

	/** A descriptor for a buffer in which references can point.
	 *
	 * Both locking and memory management happen at this granularity.
	 */
	struct PDI_NO_EXPORT Referenced_buffer {
	
		/// The function to call to deallocate the buffer memory
		std::function<void()> m_delete;
		
		/// Number of references to this buffer
		int m_owners;
		
		/// Number of locks preventing read access
		int m_read_locks;
		
		/** Number of locks preventing write access
		 *
		 * this should always remain between 0 & 2 inclusive w. current implem.
		 */
		int m_write_locks;
		
		/// Nullification notifications registered on this instance
		std::unordered_map<const Reference_base*, std::function<void(Ref)> > m_notifications;
		
		/** Constructs a new buffer descriptor
		 *
		 * \param deleter the function to use to deallocate the buffer memory
		 * \param readable whether it is allowed to read the content
		 * \param writable whether it is allowed to write the content
		 */
		Referenced_buffer(std::function<void()> deleter, bool readable, bool writable) noexcept:
			m_delete{deleter},
		         m_owners{0},
		         m_read_locks{readable ? 0 : 1},
		         m_write_locks{writable ? 0 : 1}
		{}
		
		
		Referenced_buffer() = delete;
		
		Referenced_buffer(const Referenced_buffer&) = delete;
		
		Referenced_buffer(Referenced_buffer&&) = delete;
		
		~Referenced_buffer()
		{
			m_delete();
			assert(!m_owners);
			assert(m_read_locks == 0 || m_read_locks == 1);
			assert(m_write_locks == 0 || m_write_locks == 1);
			assert(m_notifications.empty());
		}
		
	};
	
	/** A descriptor for data on which references can point.
	 *
	 * The content type is handled at this granularity
	 */
	struct PDI_NO_EXPORT Referenced_data {
	
		/// The buffer in which the data lives
		mutable Referenced_buffer* m_buffer;
		
		/// In-memory location of the data
		void* m_data;
		
		/// Type of the data
		Datatype_uptr m_type;
		
		/// Number of references to this data
		int m_owners;
		
		/** Constructs a new data descriptor from an already referenced buffer.
		 *
		 * \param buffer the buffer containing the data
		 * \param data the data location
		 * \param type the type of the data
		 */
		Referenced_data(Referenced_buffer* buffer, void* data, Datatype_uptr type):
			m_buffer{buffer},
			m_data{data},
			m_type{std::move(type)},
			m_owners{0}
		{
			assert(buffer);
			assert(data);
			m_buffer->m_owners++;
		}
		
		/** Constructs a new data descriptor.
		 *
		 * \param data the data location
		 * \param freefunc the function to use to free the data buffer
		 * \param type the type of the data
		 * \param readable the maximum allowed access to the underlying content
		 * \param writable the maximum allowed access to the underlying content
		 */
		Referenced_data(void* data, std::function<void(void*)> freefunc, Datatype_uptr type, bool readable, bool writable):
			m_data{data},
			m_type{std::move(type)},
			m_owners{0}
		{
			assert(data);
			Datatype* cloned_type = m_type->clone_type().release(); // cannot use uptr, because std::function must be CopyConstructible
			m_buffer = new Referenced_buffer{[data, freefunc, cloned_type]() mutable {
					cloned_type->destroy_data(data);
					delete (cloned_type);
					freefunc(data);
				}, readable, writable};
			m_buffer->m_owners++;
		}
		
		Referenced_data() = delete;
		
		Referenced_data(const Referenced_data&) = delete;
		
		Referenced_data(Referenced_data&&) = delete;
		
		~Referenced_data()
		{
			m_buffer->m_owners--;
			if (m_buffer->m_owners == 0) {
				delete m_buffer;
			}
		}
	};
	
	
	/** Pointer on the data content, can be null if the ref is null
	 */
	mutable Referenced_data* m_content;
	
	
	/** Function to access the content from a reference with different access right
	 */
	static Referenced_data PDI_NO_EXPORT* get_content(const Reference_base& other) noexcept
	{
		if ( !other.m_content ) return NULL;
		if ( !other.m_content->m_data ) return NULL;
		return other.m_content;
	}
	
	// Symbol should not be exported, but it required to force
	// generation of all 4 variants of `Ref_any::copy`
	static Ref do_copy(Ref_r ref);
	
	/** Constructs a null reference
	 */
	Reference_base() noexcept:
		m_content(nullptr)
	{}
	
	Reference_base(const Reference_base&) = delete;
	
	Reference_base(Reference_base&&) = delete;
	
	Reference_base& operator = (const Reference_base&) = delete;
	
	Reference_base& operator = (Reference_base&&) = delete;
	
public:
	/** accesses the type of the referenced raw data
	 */
	const Datatype& type() const noexcept;
	
	size_t hash() const noexcept
	{
		return std::hash<Referenced_data*>()(get_content(*this));
	}
	
}; // class Data_ref_base


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
	public Reference_base
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
		Reference_base()
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
		Reference_base()
	{
		link(get_content(other));
	}
	
	/** Moves an existing reference
	 * \param other the ref to copy
	 */
	Ref_any(Ref_any&& other) noexcept:
		Reference_base()
	{
		if (!other.m_content) return;
		// the other ref notification disappears
		other.m_content->m_buffer->m_notifications.erase(&other);
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
		Reference_base()
	{
		if (type->datasize() && !data && (readable||writable)) {
			throw Error{PDI_ERR_TYPE, "Referencing null data with non-null size"};
		}
		if (data) {
			link(new Referenced_data(data, freefunc, std::move(type), readable,  writable));
		}
	}
	
	/** Creates a subreference from reference
	 *
	 * \deprecated offset/type will be replace by datatype access sequence
	 *
	 * \param other source reference
	 * \param offset _dataoffset of the new memory address
	 * \param type the type of the subreferenced data
	 */
	Ref_any(Ref other, size_t offset, Datatype_uptr type):
		Reference_base()
	{
		if (other) {
			Referenced_data* other_content = get_content(other);
			link(new Referenced_data(
			        other_content->m_buffer,
			        static_cast<int8_t*>(other_content->m_data) + offset,
			        std::move(type)));
		}
	}
	
	/** Destructor
	 */
	~Ref_any()
	{
		reset();
	}
	
	Ref_any& operator= (Ref_any&& other) const noexcept
	{
		// self-copy: nothing to do
		if (&other ==  this) return *this;
		// we'll be copied into, start nullifying ourselves first
		reset();
		// if the other is null also, we're done
		if (other.is_null()) return *this;
		// the other ref notification disappears
		other.m_content->m_buffer->m_notifications.erase(&other);
		// since we get the same privileges as those we release we can just steal the content
		m_content = other.m_content;
		other.m_content = nullptr;
		return *this;
	}
	
	Ref_any& operator= (const Ref_any& other) const noexcept
	{
		// self-copy: nothing to do
		if (&other ==  this) return *this;
		// we'll be copied into, start nullifying ourselves first
		reset();
		// and copy the content from the other
		link(get_content(other));
		return *this;
	}
	
	bool operator== (const Reference_base& o) const noexcept
	{
		is_null();
		return m_content == get_content(o);
	}
	
	bool operator!= (const Reference_base& o) const noexcept
	{
		is_null();
		return m_content != get_content(o);
	}
	
	bool operator<  (const Reference_base& o) const noexcept
	{
		is_null();
		return m_content < get_content(o);
	}
	
	bool operator>  (const Reference_base& o) const noexcept
	{
		is_null();
		return m_content > get_content(o);
	}
	
	bool operator<= (const Reference_base& o) const noexcept
	{
		is_null();
		return m_content <= get_content(o);
	}
	
	bool operator>= (const Reference_base& o) const noexcept
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
		return m_content->m_data;
	}
	
	/** Offers access to the referenced raw data, returns null for null references
	 *
	 * \return a pointer to the referenced raw data
	 */
	typename Ref_access<R, W>::type get(std::nothrow_t) const noexcept
	{
		if (is_null()) return nullptr;
		return m_content->m_data;
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
		while (!m_content->m_buffer->m_notifications.empty()) {
			// get the key of a notification
			const Reference_base* key = m_content->m_buffer->m_notifications.begin()->first;
			// call this notification, this might invalidate any iterator
			m_content->m_buffer->m_notifications.begin()->second(*this);
			// remove the notification we just called
			m_content->m_buffer->m_notifications.erase(key);
		}
		
		void* result = m_content->m_data;
		m_content->m_data = nullptr;
		m_content->m_buffer->m_delete = []() {}; // Referenced_metadata won't delete data
		
		unlink();
		
		return result;
	}
	
	/** Registers a nullification callback
	 *
	 * \param notifier the function to call when this reference becomes null
	 */
	void on_nullify(std::function<void(Ref)> notifier) const noexcept
	{
		if (!is_null()) m_content->m_buffer->m_notifications[this] = notifier;
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
		if (!m_content->m_data) {
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
		m_content->m_buffer->m_notifications.erase(this);
		if (R || W) --m_content->m_buffer->m_write_locks;
		if (W) --m_content->m_buffer->m_read_locks;
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
	void PDI_NO_EXPORT link(Referenced_data* content) noexcept
	{
		assert(!m_content);
		if (!content || !content->m_data) return; // null ref
		if (R && content->m_buffer->m_read_locks) return;
		if (W && content->m_buffer->m_write_locks) return;
		m_content = content;
		++m_content->m_owners;
		if (R || W) ++m_content->m_buffer->m_write_locks;
		if (W) ++m_content->m_buffer->m_read_locks;
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
