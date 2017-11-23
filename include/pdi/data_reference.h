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

#include <memory>

#include "pdi.h"

#include "pdi/data_descriptor_fwd.h"
#include "pdi/datatype.h"
#include "pdi/plugin.h"

namespace PDI
{

/** A dynamically typed reference to data with automatic memory management and
 * read/write locking semantic.
 *
 * Data_ref is a smart pointer that features:
 * - a dynamic type system,
 * - cycle-free garbage collecting similar to std::shared_ptr,
 * - a read/write locking mechanism similar to std::shared_mutex,
 * - a notification system to be notified when the raw data is to be deleted,
 * - a release system that nullifies all existing references to the raw data.
 *
 * \warning As of now, and unlike std::shared_ptr, the lock system can not be
 * relied upon in a multithreaded environment.
 *
 * \author Corentin Roussel (CEA) <corentin.roussel@cea.fr>
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 */
class Data_ref
{
public:
	/**
	 */
	typedef void (*Free_function)(void *);
	
	/** Constructs a null ref
	 */
	Data_ref();
	
	/** Creates a reference to currently unreferenced data
	 * \param data the raw data to reference
	 * \param readable the maximum allowed access to the underlying content
	 * \param writable the maximum allowed access to the underlying content
	 */
	Data_ref(void *data, Free_function freefunc, const PDI_datatype_t &type, bool readable, bool writable);
	
	/** Copies an existing reference
	 * \param other the ref to copy
	 */
	Data_ref(const Data_ref &other);
	
	/** Destructor
	 */
	~Data_ref();
	
	/** Copies an existing reference into this one
	 * \param other the ref to copy
	 * \return *this
	 */
	Data_ref &operator= (const Data_ref &other);
	
	/** Offers access to the referenced raw data
	 * \return a pointer to the referenced raw data
	 */
	operator void *() const;
	
	/** Offers access to the referenced raw data
	 * \return a pointer to the referenced raw data
	 */
	void *get() const;
	
	/** Checks whether this is a null reference
	 * \return whether this reference is non-null
	 */
	operator bool () const;
	
	/** Releases ownership of the referenced raw data by replacing all existing
	 *  references by references to a copy.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void *copy_release();
	
	/** Releases ownership of the referenced raw data by nullifying all existing
	 *  references.
	 *
	 * \return the previously referenced raw data or nullptr if this was a null
	 * reference, i.e. the value which would be returned by get() before the call.
	 */
	void *null_release();
	
	template <typename T>
	void on_nullify(const T &notifier)
	{
		m_data_end = new Notification_wrapper<T>(notifier);
	}
	
	/** accesses the type of the referenced raw data
	 */
	const PDI_datatype_t &type() const;
	
	/** Check if a request for additional access priviledges would success
	 *  without actually requesting them
	 */
	bool can_grant(bool read, bool write);
	
	/** Increase the access priviledge of this reference
	 */
	bool grant(bool read, bool write);
	
	/** Releases the specified access priviledge from this reference
	 */
	bool revoke(bool read, bool write);
	
	/** Checks whether this reference offers the requested access priviledge
	 */
	bool priviledge(bool read, bool write) const;
	
private:
	class Notification
	{
	public:
		virtual void operator()(const Data_ref &) = 0;
		virtual ~Notification() {};
	};
	
	template< typename T > class Notification_wrapper : public Notification
	{
	public:
		Notification_wrapper(const T &notifier): m_notifier(notifier) {}
		virtual void operator()(const Data_ref &ref)
		{
			m_notifier(ref);
		};
		T m_notifier;
	};
	
	class Data_content;
	
	friend class Data_content;
	
	/** Makes this the null reference again
	 */
	void reset();
	
	/** Calls the data_end function of this reference
	 */
	PDI_status_t data_end();
	
	/** shared pointer on the data content, it is never null
	 * \todo replace by a raw pointer we manage ourselves
	 */
	std::shared_ptr< Data_content >  m_content;
	
	/// Authorized access using this reference
	bool m_read_access;
	
	/// Authorized access using this reference
	bool m_write_access;
	
	/// function to use before releasing the data. Wrapper below.
	std::unique_ptr<Notification> m_data_end;
	
}; // class Data_ref

} // namespace PDI

#endif //  DATA_REF_H__
