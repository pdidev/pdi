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

/**
 * \file Data_ref.h
 * \author C. Roussel, corentin.roussel@cea.fr
 * \date 2017-09-08
 */

#include "config.h"

#include <cassert>
#include <memory>

#include "pdi.h"

#include "pdi/data_reference.h"
#include "pdi/state.h"

#include "status.h"

namespace PDI {

using std::make_shared;
using std::move;
using std::unique_ptr;

/** \class  Data_content
*   \brief  Manipulate and grant access to a buffer depending on the remaining right access (read/write).
*/
class Data_ref::Data_content
{
public:
	Data_content();   ///< constructor
	~Data_content();  ///< destructor
	Data_content(const Data_content &) = delete ; ///< unused
	Data_content(Data_content &&) = delete;       ///< unused
	
	PDI_status_t init(void *buffer, Destroyer func, PDI_inout_t permission, const PDI_datatype_t &type); ///< Initialized a Data_content
	
	bool is_writable(); ///< True if no references has writing nor reading access right.
	bool is_readable(); ///< True if is initialized and no reference can write.
	bool has_reader();  ///< True if the number of reader > 0
	
	void *get_buffer() const; ///< return the buffer address
	
	const PDI_datatype_t &get_type() const;///< return a copy of the PDI_datatype_t or the a pointer on the current PDI_datatype_t
	
	bool try_lock(PDI_inout_t access);  ///< retain right access
	bool lock(PDI_inout_t access);  ///< retain right access
	bool unlock(PDI_inout_t access); ///< return right access
	
	/// buffer that contains data
	void *m_buffer;
	
	/// free memory allocated for the buffer
	Destroyer m_delete;
	
	/// a context that is consumed by the destroyer
	void *m_context;
	
	/// type of the data inside the buffer
	PDI_datatype_t m_type;
	
	/// m_buffer has been initialized (is readable)
	bool m_initialized;
	
	/// m_buffer is writable (if True, m_nb_reader=0)
	bool m_writable;
	
	/// number of concurrent reader (if >1 , is_writable = False)
	int m_nb_reader;
	
	/// m_buffer has writer
	bool m_writer;
	
	/// references to this instance
	std::unordered_set< Data_ref *> m_refs;
	
}; // class Data_content

void destroyer_free(void *buffer, void *context)
{
	context = (void *)context;
	free(buffer);
}

template <typename T>
void destroyer_delete(void *buffer, void *context)
{
	context = (void *)context;
	delete(reinterpret_cast<T*>(buffer));
}

Data_ref::Data_ref():
		m_access(PDI_NONE),
		m_data_end(nullptr),
		m_desc(nullptr)
{
}

Data_ref::Data_ref(const Data_descriptor& desc, void* data, PDI_inout_t access, PDI_inout_t lock):
		m_content(make_shared<Data_content>()),
		m_access(lock),
		m_data_end(nullptr),
		m_desc(&desc)
{
	assert(data);
	m_content->init(data, &destroyer_free, access, desc.get_type());
	m_content->m_refs.insert(this);
	grant(lock);
}

Data_ref::Data_ref(const Data_ref &other, PDI_inout_t lock):
		m_content(other.m_content),
		m_access(PDI_NONE),
		m_data_end(nullptr),
		m_desc(other.m_desc)
{
	m_content->m_refs.insert(this);
	if (m_content) m_content->m_refs.insert(this);
	grant(lock);
}

Data_ref::Data_ref(Data_ref &&other, PDI_inout_t lock):
		m_content(move(other.m_content)),
		m_access(PDI_NONE),
		m_data_end(nullptr),
		m_desc(other.m_desc)
{
	if (m_content) {
		m_content->m_refs.insert(this);
		m_content->m_refs.erase(&other);
	}
	grant(lock);
}

Data_ref &Data_ref::operator=(const Data_ref &other) // copy assignment
{
	Data_ref tmp = other; // copy the provided ref with copy cstr
	*this = move(tmp); // move the just created ref into this with move operator
	return *this;
}


Data_ref &Data_ref::operator = (Data_ref &&other)
{
	if (this == &other) return *this;
	
	clear(); // nullify this
	
	m_content = other.m_content;
	m_access = PDI_NONE;
	m_data_end = nullptr;
	m_content->m_refs.insert(this);
	
	return *this;
}

Data_ref::~Data_ref()
{
	clear();
}

Data_ref::operator void* () const
{
	return get();
}

void* Data_ref::get () const
{
	return m_content->m_buffer;
}

Data_ref::operator bool() const
{
	return static_cast<bool>(m_content);
}

void* Data_ref::copy_release()
{
	//TODO: error handling if data is not readable

	//TODO: handle errors
	PDI_datatype_t newtype; PDI_datatype_densify(&newtype, &m_content->m_type);
	
	//TODO: handle errors
	size_t dsize; PDI_datatype_buffersize(&m_content->m_type, &dsize);
	unique_ptr<void, decltype(free)*> newbuffer(malloc(dsize), free);
	PDI_buffer_copy(newbuffer.get(),
	                &newtype,
	                m_content->m_buffer,
	                &m_content->m_type);
	
	// replace the buffer
	void *oldbuffer = m_content->m_buffer;
	m_content->m_buffer = newbuffer.release();
	
	// replace the type
	//TODO: handle errors & replace by move
	PDI_datatype_destroy(&m_content->m_type);
	PDI_datatype_copy(&m_content->m_type, &newtype);
	PDI_datatype_destroy(&newtype);
	
	return oldbuffer;
}

void* Data_ref::null_release()
{
	for (auto && ref: m_content->m_refs) {
		// TODO: should return errors
		ref->data_end();
	}
	m_content->m_buffer = nullptr;
	m_content->m_context = nullptr;
	m_content->m_delete = nullptr;
	PDI_datatype_destroy(&m_content->m_type);
	return *this;
}

const PDI_datatype_t& Data_ref::get_type() const
{
	return m_content->get_type();
}

const Data_descriptor& Data_ref::get_desc() const
{
	return *m_desc;
}



void Data_ref::clear()
{
	rm_priviledge(m_access);
	if ( m_content ) {
		m_content->m_refs.erase(this);
	}
	m_desc = nullptr;
	m_data_end = nullptr;
	m_content.reset();
}

PDI_status_t Data_ref::data_end()
{
	if (m_data_end) {
		//TODO: this does not make much sense here...
		if( PDI_status_t status = m_data_end("", *this) ) return status;
	}
	
	return PDI_OK;
}

bool Data_ref::add_priviledge(const PDI_inout_t inout)
{
	if (m_content->lock(inout)) {
		m_access |= inout;
		return true;
	}
	return false;
}


bool Data_ref::try_grant(PDI_inout_t access) ///< Attempt to ask for (additional) priviledge
{
	switch (m_access) {
	case PDI_INOUT: { ///< Nothing to do here
		return true;
	} case PDI_OUT: {
		if (access & PDI_IN) return m_content->try_lock(PDI_IN);
		return true;
	} case PDI_IN: {
		if (access & PDI_OUT) return m_content->try_lock(PDI_OUT);
		return true;
	} case PDI_NONE: {
		switch (access) {
		case PDI_INOUT: return m_content->try_lock(PDI_INOUT);
		case PDI_OUT: return m_content->try_lock(PDI_OUT);
		case PDI_IN: return m_content->try_lock(PDI_IN);
		case PDI_NONE: return true;
		}
	}
	}
	return false; // remove warning
}


bool Data_ref::grant(PDI_inout_t access) ///< Attempt to ask for (additional) priviledge
{
	switch (m_access) {
	case PDI_INOUT: { ///< Nothing to do here
		return true;
	} case PDI_OUT: {
		if (access & PDI_IN) return add_priviledge(PDI_IN);
		return true;
	} case PDI_IN: {
		if (access & PDI_OUT) return add_priviledge(PDI_OUT);
		return true;
	} case PDI_NONE: {
		switch (access) {
		case PDI_INOUT: return add_priviledge(PDI_INOUT);
		case PDI_OUT: return add_priviledge(PDI_OUT);
		case PDI_IN: return add_priviledge(PDI_IN);
		case PDI_NONE: return true;
		}
	}
	}
	return false; // remove warning
}

bool Data_ref::rm_priviledge(const PDI_inout_t inout)
{
	if (m_content->unlock(inout)) {
		m_access &= (PDI_inout_t) ~inout;
		return true;
	}
	return false;
}

bool Data_ref::revoke(PDI_inout_t access) ///< Release current priviledge on the content
{
	switch (access) {
	case PDI_NONE: {
		return true;
	} case PDI_INOUT: {
		switch (m_access) {
		case PDI_NONE: return true;
		case PDI_INOUT: return rm_priviledge(PDI_INOUT);
		case PDI_OUT: return rm_priviledge(PDI_OUT);
		case PDI_IN: return rm_priviledge(PDI_IN);
		}
	} case PDI_OUT: {
		if (m_access & PDI_OUT) return rm_priviledge(PDI_OUT);
		return true;
	} case PDI_IN: {
		if (m_access & PDI_IN) return rm_priviledge(PDI_IN);
		return true;
	}
	}
	return false; // remove warning
}

bool Data_ref::priviledge(PDI_inout_t access) const
{
	if (access == PDI_INOUT) return bool(m_access == PDI_INOUT);
	return bool(m_access & access);
}

PDI_inout_t Data_ref::priviledge() const
{
	return m_access;
}

Data_ref::Data_content::Data_content():
		m_buffer(NULL),
		m_initialized(false),
		m_writable(true),
		m_nb_reader(0),
		m_writer(false),
		m_refs(std::unordered_set<Data_ref * >())
{ }

/// Initialization
PDI_status_t Data_ref::Data_content::init(void *buffer, Destroyer destroy, PDI_inout_t access, const PDI_datatype_t &type)
{
	PDI_status_t status(PDI_OK);
	if (buffer == NULL || destroy == NULL) return PDI_ERR_VALUE;
	
	m_buffer = buffer;
	m_delete = destroy;
	m_context = nullptr;
	m_initialized = (bool)(access & PDI_OUT);
	m_writable = (bool)(access & PDI_IN);
	PDI_handle_err(PDI_datatype_copy(&m_type, &type), err0);
	
	return status;
err0:
	return status;
}

/// Destructor
Data_ref::Data_content::~Data_content()
{
	// If the data exist and has been reclaimed release it.
	if (m_buffer) {
		m_delete(m_buffer, m_context);
		PDI_datatype_destroy(&m_type);
	}
}

/* ****** ACCESSORS  ****** */
bool Data_ref::Data_content::is_writable()
{
	return m_writable && (m_nb_reader == 0) && (!m_writer); // No one is writing nor reading
}

bool Data_ref::Data_content::is_readable()
{
	return m_initialized && (!m_writer); // data is initialized and no one is writing
}

bool Data_ref::Data_content::has_reader()
{
	return bool(m_nb_reader);
}

void *Data_ref::Data_content::get_buffer() const
{
	return m_buffer;
}

const PDI_datatype_t &Data_ref::Data_content::get_type() const
{
	return m_type;
}

bool Data_ref::Data_content::try_lock(PDI_inout_t access)
{
	bool success(true);
	
	if (access & PDI_OUT) {
		if (! is_readable()) {
			success = false;
		}
	}
	
	if (access & PDI_IN) {
		if (is_writable()) {
			return success;
		} else {
			success = false;
		}
	}
	
	return success;
}

bool Data_ref::Data_content::lock(PDI_inout_t access)
{
	bool success(true);
	int new_reader(0);
	
	if (access & PDI_OUT) {
		if (is_readable()) {
			++m_nb_reader ;
			new_reader = 1;
		} else {
			success = false;
		}
	}
	
	
	if (access & PDI_IN) {
		if (((m_nb_reader - new_reader) == 0) && m_writable && (!m_writer)) {
			m_writer = true;
		} else {
			success = false;
		}
	}
	
	return success;
}


bool Data_ref::Data_content::unlock(PDI_inout_t access)
{
	if ((access & PDI_OUT) && m_initialized && m_nb_reader > 0) {
		--m_nb_reader;
	} else {
		return false;
	}
	
	if ((access & PDI_IN) && m_writer) {
		m_writer = false;
	} else {
		return false;
	}
	
	return true;
}


} // namespace PDI
