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

#include "config.h"

#include <cassert>
#include <iostream>
#include <memory>

#include "pdi.h"

#include "pdi/data_reference.h"
#include "pdi/state.h"

#include "status.h"

namespace PDI
{

using std::cout;
using std::endl;
using std::make_shared;
using std::move;
using std::unique_ptr;

/** Manipulate and grant access to a buffer depending on the remaining right access (read/write).
*/
class Data_ref::Data_content
{
public:
	/// buffer that contains data
	void *m_buffer;
	
	/// Function used to free the memory allocated for the buffer
	Free_function m_delete;
	
	/// type of the data inside the buffer
	PDI_datatype_t m_type;
	
	/// whether m_buffer is readable (if false, m_nb_reader==0)
	bool m_readable;
	
	/// whether m_buffer is writable (if false, m_has_writer==false)
	bool m_writable;
	
	/// number of concurrent reader (if >0, is_readable==true)
	int m_nb_reader;
	
	/// whether a reader has locked this (if true, m_writable==true)
	bool m_has_writer;
	
	/// references to this instance
	std::unordered_set< Data_ref *> m_refs;
	
	Data_content():
		m_buffer(nullptr),
		m_delete(nullptr),
		m_readable(true),
		m_writable(true),
		m_nb_reader(0),
		m_has_writer(false)
	{
		PDI_datatype_init_scalar(&m_type, PDI_T_UNDEF);
	}
	
	Data_content(void *buffer, Free_function destroy, const PDI_datatype_t &type, bool readable, bool writable):
		m_buffer(buffer),
		m_delete(destroy),
		m_readable(readable),
		m_writable(writable),
		m_nb_reader(0),
		m_has_writer(false)
	{
		PDI_datatype_copy(&m_type, &type);
	}
	
	~Data_content()
	{
		assert(m_refs.empty());
		assert(!m_has_writer);
		assert(!m_nb_reader);
		if (m_buffer) m_delete(m_buffer);
		PDI_datatype_destroy(&m_type);
	}
	
	Data_content(const Data_content &) = delete;
	
	Data_content(Data_content &&) = delete;
	
}; // class Data_content

Data_ref::Data_ref():
	m_content(make_shared<Data_content>()),
	m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
}

Data_ref::Data_ref(void *data, Free_function freefunc, const PDI_datatype_t &type, bool readable, bool writable):
	m_content(make_shared<Data_content>(data, freefunc, type, readable, writable)),
	m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
}

Data_ref::Data_ref(const Data_ref &other):
	m_content(other.m_content),
	m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
}

Data_ref &Data_ref::operator=(const Data_ref &other) // copy assignment
{
	if (this == &other) return *this;
	
	m_content->m_refs.erase(this);
	unlock();
	m_content = other.m_content;
	if (!lock()) {
		m_content = make_shared<Data_content>();
		lock();
	}
	m_content->m_refs.insert(this);
	
	return *this;
}

Data_ref::~Data_ref()
{
	m_content->m_refs.erase(this);
}

void *Data_ref::get() const
{
	return m_content->m_buffer;
}

Data_ref::operator bool() const
{
	return get();
}

void *Data_ref::copy_release()
{
	//TODO: error handling if data is not readable
	
	//TODO: handle errors
	PDI_datatype_t newtype; PDI_datatype_densify(&newtype, &m_content->m_type);
	
	//TODO: handle errors
	size_t dsize; PDI_datatype_buffersize(&m_content->m_type, &dsize);
	void *newbuffer = malloc(dsize);
	PDI_buffer_copy(newbuffer,
	                &newtype,
	                m_content->m_buffer,
	                &m_content->m_type);
	                
	// replace the buffer
	void *oldbuffer = m_content->m_buffer;
	m_content->m_buffer = newbuffer;
	
	// replace the type
	//TODO: handle errors
	PDI_datatype_destroy(&m_content->m_type);
	m_content->m_type = newtype;
	
	return oldbuffer;
}

void no_action(void *) {}

void *Data_ref::null_release()
{
	// no need to notify ourselves, we'll do it last
	m_content->m_refs.erase(this);
	// but notify everybody else
	while (!m_content->m_refs.empty())(*m_content->m_refs.begin())->reset();
	// now we shall be the sole owner of the data
	m_content->m_delete = no_action; // prevent deletion
	void *result = m_content->m_buffer;
	reset(); // nullify and notify ourselves
	return result;
}

const PDI_datatype_t &Data_ref::type() const
{
	return m_content->m_type;
}

PDI_status_t Data_ref::reset()
{
	if (m_data_end)(*m_data_end)(*this);
	unlock();
	m_content->m_refs.erase(this);
	m_content = make_shared<Data_content>();
	m_content->m_refs.insert(this);
	lock();
	return PDI_OK;
}

bool Data_ref::can_lock(bool read, bool write)
{
	if (write) {   // need read access
		if (!m_content->m_writable) return false;   // content is not writable at all
		if (m_content->m_has_writer) return false;   // content already has a writer
		if (m_content->m_nb_reader) return false;   // content has a reader
	}
	if (read) {   // need read access
		if (!m_content->m_readable) return false;   // content is not readable at all
		if (m_content->m_has_writer) return false;   // content already has a writer
	}
	return true;
}

bool Data_ref::lock()
{
	return true;
}

void Data_ref::unlock()
{
}

Data_r_ref::Data_r_ref()
{
	++m_content->m_nb_reader; // we automatically get W access on a null ref
}

Data_r_ref::Data_r_ref(const Data_ref &o):
	Data_r_ref()
{
	Data_ref::operator=(o);
}

bool Data_r_ref::lock()
{
	if (!can_lock(true, false)) return false;
	++m_content->m_nb_reader;
	return true;
}

void Data_r_ref::unlock()
{
	--m_content->m_nb_reader;
}

Data_w_ref::Data_w_ref()
{
	m_content->m_has_writer = true; // we automatically get W access on a null ref
}

Data_w_ref::Data_w_ref(const Data_ref &o):
	Data_w_ref()
{
	Data_ref::operator=(o);
}

bool Data_w_ref::lock()
{
	if (!can_lock(false, true)) return false;
	m_content->m_has_writer = true;
	return true;
}

void Data_w_ref::unlock()
{
	m_content->m_has_writer = false;
}

Data_rw_ref::Data_rw_ref()
{
	++m_content->m_nb_reader; // we automatically get R access on a null ref
	m_content->m_has_writer = true; // we automatically get W access on a null ref
}

Data_rw_ref::Data_rw_ref(const Data_ref &o):
	Data_rw_ref()
{
	Data_ref::operator=(o);
}

bool Data_rw_ref::lock()
{
	if (!can_lock(true, true)) return false;
	++m_content->m_nb_reader;
	m_content->m_has_writer = true;
	return true;
}

void Data_rw_ref::unlock()
{
	Data_w_ref::unlock();
	Data_r_ref::unlock();
}

} // namespace PDI
