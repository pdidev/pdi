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

/** Manipulate and grant access to a buffer depending on the remaining right access (read/write).
*/
class Data_ref::Data_content
{
public:
	Data_content();
	
	~Data_content();
	
	Data_content(void *buffer, Data_ref::Free_function destroy, PDI_inout_t access, const PDI_datatype_t &type);
	
	Data_content(const Data_content &) = delete;
	
	Data_content(Data_content &&) = delete;
	
	/** Whether one can get writer access to the data
	 */
	bool is_writable() { return m_writable && (m_nb_reader == 0) && (!m_has_writer); }
	
	/** Whether one can get reader access to the data
	 */
	bool is_readable() { return m_readable && (!m_has_writer); }
	
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
	
}; // class Data_content

Data_ref::Data_content::Data_content():
		m_buffer(nullptr),
		m_delete(nullptr),
		m_readable(false),
		m_writable(false),
		m_nb_reader(0),
		m_has_writer(false)
{
	PDI_datatype_init_scalar(&m_type, PDI_T_UNDEF);
}

Data_ref::Data_content::Data_content(void *buffer, Free_function destroy, PDI_inout_t access, const PDI_datatype_t &type):
		m_buffer(buffer),
		m_delete(destroy),
		m_readable(access & PDI_OUT),
		m_writable(access & PDI_IN),
		m_nb_reader(0),
		m_has_writer(false)
{
	PDI_datatype_copy(&m_type, &type);
}

Data_ref::Data_content::~Data_content()
{
	assert(m_refs.empty());
	if (m_buffer) m_delete(m_buffer);
	PDI_datatype_destroy(&m_type);
}

Data_ref::Data_ref():
		m_content(make_shared<Data_content>()),
		m_access(PDI_NONE),
		m_data_end(nullptr)
{
}

Data_ref::Data_ref(void* data, Free_function freefunc, const PDI_datatype_t& type, PDI_inout_t access, PDI_inout_t lock):
		m_content(make_shared<Data_content>(data, freefunc, access, type)),
		m_access(lock),
		m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
	grant(lock);
}

Data_ref::Data_ref(const Data_ref &other, PDI_inout_t lock):
		m_content(other.m_content),
		m_access(PDI_NONE),
		m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
	grant(lock);
}

Data_ref::Data_ref(Data_ref &&other, PDI_inout_t lock):
		m_content(other.m_content),
		m_access(PDI_NONE),
		m_data_end(nullptr)
{
	m_content->m_refs.insert(this);
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
	
	reset(); // nullify this
	
	m_content = other.m_content;
	m_access = PDI_NONE;
	m_data_end = nullptr;
	m_content->m_refs.insert(this);
	
	return *this;
}

Data_ref::~Data_ref()
{
	reset();
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
	return get();
}

void* Data_ref::copy_release()
{
	//TODO: error handling if data is not readable

	//TODO: handle errors
	PDI_datatype_t newtype; PDI_datatype_densify(&newtype, &m_content->m_type);
	
	//TODO: handle errors
	size_t dsize; PDI_datatype_buffersize(&m_content->m_type, &dsize);
	void* newbuffer = malloc(dsize);
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

void no_action(void*){}

void* Data_ref::null_release()
{
	// no need to notify ourselves
	m_content->m_refs.erase(this);
	// but notify everybody else
	while ( !m_content->m_refs.empty() ) (*m_content->m_refs.begin())->data_end();
	// now we shall be the sole owner of the data
	m_content->m_delete = no_action; // prevent deletion
	void *result = m_content->m_buffer;
	reset();
	return result;
}

const PDI_datatype_t& Data_ref::type() const
{
	return m_content->m_type;
}

void Data_ref::reset()
{
	revoke(m_access);
	m_content->m_refs.erase(this);
	m_content = make_shared<Data_content>();
}

PDI_status_t Data_ref::data_end()
{
	if (m_data_end) m_data_end("", *this);
	reset();
	return PDI_OK;
}

bool Data_ref::try_grant(PDI_inout_t access)
{
	if ( !m_content ) return false;
	if ( (access&(~m_access)&PDI_OUT) && !m_content->is_readable() ) return false;
	if ( (access&(~m_access)&PDI_IN)  && !m_content->is_writable() ) return false;
	return true;
}

bool Data_ref::grant(PDI_inout_t access)
{
	if ( !try_grant(access) ) return false;
	if ( access & (~m_access) & PDI_IN  ) m_content->m_has_writer = true;
	if ( access & (~m_access) & PDI_OUT ) ++m_content->m_nb_reader;
	return true;
}

bool Data_ref::revoke(PDI_inout_t access)
{
	if ( !priviledge(access) ) return false;
	if ( access & PDI_IN  ) m_content->m_has_writer = false;
	if ( access & PDI_OUT ) --m_content->m_nb_reader;
	return true;
}

bool Data_ref::priviledge(PDI_inout_t access) const
{
	return (m_access & access) == access;
}

PDI_inout_t Data_ref::priviledge() const
{
	return m_access;
}

} // namespace PDI
