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
	
	/// number of locks preventing read access
	int m_read_locks;
	
	/// number of locks preventing write access (should always remain between 0 & 2 inclusive w. current implem.)
	int m_write_locks;
	
	/// references to this instance
	std::unordered_set< Data_ref *> m_refs;
	
	Data_content() = delete;
	
	Data_content(const Data_content &) = delete;
	
	Data_content(Data_content &&) = delete;
	
	/** Constrcuts a referenceable version of the content
	 * \param buffer the actual content
	 * \param destroy the function to use to destroy the content or null to not destroy it
	 * \param type the content type, this takes ownership of the type
	 * \param readable whether it is allowed to read the content
	 * \param writable whether it is allowed to write the content
	 */
	Data_content(void *buffer, Free_function destroy, const PDI_datatype_t &type, bool readable, bool writable):
		m_buffer(buffer),
		m_delete(destroy),
		m_type(type),
		m_read_locks(readable ? 0 : 1),
		m_write_locks(writable ? 0 : 1)
	{
		assert(buffer);
	}
	
	~Data_content()
	{
		assert(m_refs.empty());
		if (m_delete) m_delete(m_buffer);
		PDI_datatype_destroy(&m_type);
		assert(m_read_locks == 0 || m_read_locks == 1);
		assert(m_write_locks == 0 || m_write_locks == 1);
	}
	
}; // class Data_content

Data_ref::Data_ref():
	m_content(nullptr),
	m_data_end(nullptr)
{
}

Data_ref::Data_ref(void *data, Free_function freefunc, const PDI_datatype_t &type, bool readable, bool writable):
		Data_ref()
{
	if (data) link(new Data_content(data, freefunc, type, readable, writable));
}

Data_ref::Data_ref(const Data_ref &other):
		Data_ref()
{
	link(other.m_content);
}

Data_ref &Data_ref::operator=(const Data_ref &other) // copy assignment
{
	if (this == &other) return *this;
	
	unlink();
	link(other.m_content);
	
	return *this;
}

Data_ref::~Data_ref()
{
	unlink();
}

void *Data_ref::get() const
{
	if (!m_content) return nullptr;
	return m_content->m_buffer;
}

void *Data_ref::copy_release()
{
	if (!m_content) return nullptr;
	
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
	
	// replace the destroyer
	m_content->m_delete = free;
	
	// replace the type
	//TODO: handle errors
	PDI_datatype_destroy(&m_content->m_type);
	m_content->m_type = newtype;
	
	return oldbuffer;
}

void *Data_ref::null_release()
{
	if (!m_content) return nullptr;
	
	void *result = m_content->m_buffer; // keep the content for later
	m_content->m_delete = nullptr; // prevent its deletion
	
	// make a copy of the content pointer because we are going to be nullified
	Data_content &content = *m_content;
	
	// notify everybody but one to release the data (including ourselves)
	while (content.m_refs.size() > 1)(*content.m_refs.begin())->reset();
	(*content.m_refs.begin())->reset(); // notify the last one which should delete the content
	//TODO: handle the case where this last reset actually created a new ref
	
	return result;
}

const PDI_datatype_t &Data_ref::type() const
{
	if (!m_content) return PDI_UNDEF_TYPE;
	return m_content->m_type;
}

PDI_status_t Data_ref::reset()
{
	if (!m_content) return PDI_OK;
	if (m_data_end)(*m_data_end)(*this);
	unlink();
	return PDI_OK;
}

void Data_ref::link(Data_content *content)
{
	assert(!m_content);
	if (!content) return;
	m_content = content;
	m_content->m_refs.insert(this);
}

void Data_ref::unlink()
{
	if (!m_content) return;
	m_content->m_refs.erase(this);
	if ( m_content->m_refs.empty() ) delete m_content;
	m_content = nullptr;
}

void Data_r_ref::link(Data_content *new_content)
{
	assert(!m_content);
	if (!new_content) return;
	if (new_content->m_read_locks) return;
	Data_ref::link(new_content);
	++m_content->m_write_locks; // a read ref locks data for writing, reading remains possible
}

void Data_r_ref::unlink()
{
	if (!m_content) return;
	--m_content->m_write_locks;
	Data_ref::unlink();
}

void Data_w_ref::link(Data_content *new_content)
{
	assert(!m_content);
	if (!new_content) return;
	if (new_content->m_write_locks) return;
	Data_ref::link(new_content);
	++m_content->m_write_locks; // a write ref locks data for both reading & writing
	++m_content->m_read_locks;
}

void Data_w_ref::unlink()
{
	if (!m_content) return;
	--m_content->m_write_locks;
	--m_content->m_read_locks;
	Data_ref::unlink();
}

void Data_rw_ref::link(Data_content *new_content)
{
	assert(!m_content);
	if (!new_content) return;
	if (new_content->m_read_locks) return;
	if (new_content->m_write_locks) return;
	Data_ref::link(new_content);
	++m_content->m_write_locks; // a read-write ref locks data for both reading & writing
	++m_content->m_read_locks;
}

void Data_rw_ref::unlink()
{
	if (!m_content) return;
	--m_content->m_write_locks;
	--m_content->m_read_locks;
	Data_ref::unlink();
}

} // namespace PDI
