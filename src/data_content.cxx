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
/* The following is used for doxygen documentation */
/**
 * \file data_content.cxx
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
 */
// Created:  04/09/2017 09:49:11


#include "config.h"

#include <cassert>
#include <memory>

#include "pdi/data_content.h"
#include "pdi/data_reference.h"

#include "status.h"

using namespace PDI;
using std::unique_ptr;

/*** PDI_memmode_t operator ***/
PDI_memmode_t operator|(PDI_memmode_t a, PDI_memmode_t b)
{
	typedef std::underlying_type< PDI_memmode_t >::type UL;
	return (PDI_memmode_t)(static_cast< UL >(a) | static_cast< UL >(b));
}

PDI_memmode_t &operator|=(PDI_memmode_t &lhs, PDI_memmode_t rhs)
{
	return lhs = (PDI_memmode_t)(lhs | rhs);
}

PDI_memmode_t operator&(PDI_memmode_t a, PDI_memmode_t b)
{
	typedef std::underlying_type< PDI_memmode_t >::type UL;
	return (PDI_memmode_t)(static_cast< UL >(a) & static_cast< UL >(b));
}

PDI_memmode_t &operator&=(PDI_memmode_t &lhs, PDI_memmode_t rhs)
{
	return lhs = (PDI_memmode_t)(lhs & rhs);
}


/*** Functions ***/
void PDI::destroyer_free(void *buffer, void *context)
{
	context = (void *)context;
	free(buffer);
}

template <typename T>
void PDI::destroyer_delete(T *t, void *context)
{
	context = (void *)context;
	delete(t);
}

//============================ Class Data_content

/* ****** LIFECYCLE  ****** */
/// Default constructor
Data_content::Data_content() :
	m_buffer(NULL), m_initialized(false), m_nb_reader(0), m_writable(true), m_writer(false), m_memory_mode(PDI_MM_NONE),  m_refs(std::unordered_set<Data_ref * >())
{ }

/// Initialization
PDI_status_t Data_content::init(void *buffer, Destroyer destroy, PDI_inout_t access, const PDI_datatype_t &type)
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
Data_content::~Data_content()
{
	// If the data exist and has been reclaimed release it.
	if (m_buffer && (m_memory_mode & PDI_MM_FREE)) m_delete(m_buffer, m_context);
	
	PDI_datatype_destroy(&m_type);
}

/* ****** ACCESSORS  ****** */
bool Data_content::is_writable()
{
	return m_writable && (m_nb_reader == 0) && (!m_writer); // No one is writing nor reading
}

bool Data_content::is_readable()
{
	return m_initialized && (!m_writer); // data is initialized and no one is writing
}

bool Data_content::has_reader()
{
	return bool(m_nb_reader);
}

void *Data_content::get_buffer() const
{
	return m_buffer;
}

const PDI_datatype_t &Data_content::get_type() const
{
	return m_type;
}

void Data_content::add_memory_mode(const PDI_memmode_t ownership)
{
	m_memory_mode |= ownership;
}


/* ****** METHODS  ****** */
PDI_status_t Data_content::copy_metadata()
{
	PDI_status_t status(PDI_OK);
	void *newval = NULL;
	
	assert(m_buffer);
	if (!m_initialized) {
		PDI_handle_err(PDI_make_err(PDI_ERR_RIGHT, "Metadata has been exposed without read access"), err0);
	}
	
	// copy the last exposed value of the data
	PDI_datatype_t oldtype;
	PDI_handle_err(PDI_datatype_copy(&oldtype, &m_type), err0);
	PDI_handle_err(PDI_datatype_destroy(&m_type), err1);
	PDI_handle_err(PDI_datatype_densify(&m_type, &oldtype), err1);
	size_t dsize; PDI_handle_err(PDI_datatype_buffersize(&m_type, &dsize), err1);
	newval = malloc(dsize);
	PDI_handle_err(PDI_buffer_copy(
	                   newval,
	                   &m_type,
	                   m_buffer,
	                   &oldtype),
	               err2);
	               
	m_buffer = newval;
	m_memory_mode |= PDI_MM_FREE; // we are now responsible to free the newly created copy
	
	PDI_datatype_destroy(&oldtype);
	return status;
	
err2:
	free(newval);
err1:
	PDI_datatype_destroy(&oldtype);
err0:
	return status;
}


PDI_status_t Data_content::reclaim(Data_ref *ignore = nullptr)
{
	PDI_status_t status(PDI_OK);
	
	size_t nref = m_refs.size();
	if ((ignore && nref > 1) || nref >= 1) { // if multiple references still exist
		for (auto &iter : m_refs) {
			if (iter !=  ignore) {
				iter->data_end(); // TODO: should return errors
			}
		}
	}
	return status;
}

bool Data_content::try_lock(PDI_inout_t access)
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

bool Data_content::lock(PDI_inout_t access)
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


bool Data_content::unlock(PDI_inout_t access)
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
