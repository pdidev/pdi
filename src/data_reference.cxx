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

#include "pdi.h"

#include "pdi/data_reference.h"
#include "pdi/data_content.h"
#include "pdi/state.h"

#include "status.h"

namespace PDI {

using std::make_shared;
using std::move;

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

Data_ref::operator bool() const
{
	return static_cast<bool>(m_content);
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

PDI_status_t Data_ref::reclaim()
{
	return m_content->reclaim(this); ///< reclaim all reference excep this one
}

PDI_status_t Data_ref::data_end()
{
	PDI_status_t status(PDI_OK);
	if (m_data_end) PDI_handle_err((*m_data_end)(std::move(*this)), err0);  // calling data_end
	
	return status;
err0:
	this->revoke(PDI_INOUT);
	return status;
}

std::shared_ptr< Data_content> Data_ref::get_content() const
{
	return m_content;
}

const std::string& Data_ref::get_name() const
{
	return get_desc().get_name();
}

const Data_descriptor& Data_ref::get_desc() const
{
	return *m_desc;
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

} // namespace PDI
