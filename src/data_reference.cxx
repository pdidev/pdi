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
 * \file data_ref.cxx
 * \brief .
 * \author C. Roussel, corentin.roussel@cea.fr
 */

#include "config.h"

#include "pdi.h"

#include "pdi/data_reference.h"
#include "pdi/data_content.h"
#include "pdi/state.h"

#include "status.h"

using namespace PDI;


/* ****** LIFECYCLE  ****** */
/// default constructor
Data_ref::Data_ref()
{
	this->clear();
}

Data_ref::Data_ref(Data_ref &&old_ref)
{
	*this = std::move(old_ref); ///< Calling move operator
}

Data_ref::Data_ref(const Data_ref &origin):
	m_content(origin.m_content),
	m_access(PDI_NONE),
	m_data_end(origin.m_data_end),
	m_desc(origin.m_desc)
{
	/// update the list of reference inside content
	m_content->m_refs.insert(this);
}

/// Init function with explicit data_end function
PDI_status_t Data_ref::init(const std::string name, const std::shared_ptr< Data_content > content, const PDI_data_end_f data_end, const PDI_inout_t inout = PDI_NONE)
{
	PDI_status_t status(PDI_OK);
	if (! content.get()) PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Cannot initialized reference. Content is empty"), err0);
	
	if (PDI_state.descriptors.find(name) == PDI_state.descriptors.end()) {
		PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Cannot initialized reference. No descriptor for data : %s", name.c_str()), err0);
	}
	
	m_content = content;
	m_content->m_refs.insert(this);
	m_data_end = data_end;
	m_access = inout;
	m_desc = &PDI_state.descriptors[name];
	
	return status;
err0:
	return status;
}

PDI_status_t Data_ref::init(const std::string name, const std::shared_ptr< Data_content > content, const PDI_inout_t inout)
{
	PDI_status_t status(PDI_OK);
	if (! content.get()) PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Cannot initialized reference. Content is empty"), err0);
	
	if (PDI_state.descriptors.find(name) == PDI_state.descriptors.end()) {
		PDI_handle_err(PDI_make_err(PDI_ERR_IMPL, "Cannot initialized reference. No descriptor for data : %s", name.c_str()), err0);
	}
	
	m_content = content;
	m_content->m_refs.insert(this);
	m_data_end = nullptr;
	m_access = inout;
	m_desc = &PDI_state.descriptors[name];
	
	return status;
err0:
	return status;
}


/// Destructor
Data_ref::~Data_ref()
{
	// remove reference from the list of ref
	if (m_content) m_content->m_refs.erase(this);
	if (m_access & PDI_IN) m_content->unlock(PDI_IN);
	if (m_access & PDI_OUT) m_content->unlock(PDI_OUT);
}


/* ****** OPERATOR  ****** */
Data_ref &Data_ref::operator = (Data_ref &&old_ref)
{
	if (this != &old_ref) {
	
		m_content = old_ref.m_content;///< move the data content
		old_ref.m_content = nullptr;
		
		m_access = old_ref.m_access; ///< move and nullify the old ref access
		old_ref.m_access = PDI_NONE;
		
		m_desc = old_ref.m_desc;
		old_ref.m_desc = nullptr;
		
		m_data_end = old_ref.m_data_end;
		old_ref.m_data_end = nullptr;
		
		/// update the list of reference inside content
		if (m_content) {
			m_content->m_refs.erase(&old_ref);
			m_content->m_refs.insert(this);
		}
		return *this;
	} else {
		return old_ref;
	}
}

Data_ref &Data_ref::operator=(const Data_ref &other) // copy assignment
{
	if (this != &other) {
		if (other.m_content) { // If there is a content initialized this.
			if (m_data_end) {
				this->init(other.get_name(), other.m_content, other.m_data_end);
			} else {
				this->init(other.get_name(), other.m_content, PDI_NONE);
			}
		} else {
			this->clear();
		}
	}
	return *this;
}


PDI::Data_ref::operator bool() const
{
	return static_cast<bool>(m_content);
}


/* ****** METHOD  ****** */
void Data_ref::clear()
{
	m_content = nullptr;
	m_access = PDI_NONE;
	m_data_end = nullptr;
	m_desc = nullptr;
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


/* ****** ACCESSORS  ****** */
std::shared_ptr< Data_content> Data_ref::get_content() const
{
	return m_content;
}

const Data_descriptor &Data_ref::get_desc() const
{
	return *m_desc;
}

bool Data_ref::is_metadata() const
{
	return m_desc->is_metadata();
}

const std::string &Data_ref::get_name() const
{
	return m_desc->get_name();
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

bool Data_ref::priviledge(PDI_inout_t access) const ///< Return true if access is less or equal to the current priviledge
{
	if (access == PDI_INOUT) return bool(m_access == PDI_INOUT);
	return bool(m_access & access);
}

PDI_inout_t Data_ref::priviledge() const ///< Return the current priviledge
{
	return m_access;
}
