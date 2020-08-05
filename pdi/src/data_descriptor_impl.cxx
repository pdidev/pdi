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

#include "config.h"

#include <functional>
#include <iostream>
#include <memory>
#include <vector>

#include <spdlog/spdlog.h>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "data_descriptor_impl.h"


namespace PDI {

using std::exception;
using std::nothrow;
using std::stack;
using std::string;
using std::unique_ptr;
using std::vector;

struct Data_descriptor_impl::Ref_holder {

	virtual Ref ref() const = 0;
	
	virtual ~Ref_holder() {}
	
	template<bool R, bool W> struct Impl;
	
};

template<bool R, bool W>
struct Data_descriptor_impl::Ref_holder::Impl: Data_descriptor_impl::Ref_holder {

	Ref_any<R, W> m_t;
	
	
	Impl(Ref t):
		m_t(t)
	{}
	
	Ref ref() const override
	{
		return m_t;
	}
	
};


Data_descriptor_impl::Data_descriptor_impl(Global_context& ctx, const char* name):
	m_context{ctx},
	m_type{UNDEF_TYPE.clone_type()},
	m_name{name},
	m_metadata{false}
{
}

Data_descriptor_impl::Data_descriptor_impl(Data_descriptor_impl&&) = default;

Data_descriptor_impl::~Data_descriptor_impl()
{
	// release metadata placeholder
	if ( metadata() && m_refs.size() == 1) m_refs.pop();
	
	// on error, we might be destroyed while not empty.
	if (!m_refs.empty()) {
		m_context.logger()->warn("Remaining {} reference(s) to `{}' in PDI after program end", m_refs.size()-(metadata()?1:0), m_name);
		// leak the remaining data
		while ( !m_refs.empty() ) {
			m_refs.top().release();
			m_refs.pop();
		}
	}
	
	assert(m_refs.empty());
}

void Data_descriptor_impl::default_type(Datatype_template_uptr type)
{
	m_type = move(type);
}

Datatype_template_uptr Data_descriptor_impl::default_type()
{
	return m_type->clone();
}

bool Data_descriptor_impl::metadata() const
{
	return m_metadata;
}

void Data_descriptor_impl::metadata(bool metadata)
{
	assert((!m_metadata || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if ( !m_refs.empty() ) {
		throw State_error{"Can not change the metadata status of a non-empty descriptor"};
	}
	m_metadata = metadata;
	
	// for metadata, ensure we have a placeholder ref at stack bottom
	if ( metadata ) {
		m_refs.emplace(new Ref_holder::Impl<true, false>(Ref{}));
	}
	assert((!m_metadata || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
}

const string& Data_descriptor_impl::name() const
{
	return m_name;
}

Ref Data_descriptor_impl::ref()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if (m_refs.empty()) {
		m_context.callbacks().call_empty_desc_access_callbacks(m_name);
		
		//at least one plugin should share a Ref
		if (m_refs.empty()) {
			throw Value_error{"Cannot access a non shared value: `{}'", m_name};
		}
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return m_refs.top()->ref();
}

bool Data_descriptor_impl::empty()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return m_refs.empty();
}

void Data_descriptor_impl::share(void* data, bool read, bool write)
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	Ref r{data, &free, m_type->evaluate(m_context), read, write};
	try {
		m_context.logger()->trace("Sharing `{}' Ref with rights: R = {}, W = {}", m_name, read, write);
		share(r, false, false);
	} catch (...) {
		// on error, do not free the data as would be done automatically otherwise
		r.release();
		assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
		throw;
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
}

void* Data_descriptor_impl::share(Ref data_ref, bool read, bool write)
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// metadata must provide read access
	if (metadata() && !Ref_r(data_ref)) {
		throw Right_error{"Metadata sharing must offer read access"};
	}
	
	// make a reference and put it in the store
	void* result = nullptr;
	if (read) {
		if (write) {
			result = Ref_rw{data_ref} .get(nothrow);
			m_refs.emplace(new Ref_holder::Impl<true, true>(data_ref));
		} else {
			result = const_cast<void*>(Ref_r{data_ref} .get(nothrow));
			m_refs.emplace(new Ref_holder::Impl<true, false>(data_ref));
		}
	} else {
		if (write) {
			result = Ref_w{data_ref} .get(nothrow);
			m_refs.emplace(new Ref_holder::Impl<false, true>(data_ref));
		} else {
			m_refs.emplace(new Ref_holder::Impl<false, false>(data_ref));
		}
	}
	
	if (data_ref && !ref()) {
		m_refs.pop();
		throw Right_error{"Unable to grant requested rights"};
	}
	
	try {
		m_context.callbacks().call_data_callbacks(m_name, ref());
	} catch (const exception&) {
		m_refs.pop();
		throw;
	}
	
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	return result;
}

void Data_descriptor_impl::release()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// move reference out of the store
	if (m_refs.empty() || (m_refs.size()==1 && metadata())) throw State_error{"Cannot release a non shared value: `{}'", m_name};
	
	Ref oldref = ref();
	m_refs.pop();
	
	// if only the metadata placeholder ref remains replace it by this one
	if (metadata() && m_refs.size() == 1) {
		m_refs.pop();
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref));
	}
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
}

void* Data_descriptor_impl::reclaim()
{
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	if (m_refs.empty() || (m_refs.size()==1 && metadata())) throw State_error{"Cannot reclaim a non shared value: `{}'", m_name};
	
	Ref oldref = ref();
	m_refs.pop();
	
	// if only the metadata placeholder ref remains replace it by a copy of this one
	if (metadata() && m_refs.size() == 1) {
		m_refs.pop();
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref.copy()));
	}
	
	assert((!metadata() || !m_refs.empty()) && "metadata descriptors should always keep a placeholder");
	// finally release the data behind the ref
	return oldref.release();
}

} // namespace PDI
