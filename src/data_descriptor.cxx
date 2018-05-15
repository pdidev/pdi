/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <iostream>
#include <memory>
#include <vector>

#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/scalar_datatype.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/error.h"

#include "pdi/data_descriptor.h"


namespace PDI {

using std::cerr;
using std::endl;
using std::exception;
using std::nothrow;
using std::stack;
using std::string;
using std::unique_ptr;
using std::vector;

struct Data_descriptor::Ref_holder {

	virtual Ref ref() const = 0;
	
	virtual ~Ref_holder() {}
	
	template<bool R, bool W> struct Impl;
	
};

template<bool R, bool W>
struct Data_descriptor::Ref_holder::Impl: Data_descriptor::Ref_holder {

	Ref_any<R, W> m_t;
	
	
	Impl(Ref t):
		m_t(t)
	{}
	
	Ref ref() const override
	{
		return m_t;
	}
	
};


Data_descriptor::Data_descriptor(Context& ctx, const char* name):
	m_context{ctx},
	m_config(PC_parse_string("")),
	m_type{UNDEF_TYPE.clone_type()},
	m_name{name},
	m_metadata{false}
{
}

Data_descriptor::Data_descriptor(Data_descriptor&&) = default;

Data_descriptor::~Data_descriptor()
{
	// release metadata copies we kept
	if ( metadata() && m_refs.size() == 1) m_refs.pop();
	
	/* on error, we might be destroyed while not empty. In that case, don't keep
	 * ownership
	 */
	if (!m_refs.empty()) {
		std::cerr<<" *** [PDI] Warning: Remaining "<<m_refs.size()<<" reference(s) to `"<<m_name<<"' in PDI after program end"<<std::endl;
	}
	if ( metadata()  ) while (!m_refs.empty()) m_refs.pop();
	if ( !metadata() ) while (!m_refs.empty()) reclaim();
}

void Data_descriptor::creation_template(PC_tree_t config)
{
	m_type = Datatype::load(config);
	m_config = config;
}

PC_tree_t Data_descriptor::config() const
{
	return m_config;
}

bool Data_descriptor::metadata() const
{
	return m_metadata;
}

void Data_descriptor::metadata(bool metadata)
{
	m_metadata = metadata;
}

const std::string& Data_descriptor::name() const
{
	return m_name;
}

Ref Data_descriptor::ref()
{
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot access a non shared value: `%s'", m_name.c_str()};
	return m_refs.top()->ref();
}

void Data_descriptor::share(void* data, bool read, bool write)
{
	Ref r{data, &free, m_type->evaluate(m_context), read, write};
	try {
		share(r, false, false);
	} catch (...) {
		// on error, do not free the data as would be done automatically otherwise
		r.release();
	}
}

void* Data_descriptor::share(Ref data_ref, bool read, bool write)
{
	// metadata must provide read access
	if (metadata() && !Ref_r(data_ref)) {
		throw Error{PDI_ERR_RIGHT, "Metadata sharing must offer read access"};
	}
	
	// for metadata, unlink happens on share
	if (!m_refs.empty() && metadata()) {
		m_refs.pop();
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
		throw Error{PDI_ERR_RIGHT, "Unable to grant requested rights"};
	}
	
	for (auto& elmnt : m_context.plugins) {
		vector<Error> errors;
		try {
			elmnt.second->data(m_name.c_str(), ref());
			//TODO: remove the faulty plugin in case of error?
		} catch (const Error& e) {
			errors.emplace_back(e.status(), "for plugin `%s': %s", elmnt.first.c_str(), e.what());
		} catch (const std::exception& e) {
			errors.emplace_back(PDI_ERR_SYSTEM, "for plugin `%s': %s", elmnt.first.c_str(), e.what());
		} catch (...) {
			errors.emplace_back(PDI_ERR_SYSTEM, "for plugin `%s'", elmnt.first.c_str());
		}
		if ( 1 == errors.size() ) throw Error{errors.front().status(), "While triggering data event `%s' %s", m_name.c_str(), errors.front().what()};
		if ( !errors.empty() ) {
			string errmsg = "multiple errors while triggering data event `"+m_name+"':";
			for ( auto&& err: errors ) {
				errmsg += " ";
				errmsg += err.what();
			}
			throw Error{PDI_ERR_PLUGIN, "%s", errmsg.c_str()};
		}
	}
	
	return result;
}

void Data_descriptor::release()
{
	// move reference out of the store
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot release a non shared value"};
	
	Ref oldref = ref();
	m_refs.pop();
	if (metadata()) {
		// re-share ourselves & keep a read ref to be used
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref));
	}
}

void Data_descriptor::reclaim()
{
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot reclaim a non shared value"};
	
	Ref oldref = ref();
	m_refs.pop();
	if (metadata()) {
		// if the content is a metadata, keep a copy
		m_refs.emplace(new Ref_holder::Impl<true, false>(oldref.copy()));
	}
	oldref.release();
}

} // namespace PDI
