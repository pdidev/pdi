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

#include <iostream>
#include <memory>

#include "pdi/context.h"
#include "pdi/data_reference.h"
#include "pdi/data_type.h"
#include "pdi/status.h"

#include "pdi/data_descriptor.h"
#include "pdi/plugin.h"


namespace PDI
{

using std::cerr;
using std::endl;
using std::stack;
using std::string;
using std::unique_ptr;


Data_descriptor::Data_descriptor(Context &ctx, const char *name):
	m_context{ctx},
	m_config(PC_parse_string("")),
	m_metadata{false},
	m_type{UNDEF_TYPE.clone_type()},
	m_name{name}
{
}

Data_descriptor::~Data_descriptor()
{
	if (metadata()) {
		// release metadata we kept
		if (!m_refs.empty()) m_refs.pop();
	}
	/* on error, we might be destroyed while not empty. In that case, don't keep
	 * ownership
	 */
	while (!m_refs.empty()) {
		reclaim();
	}
}

void Data_descriptor::creation_template(PC_tree_t config)
{
	m_type = Data_type::load(config);
	m_config = config;
}

Data_ref Data_descriptor::ref()
{
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot access a non shared value"};
	return m_refs.top()->ref();
}

void Data_descriptor::share(void *data, bool read, bool write)
{
	if (!data) {
		throw Error{PDI_ERR_VALUE, "Sharing null pointers is not allowed"};
	}
	
	share(Data_ref{data, &free, m_type->evaluate(m_context), read, write}, false, false);
}

void *Data_descriptor::share(Data_ref data_ref, bool read, bool write)
{
	// metadata must provide read access
	if (metadata() && !Data_r_ref(data_ref)) {
		throw Error{PDI_ERR_RIGHT, "Metadata sharing must offer read access"};
	}
	
	// for metadata, unlink happens on share
	if (!m_refs.empty() && metadata()) {
		m_refs.pop();
	}
	
	// make a reference and put it in the store
	void *result = nullptr;
	if (read) {
		if (write) {
			result = Data_rw_ref{data_ref} .get();
			m_refs.emplace(new Ref_A_holder<true, true>(data_ref));
		} else {
			result = const_cast<void *>(Data_r_ref{data_ref} .get());
			m_refs.emplace(new Ref_A_holder<true, false>(data_ref));
		}
	} else {
		if (write) {
			result = Data_w_ref{data_ref} .get();
			m_refs.emplace(new Ref_A_holder<false, true>(data_ref));
		} else {
			m_refs.emplace(new Ref_A_holder<false, false>(data_ref));
		}
	}
	
	if (!ref()) {
		m_refs.pop();
		throw Error{PDI_ERR_RIGHT, "Unable to grant requested rights"};
	}
	
	for (auto &elmnt : m_context.plugins) {
		try { // ignore errors here, try our best to notify everyone
			elmnt.second->data(m_name.c_str(), ref());
			//TODO: concatenate errors in some way
			//TODO: remove the faulty plugin in case of error?
		} catch (const std::exception &e) {
			cerr << "Error while triggering event " << m_name << " for plugin " << elmnt.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while triggering event " << m_name << " for plugin " << elmnt.first << endl;
		}
	}
	
	return result;
}

void Data_descriptor::release()
{
	// move reference out of the store
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot release a non shared value"};
	
	Data_ref oldref = ref();
	m_refs.pop();
	if (metadata()) {
		// re-share ourselves & keep a read ref to be used
		m_refs.emplace(new Ref_A_holder<true, false>(oldref));
	}
}

void Data_descriptor::reclaim()
{
	if (m_refs.empty()) throw Error{PDI_ERR_VALUE, "Cannot reclaim a non shared value"};
	
	Data_ref oldref = ref();
	m_refs.pop();
	if (metadata()) {
		// if the content is a metadata, keep a copy
		m_refs.emplace(new Ref_A_holder<true, false>(oldref.copy()));
	}
	oldref.release();
}

} // namespace PDI
