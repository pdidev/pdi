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

#include <memory>

#include "pdi/data_reference.h"
#include "pdi/datatype.h"
#include "pdi/status.h"

#include "pdi/data_descriptor.h"


namespace PDI
{

using std::stack;
using std::string;
using std::unique_ptr;


Data_descriptor::Data_descriptor(const char *name):
	m_config(PC_parse_string("")),
	m_metadata(false),
	m_type{PDI_UNDEF_TYPE},
	m_name(name)
{
}

Data_descriptor::~Data_descriptor()
{
	while (!m_values.empty()) {
		if (is_metadata()) {
			/* release metadata we kept */
			m_values.pop();
		} else {
			/* on error, we might be destroyed while not empty. In that case, don't
			   keep ownership */
			reclaim();
		}
	}
}

PDI_status_t Data_descriptor::init(PC_tree_t config, bool is_metadata, const Datatype &type)
{
	m_config = config;
	m_metadata = is_metadata;
	m_type = type;
	return PDI_OK;
}

void Data_descriptor::share(void *data, std::function< void (void *) > freefunc, bool read, bool write)
{
	if (is_metadata() && !read) throw Error{PDI_ERR_RIGHT, "Metadata sharing must offer read access"};
	
	// for metadata, unlink happens on share
	if (!m_values.empty() && is_metadata()) {
		m_values.pop();
	}
	
	// make a reference and put it in the store
	m_values.push(std::unique_ptr<Ref_holder>(new Ref_A_holder<false, false>(data, freefunc, get_type(), read, write)));
	if (!value()) {
		m_values.pop();
		throw Error{PDI_ERR_RIGHT, "Unable to grant requested rights"};
	}
}

void Data_descriptor::share(Data_ref ref, bool read, bool write)
{
	if (is_metadata() && !read) throw Error{PDI_ERR_RIGHT, "Metadata sharing must offer read access"};
	
	/// for metadata, unlink happens on share
	if (!m_values.empty() && is_metadata()) {
		m_values.pop();
	}
	
	if (read) {
		if (write) {
			m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<true, true>(ref)));
		} else {
			m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<true, false>(ref)));
		}
	} else {
		if (write) {
			m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<false, true>(ref)));
		} else {
			m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<false, false>(ref)));
		}
	}
	if (!value()) {
		m_values.pop();
		throw Error{PDI_ERR_RIGHT, "Unable to grant requested rights"};
	}
}

void Data_descriptor::release()
{
	// move reference out of the store
	if (m_values.empty()) throw Error{PDI_ERR_VALUE, "Cannot release a non shared value"};
	
	Data_ref oldref = value();
	m_values.pop();
	if (is_metadata()) {
		// re-share ourselves & keep a read ref to be used
		m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<true, false>(oldref)));
	}
}

void Data_descriptor::reclaim()
{
	if (m_values.empty()) throw Error{PDI_ERR_VALUE, "Cannot reclaim a non shared value"};
	
	Data_ref oldref = value();
	m_values.pop();
	if (is_metadata()) {
		// if the content is a metadata, keep a copy
		m_values.push(unique_ptr<Ref_holder>(new Ref_A_holder<true, false>(oldref.copy())));
	}
	oldref.release();
}

const Datatype &Data_descriptor::get_type() const
{
	return m_type;
}

bool Data_descriptor::is_metadata() const
{
	return m_metadata;
}

PC_tree_t Data_descriptor::get_config() const
{
	return m_config;
}

} // namespace PDI
