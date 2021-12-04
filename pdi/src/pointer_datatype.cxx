/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <map>
#include <string>
#include <sstream>

#include "pdi/error.h"

#include "pdi/pointer_datatype.h"


namespace PDI {

using std::dynamic_pointer_cast;
using std::endl;
using std::function;
using std::make_shared;
using std::move;
using std::pair;
using std::shared_ptr;
using std::static_pointer_cast;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;


Pointer_datatype::Pointer_datatype(Datatype_sptr subtype, const Attributes_map& attributes):
	Datatype(attributes),
	m_subtype{move(subtype)}
{}

Pointer_datatype::Pointer_datatype(Datatype_sptr subtype, function<void* (void*, const void*)> copy, function<void(void*)> destroy, const Attributes_map& attributes):
	Datatype(attributes),
	m_subtype{move(subtype)},
	m_copy{move(copy)},
	m_destroy{move(destroy)}
{}

Datatype_sptr Pointer_datatype::subtype() const
{
	return m_subtype;
}

Datatype_sptr Pointer_datatype::densify() const
{
	return unique_ptr<Pointer_datatype> {new Pointer_datatype{m_subtype->densify(), m_copy, m_destroy, m_attributes}};
}

Datatype_sptr Pointer_datatype::evaluate(Context&) const
{
	return static_pointer_cast<const Datatype>(this->shared_from_this());
}

bool Pointer_datatype::dense() const
{
	return true;
}

size_t Pointer_datatype::datasize() const
{
	return sizeof(void*);
}

size_t Pointer_datatype::buffersize() const
{
	return datasize();
}

size_t Pointer_datatype::alignment() const
{
	return datasize();
}

bool Pointer_datatype::simple() const
{
	return !m_copy && !m_destroy;
}

void* Pointer_datatype::data_to_dense_copy(void* to, const void* from) const
{
	if ( !m_copy ) {
		memcpy(to, from, datasize());
		to = reinterpret_cast<uint8_t*>(to) + datasize();
	} else {
		to = m_copy(to, from);
	}
	return to;
}

void* Pointer_datatype::data_from_dense_copy(void* to, const void* from) const
{
	return data_to_dense_copy(to, from);
}

void Pointer_datatype::destroy_data(void* ptr) const
{
	if ( m_destroy ) {
		m_destroy(ptr);
	}
}

string Pointer_datatype::debug_string() const
{
	stringstream ss;
	ss << "type: pointer" << endl
	    << "dense: " << (dense() ? "true" : "false") << endl
	    << "buffersize: " << buffersize() << endl
	    << "datasize: " << datasize() << endl
	    << "alignment: " << alignment() << endl
	    << "subtype: " << endl << m_subtype->debug_string();
	if (!m_attributes.empty()) {
		ss << endl << "attributes: " << endl;
		auto it = m_attributes.begin();
		for (; next(it) != m_attributes.end(); it++) {
			ss << "\t" << it->first << ", ";
		}
		ss << "\t" << it->first;
	}
	return ss.str();
}

bool Pointer_datatype::operator==(const Datatype& other) const
{
	auto&& rhs = dynamic_cast<const Pointer_datatype*>(&other);
	return rhs && *m_subtype == *rhs->m_subtype;
}

std::pair<void*, Datatype_sptr> Pointer_datatype::index(size_t index, void* data) const
{
	return m_subtype->index(index, *static_cast<void**>(data));
}

std::pair<void*, Datatype_sptr> Pointer_datatype::member(const char* name, void* data) const
{
	return m_subtype->member(name, *static_cast<void**>(data));
}

std::pair<void*, Datatype_sptr> Pointer_datatype::slice(size_t start_index, size_t end_index, void* data) const
{
	return m_subtype->slice(start_index, end_index, *static_cast<void**>(data));
}

Datatype_sptr Pointer_datatype::index(size_t index) const
{
	return m_subtype->index(index);
}

Datatype_sptr Pointer_datatype::member(const char* name) const
{
	return m_subtype->member(name);
}

Datatype_sptr Pointer_datatype::slice(size_t start_index, size_t end_index) const
{
	return m_subtype->slice(start_index, end_index);
}

Datatype_sptr Pointer_datatype::dereference () const
{
	return subtype();
}

std::pair<void*, Datatype_sptr> Pointer_datatype::dereference ( void* data ) const
{
	data = *static_cast<void**>(data);
	return {data, subtype()};
}

struct Pointer_datatype::Shared_enabler : public Pointer_datatype {
	Shared_enabler (Datatype_sptr subtype, const Attributes_map& attributes):
		Pointer_datatype(subtype, attributes)
	{}
	
	Shared_enabler (Datatype_sptr subtype, function<void* (void*, const void*)> copy, function<void (void*)> destroy, const Attributes_map& attributes):
		Pointer_datatype(subtype, copy, destroy, attributes)
	{}
};

shared_ptr<Pointer_datatype> Pointer_datatype::make(Datatype_sptr subtype, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(subtype, attributes);
}

shared_ptr<Pointer_datatype> Pointer_datatype::make(Datatype_sptr subtype, function<void* (void*, const void*)> copy, function<void (void*)> destroy, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(subtype, copy, destroy, attributes);
}

} // namespace PDI


