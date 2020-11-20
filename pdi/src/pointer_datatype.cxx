/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

using std::endl;
using std::function;
using std::move;
using std::pair;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

string Pointer_datatype::Accessor::access_kind() const 
{
	return "pointer access";
}

pair<void*, Datatype_uptr> Pointer_datatype::Accessor::access(const Pointer_datatype& pointer_type,
									void* from,
									vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
									vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	from = *reinterpret_cast<void**>(from);
	if (remaining_begin == remaining_end) {
		return {from, pointer_type.subtype().clone_type()};
	} else {
		return pointer_type.subtype().subaccess_by_iterators(from, remaining_begin, remaining_end);
	}
}

Pointer_datatype::Pointer_datatype(Datatype_uptr subtype):
	m_subtype{move(subtype)}
{}

Pointer_datatype::Pointer_datatype(Datatype_uptr subtype, function<void* (void*, const void*)> copy, function<void(void*)> destroy):
	m_subtype{move(subtype)},
	m_copy{move(copy)},
	m_destroy{move(destroy)}
{}

const Datatype& Pointer_datatype::subtype() const
{
	return *m_subtype;
}

Datatype_template_uptr Pointer_datatype::clone() const
{
	return clone_type();
}

Datatype_uptr Pointer_datatype::clone_type() const
{
	return unique_ptr<Pointer_datatype> {new Pointer_datatype{m_subtype->clone_type(), m_copy, m_destroy}};
}

Datatype_uptr Pointer_datatype::densify() const
{
	return unique_ptr<Pointer_datatype> {new Pointer_datatype{m_subtype->densify(), m_copy, m_destroy}};
}

Datatype_uptr Pointer_datatype::evaluate(Context& ctx) const
{
	return clone_type();
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


pair<void*, Datatype_uptr> Pointer_datatype::subaccess_by_iterators(void* from,
															vector<unique_ptr<Accessor_base>>::const_iterator remaining_begin,
															vector<unique_ptr<Accessor_base>>::const_iterator remaining_end) const
{
	if (const Pointer_datatype::Accessor* pointer_accessor = dynamic_cast<const Pointer_datatype::Accessor*>(remaining_begin->get())) {
		return remaining_begin->get()->access(*this, from, ++remaining_begin, remaining_end);
	} else {
		from = *reinterpret_cast<void**>(from);
		return subtype().subaccess_by_iterators(from, remaining_begin, remaining_end);
	}	
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
	return ss.str();
}

bool Pointer_datatype::operator==(const Datatype& other) const
{
	const Pointer_datatype* rhs = dynamic_cast<const Pointer_datatype*>(&other);
	return rhs && *m_subtype == *rhs->m_subtype;
}

Datatype_uptr Pointer_datatype::dereference() const
{
	return m_subtype->clone_type();
}

} // namespace PDI

