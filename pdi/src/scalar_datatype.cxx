/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <sstream>

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/scalar_datatype.h"


namespace PDI
{

using std::endl;
using std::function;
using std::map;
using std::make_shared;
using std::max;
using std::move;
using std::shared_ptr;
using std::static_pointer_cast;
using std::string;
using std::stringstream;
using std::transform;
using std::unique_ptr;

namespace
{

inline bool ispow2(size_t v)
{
	return v && !(v & (v-1));
}

inline bool nulltype(const Scalar_datatype& d)
{
	if (d.buffersize()) return false;
	if (d.datasize()) return false;
	if (d.alignment() ) return false;
	if (d.kind()!= Scalar_kind::UNKNOWN ) return false;
	return true;
}

}

template <class T>
std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v = Scalar_datatype::make ( kind_of_v<T>, sizeof ( T ), alignof ( T ) );

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint8_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint16_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint32_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<uint64_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int8_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int16_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int32_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<int64_t>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<bool>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<float>;

template std::shared_ptr<Scalar_datatype> const Scalar_datatype::cv_type_for_v<double>;

Scalar_datatype::Scalar_datatype(Scalar_kind kind, size_t size, const Attributes_map& attributes):
	Datatype(attributes),
	m_size{size},
	   m_dense_size{size},
	   m_align{size},
	   m_kind{kind}
{
	if ( !nulltype(*this) && !ispow2(m_align) ) throw Value_error{"alignment should be a power of 2"};
}

Scalar_datatype::Scalar_datatype(Scalar_kind kind, size_t size, size_t align, const Attributes_map& attributes):
	Datatype(attributes),
	m_size{size},
	m_dense_size{size},
	m_align{align},
	m_kind{kind}
{
	if ( !nulltype(*this) && !ispow2(m_align) ) throw Value_error{"alignment should be a power of 2"};
}

Scalar_datatype::Scalar_datatype(Scalar_kind kind, size_t size, size_t align, size_t dense_size, std::function<void* (void*, const void*)> copy, std::function<void(void*)> destroy, const Attributes_map& attributes):
	Datatype(attributes),
	m_size{size},
	m_dense_size{dense_size},
	m_align{align},
	m_kind{kind},
	m_copy{move(copy)},
	m_destroy{move(destroy)}
{
	if ( !nulltype(*this) && !ispow2(m_align) ) throw Value_error{"alignment should be a power of 2"};
}

Scalar_kind Scalar_datatype::kind() const
{
	return m_kind;
}

Datatype_sptr Scalar_datatype::densify() const
{
	return unique_ptr<Scalar_datatype> {new Scalar_datatype{m_kind, m_dense_size, m_align, m_dense_size, m_copy, m_destroy, m_attributes}};
}

Datatype_sptr Scalar_datatype::evaluate(Context&) const
{
	return static_pointer_cast<const Datatype>(this->shared_from_this());
}

bool Scalar_datatype::dense() const
{
	return m_dense_size == m_size;
}

size_t Scalar_datatype::datasize() const
{
	return m_size;
}

size_t Scalar_datatype::buffersize() const
{
	return m_dense_size;
}

size_t Scalar_datatype::alignment() const
{
	return m_align;
}

bool Scalar_datatype::simple() const
{
	return !m_copy && !m_destroy;
}

void* Scalar_datatype::data_to_dense_copy(void* to, const void* from) const
{
	if ( !m_copy ) {
		memcpy(to, from, datasize());
		to = reinterpret_cast<uint8_t*>(to) + datasize();
	} else {
		to = m_copy(to, from);
	}
	return to;
}

void* Scalar_datatype::data_from_dense_copy(void* to, const void* from) const
{
	return data_to_dense_copy(to, from);
}

void Scalar_datatype::destroy_data(void* ptr) const
{
	if ( m_destroy ) {
		m_destroy(ptr);
	}
}

string Scalar_datatype::debug_string() const
{
	const map<Scalar_kind, string> kind_map {
		{Scalar_kind::UNKNOWN,  "unknown"},
		{Scalar_kind::SIGNED,   "signed"},
		{Scalar_kind::UNSIGNED, "unsigned"},
		{Scalar_kind::FLOAT,    "float"}
	};
	stringstream ss;
	ss << "type: scalar" << endl
	   << "kind: " << kind_map.at(kind()) << endl
	   << "dense: " << (dense() ? "true" : "false") << endl
	   << "buffersize: " << buffersize() << endl
	   << "datasize: " << datasize() << endl
	   << "alignment: " << alignment();
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

bool Scalar_datatype::operator==(const Datatype& other) const
{
	const Scalar_datatype* rhs = dynamic_cast<const Scalar_datatype*>(&other);
	return rhs
		   && m_size == rhs->m_size
		   && m_align == rhs->m_align
		   && m_kind == rhs->m_kind;
}

struct Scalar_datatype::Shared_enabler : public Scalar_datatype {
	Shared_enabler(Scalar_kind kind, size_t size, const Attributes_map& attributes= {}):
		Scalar_datatype(kind, size, attributes)
	{}

	Shared_enabler(Scalar_kind kind, size_t size, size_t align, const Attributes_map& attributes= {}):
		Scalar_datatype(kind, size, align, attributes)
	{}

	Shared_enabler(Scalar_kind kind, size_t size, size_t align, size_t dense_size, function<void* (void*, const void*)> copy, function<void (void*)> destroy, const Attributes_map& attributes= {}):
		Scalar_datatype(kind, size, align, dense_size, copy, destroy, attributes)
	{}
};

shared_ptr<Scalar_datatype> Scalar_datatype::make(Scalar_kind kind, size_t size, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(kind, size, attributes);
}

shared_ptr<Scalar_datatype> Scalar_datatype::make(Scalar_kind kind, size_t size, size_t align, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(kind, size, align, attributes);
}

shared_ptr<Scalar_datatype> Scalar_datatype::make(Scalar_kind kind, size_t size, size_t align, size_t dense_size, function<void* (void*, const void*)> copy, function<void (void*)> destroy, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(kind, size, align, dense_size, copy, destroy, attributes);
}

} // namespace PDI



