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

#include <algorithm>
#include <memory>
#include <string>
#include <vector>

#include "pdi/paraconf_wrapper.h"
#include "pdi/error.h"
#include "pdi/expression.h"

#include "pdi/record_datatype.h"


namespace PDI {

using std::max;
using std::move;
using std::string;
using std::unique_ptr;
using std::vector;

Record_datatype::Member::Member(size_t displacement, Datatype_uptr type, const string& name):
	m_displacement{displacement},
	m_type{move(type)},
	m_name{name}
{}

Record_datatype::Member::Member(const Member& o):
	m_displacement{o.m_displacement},
	m_type{o.m_type->clone_type()},
	m_name{o.m_name}
{}

size_t Record_datatype::Member::displacement() const
{
	return m_displacement;
}

const Datatype& Record_datatype::Member::type() const
{
	return *m_type;
}

const string& Record_datatype::Member::name() const
{
	return m_name;
}

Record_datatype::Record_datatype(vector<Member>&& members, size_t size):
	m_members{move(members)},
	m_buffersize{move(size)}
{}

const vector<Record_datatype::Member>& Record_datatype::members() const
{
	return m_members;
}

Datatype_template_uptr Record_datatype::clone() const
{
	return clone_type();
}

Datatype_uptr Record_datatype::clone_type() const
{
	return unique_ptr<Record_datatype> {new Record_datatype{vector<Member>(m_members), m_buffersize}};
}

Datatype_uptr Record_datatype::densify() const
{
	size_t displacement = 0;
	vector<Record_datatype::Member> densified_members;
	for (auto&& member : m_members) {
		Datatype_uptr densified_type = member.type().densify();
		size_t alignment = densified_type->alignment();
		// align the next member as requested
		displacement += (displacement + alignment - 1) % alignment;
		densified_members.emplace_back(displacement, move(densified_type), member.name());
		displacement += densified_members.back().type().buffersize();
	}
	// ensure the record size is at least 1 to have a unique address
	displacement = max<size_t>(1, displacement);
	return unique_ptr<Record_datatype> {new Record_datatype{move(densified_members), displacement}};
}

Datatype_uptr Record_datatype::evaluate(Context&) const
{
	return Record_datatype::clone_type();
}

bool Record_datatype::dense() const
{
	size_t displacement = 0;
	for (auto&& member : m_members) {
		if ( !member.type().dense() ) return false;
		size_t alignment = member.type().alignment();
		// add space for alignment
		displacement += (displacement + alignment - 1) % alignment;
		if ( member.displacement() > displacement ) return false;
		displacement += member.type().buffersize();
	}
	// accept 1 extra byte for unique address
	displacement = max<size_t>(1, displacement);
	if ( buffersize() > displacement ) return false;
	return true;
}

size_t Record_datatype::datasize() const
{
	size_t result = 0;
	for (auto&& member : m_members) {
		result += member.type().datasize();
	}
	return result;
}

size_t Record_datatype::buffersize() const
{
	return m_buffersize;
}

size_t Record_datatype::alignment() const
{
	size_t result = 1;
	for (auto&& member : m_members) {
		result = max(result, member.type().alignment());
	}
	return result;
}

} // namespace PDI
