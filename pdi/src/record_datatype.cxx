/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <algorithm>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/ref_any.h"

#include "pdi/record_datatype.h"

namespace PDI {

using std::align;
using std::endl;
using std::make_shared;
using std::max;
using std::move;
using std::pair;
using std::regex;
using std::regex_replace;
using std::shared_ptr;
using std::static_pointer_cast;
using std::string;
using std::stringstream;
using std::unique_ptr;
using std::vector;

Record_datatype::Member::Member(size_t displacement, Datatype_sptr type, const string& name)
	: m_displacement{displacement}
	, m_type{std::move(type)}
	, m_name{name}
{}

Record_datatype::Member::Member(const Member& o)
	: m_displacement{o.m_displacement}
	, m_type{o.m_type}
	, m_name{o.m_name}
{}

size_t Record_datatype::Member::displacement() const
{
	return m_displacement;
}

Datatype_sptr Record_datatype::Member::type() const
{
	return m_type;
}

const string& Record_datatype::Member::name() const
{
	return m_name;
}

bool Record_datatype::Member::operator== (const Member& rhs) const
{
	return m_displacement == rhs.m_displacement && m_name == rhs.m_name && *m_type == *rhs.m_type;
}

bool Record_datatype::Member::operator!= (const Member& rhs) const
{
	return !(*this == rhs);
}

Record_datatype::Record_datatype(vector<Member>&& members, size_t size)
	: m_members{std::move(members)}
	, m_buffersize{std::move(size)}
{}

const vector<Record_datatype::Member>& Record_datatype::members() const
{
	return m_members;
}

Datatype_sptr Record_datatype::densify() const
{
	size_t displacement = 0;
	vector<Record_datatype::Member> densified_members;
	for (auto&& member: m_members) {
		Datatype_sptr densified_type = member.type()->densify();
		size_t alignment = densified_type->alignment();
		// align the next member as requested
		displacement += (alignment - (displacement % alignment)) % alignment;
		densified_members.emplace_back(displacement, std::move(densified_type), member.name());
		displacement += densified_members.back().type()->buffersize();
	}
	//add padding at the end of record
	size_t record_alignment = alignment();
	displacement += (record_alignment - (displacement % record_alignment)) % record_alignment;

	// ensure the record size is at least 1 to have a unique address
	displacement = max<size_t>(1, displacement);
	return unique_ptr<Record_datatype>{new Record_datatype{std::move(densified_members), displacement}};
}

bool Record_datatype::dense() const
{
	size_t displacement = 0;
	for (auto&& member: m_members) {
		if (!member.type()->dense()) return false;
		size_t alignment = member.type()->alignment();
		// add space for alignment
		displacement += (alignment - (displacement % alignment)) % alignment;
		if (member.displacement() > displacement) return false;
		displacement += member.type()->buffersize();
	}
	//add padding at the end of record
	size_t record_alignment = alignment();
	displacement += (record_alignment - (displacement % record_alignment)) % record_alignment;

	// accept 1 extra byte for unique address
	displacement = max<size_t>(1, displacement);
	if (buffersize() > displacement) return false;
	return true;
}

size_t Record_datatype::datasize() const
{
	size_t result = 0;
	for (auto&& member: m_members) {
		result += member.type()->datasize();
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
	for (auto&& member: m_members) {
		result = max(result, member.type()->alignment());
	}
	return result;
}

bool Record_datatype::simple() const
{
	for (auto&& member: m_members) {
		if (!member.type()->simple()) return false;
	}
	return true;
}

void* Record_datatype::data_to_dense_copy(void* to, const void* from) const
{
	if (simple() && dense()) {
		//dense copy
		memcpy(to, from, buffersize());
		to = reinterpret_cast<uint8_t*>(to) + buffersize();
		return to;
	}

	for (auto&& member: members()) {
		//space_to_align is set to alignment(), because we always find the alignment in the size of alignment
		auto space_to_align = member.type()->alignment();
		//size = 0, because we know that to points to allocated memory
		to = align(member.type()->alignment(), 0, to, space_to_align);
		const uint8_t* member_from = reinterpret_cast<const uint8_t*>(from) + member.displacement();
		to = member.type()->data_to_dense_copy(to, member_from);
	}
	return to;
}

void* Record_datatype::data_from_dense_copy(void* to, const void* from) const
{
	uint8_t* original_to = reinterpret_cast<uint8_t*>(to);

	if (simple() && dense()) {
		//dense copy
		memcpy(to, from, buffersize());
		to = original_to + buffersize();
		return to;
	}

	for (auto&& member: members()) {
		auto member_align = member.type()->alignment();
		int padding = (member_align - (reinterpret_cast<const uintptr_t>(from) % member_align)) % member_align;
		from = reinterpret_cast<const uint8_t*>(from) + padding;
		to = original_to + member.displacement();

		member.type()->data_from_dense_copy(to, from);
		from = reinterpret_cast<const uint8_t*>(from) + member.type()->datasize();
	}
	to = original_to + buffersize();
	return to;
}

Datatype_sptr Record_datatype::member(const char* name) const
{
	auto member_it = find_if(members().begin(), members().end(), [=](const Member& member) { return name == member.name(); });
	if (member_it == members().end()) {
		throw Value_error{"Record subaccess error: no member named {}", name};
	}
	return member_it->type();
}

std::pair<void*, Datatype_sptr> Record_datatype::member(const char* name, void* data) const
{
	auto member_it = find_if(members().begin(), members().end(), [=](const Member& member) { return name == member.name(); });
	if (member_it == members().end()) {
		throw Value_error{"Record subaccess error: no member named {}", name};
	}
	data = reinterpret_cast<uint8_t*>(data) + member_it->displacement();
	return {data, member_it->type()};
}

void Record_datatype::destroy_data(void* ptr) const
{
	if (simple()) return;
	for (auto&& member: members()) {
		member.type()->destroy_data(reinterpret_cast<uint8_t*>(ptr) + member.displacement());
	}
}

string Record_datatype::debug_string() const
{
	stringstream ss;
	ss << "type: record" << endl
	   << "dense: " << (dense() ? "true" : "false") << endl
	   << "buffersize: " << buffersize() << endl
	   << "datasize: " << datasize() << endl
	   << "alignment: " << alignment() << endl
	   << "members: ";
	for (auto&& member: members()) {
		auto type_str = member.type()->debug_string();
		type_str = regex_replace(type_str, regex("\n"), "\n\t\t");
		type_str.insert(0, "\t\t");
		ss << endl
		   << "\t - name: " << member.name() << endl
		   << "\t   displacement: " << member.displacement() << endl
		   << "\t   type: " << endl
		   << type_str;
	}
	if (!attributes().empty()) {
		ss << endl << "attributes: " << endl;
		auto it = attributes().begin();
		for (; next(it) != attributes().end(); it++) {
			ss << "\t" << it->first << ", ";
		}
		ss << "\t" << it->first;
	}
	return ss.str();
}

bool Record_datatype::operator== (const Datatype& other) const
{
	const Record_datatype* rhs = dynamic_cast<const Record_datatype*>(&other);
	return rhs && m_buffersize == rhs->m_buffersize && m_members == rhs->m_members;
}

struct Record_datatype::Shared_enabler: public Record_datatype {
	Shared_enabler(vector<Member>&& members, size_t size, const Attributes_map& attributes)
		: Record_datatype(move(members), size, attributes)
	{}
};

shared_ptr<Record_datatype> Record_datatype::make(vector<Member>&& members, size_t size, const Attributes_map& attributes)
{
	return make_shared<Shared_enabler>(move(members), size, attributes);
}

} // namespace PDI
