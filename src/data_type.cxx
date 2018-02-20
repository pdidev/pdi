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

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <memory>
#include <string>

#include "pdi/paraconf_wrapper.h"
#include "pdi/status.h"
#include "pdi/value.h"

#include "pdi/data_type.h"


namespace PDI
{

using std::back_inserter;
using std::max;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;


Data_type_uptr Scalar_datatype::clone_type() const
{
	return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size, m_align}};
}

Data_type_uptr Scalar_datatype::densify() const
{
	return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size, m_align}};
}

Data_type_uptr Scalar_datatype::evaluate() const
{
	return clone_type();
}

Data_type_uptr Array_datatype::clone_type() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->clone_type(), m_size, m_start, m_subsize}};
}

Data_type_uptr Array_datatype::densify() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->densify(), m_subsize}};
}

Data_type_uptr Array_datatype::evaluate() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->evaluate(), m_size, m_start, m_subsize}};
}

bool Array_datatype::dense() const
{
	if ( m_size != m_subsize ) return false;
	return m_subtype->dense();
}

size_t Array_datatype::datasize() const
{
	return m_subsize * m_subtype->datasize();
}

size_t Array_datatype::buffersize() const
{
	return m_size * m_subtype->datasize();
}

size_t Array_datatype::alignment() const
{
	return m_subtype->alignment();
}

Data_type_uptr Record_datatype::clone_type() const
{
	return unique_ptr<Record_datatype>{new Record_datatype{vector<Member>(m_members), Value{m_buffersize}}};
}

Data_type_uptr Record_datatype::densify() const
{
	long displacement = 0;
	vector<Record_datatype::Member> densified_members;
	for ( auto&& member: m_members ) {
		densified_members.emplace_back(displacement, member.type().densify(), member.name());
		displacement += densified_members.back().type().datasize();
	}
	return unique_ptr<Record_datatype>{new Record_datatype{move(densified_members), Value{m_buffersize}}};
}

Data_type_uptr Record_datatype::evaluate() const
{
	vector<Record_datatype::Member> evaluated_members;
	for ( auto&& member: m_members ) {
		evaluated_members.emplace_back(member.displacement(), member.type().evaluate(), member.name());
	}
	return unique_ptr<Record_datatype>{new Record_datatype{move(evaluated_members), m_buffersize}};
}

bool Record_datatype::dense() const
{
	throw Error{PDI_ERR_IMPL, "Record support incomplete"};
}

size_t Record_datatype::datasize() const
{
	size_t result = 0;
	for ( auto&& member: m_members ) {
		result += member.type().datasize();
	}
	return result;
}

size_t Record_datatype::alignment() const
{
	size_t result = 0;
	for ( auto&& member: m_members ) {
		result = max(result, member.type().alignment());
	}
	return result;
}

} // namespace PDI
