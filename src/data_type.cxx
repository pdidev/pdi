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

namespace
{

/// ordering of array
enum class Array_order: uint8_t { C, FORTRAN };

/** Return a reordered index, i.e. a C style index given either a C for Fortran
 *  style one
 * \param ordered_index the initial C or Fortran ordered index
 * \param order the style of the initial index (C/Fortran)
 * \param size the size of the dimension the index accesses
 * \return the reordered index (C style)
 */
int ridx(int ordered_index, Array_order order, int size)
{
	if (order == Array_order::FORTRAN) return size - ordered_index - 1;
	return ordered_index;
}

Data_type_uptr to_scalar_datatype(PC_tree_t node)
{
	string type;
	long kind;
	try {
		type = to_string(PC_get(node, ".type"));
		try {
			kind = to_long(PC_get(node, ".kind"));
		} catch (const Error &) {
			kind = 0;
		}
	} catch (const Error &) {
		type = to_string(node);
		kind = 0;
	}
	
	// For Fortran, we assume kind means number of bytes... TODO: autodetect
	if (type == "char" && kind == 0) {  // C char
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::UNSIGNED, sizeof(char)}};
	} else if (type == "int" && kind == 0) {  // C int
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, sizeof(int)}};
	} else if (type == "int8"  && kind == 0) {  // C int8
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, 1}};
	} else if (type == "int16"  && kind == 0) {  // C int16
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, 2}};
	} else if (type == "int32"  && kind == 0) {  // C int32
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, 4}};
	} else if (type == "int64"  && kind == 0) {  // C int64
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, 8}};
	} else if (type == "float"  && kind == 0) {  // C float
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::FLOAT, 4}};
	} else if (type == "double"  && kind == 0) {  // C double
			return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::FLOAT, 8}};
	} else if (type == "character") {     // Fortran character
		if (kind == 0) kind = PDI_CHARACTER_DEFAULT_KIND;
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::UNSIGNED, kind}};
	} else if (type == "integer") { // Fortran integer
		if (kind == 0) kind = PDI_INTEGER_DEFAULT_KIND;
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::SIGNED, kind}};
	} else if (type == "logical") { // Fortran logical
		if (kind == 0) kind = PDI_LOGICAL_DEFAULT_KIND;
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::UNSIGNED, kind}};
	} else if (type == "real") { // Fortran real
		if (kind == 0) kind = PDI_REAL_DEFAULT_KIND;
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{Scalar_datatype::FLOAT, kind}};
	}
	throw Error{PDI_ERR_VALUE, "Invalid scalar type: `%s(kind=%d)'", type.c_str(), kind};
}

Data_type_uptr to_array_datatype(PC_tree_t node)
{
	// Order: C or fortran ordering, default is C
	Array_order order = Array_order::C;
	{
		string order_str;
		try {
			order_str = to_string(PC_get(node, ".order"));
		} catch (const Error &) {
			order_str = "c";
		}
		if (order_str == "c" || order_str == "C") {
			order = Array_order::C;
		} else if (order_str == "fortran" || order_str == "Fortran") {
			order = Array_order::FORTRAN;
		} else {
			throw Error{PDI_ERR_CONFIG, "Incorrect array ordering: `%s'", order_str.c_str()};
		}
	}
	
	vector<Value> sizes;
	PC_tree_t conf_sizes = PC_get(node, ".sizes");
	if ( !PC_status(conf_sizes) ) { // multi dim array
		int nsizes = len(conf_sizes);
		for (int ii = 0; ii < nsizes; ++ii) {
			sizes.emplace_back(to_string(PC_get(node, ".sizes[%d]", ridx(ii, order, nsizes))));
		}
	} else { // else we expect a single dim array
		sizes.emplace_back(to_string(PC_get(node, ".size")));
	}
	
	vector<Value> subsizes;
	PC_tree_t conf_subsizes = PC_get(node, ".subsizes");
	if ( !PC_status(conf_subsizes) ) {
		size_t nsubsizes = len(conf_subsizes);
		if ( nsubsizes != sizes.size() ) {
			throw Error{PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", nsubsizes, sizes.size()};
		}
		for (size_t ii = 0; ii < nsubsizes; ++ii) {
			subsizes.emplace_back(to_string(PC_get(node, ".subsizes[%d]", ridx(ii, order, nsubsizes))));
		}
	} else {
		PC_tree_t conf_subsize = PC_get(node, ".subsize");
		if ( !PC_status(conf_subsize) ) {
			if ( sizes.size() != 1 ) {
				throw Error{PDI_ERR_CONFIG, "Invalid single subsize for %dD array", sizes.size()};
			}
			subsizes.emplace_back(to_string(conf_subsize));
		} else {
			subsizes = sizes;
		}
	}
	
	vector<Value> starts;
	PC_tree_t conf_starts = PC_get(node, ".starts");
	if ( !PC_status(conf_starts) ) {
		size_t nstarts = len(conf_starts);
		if ( nstarts != sizes.size() ) {
			throw Error{PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", nstarts, sizes.size()};
		}
		for (size_t ii = 0; ii < nstarts; ++ii) {
			starts.emplace_back(to_string(PC_get(node, ".starts[%d]", ridx(ii, order, nstarts))));
		}
	} else {
		PC_tree_t conf_start = PC_get(node, ".start");
		if ( !PC_status(conf_start) ) {
			if ( sizes.size() != 1 ) {
				throw Error{PDI_ERR_CONFIG, "Invalid single start for %dD array", sizes.size()};
			}
			starts.emplace_back(to_string(conf_start));
		} else {
			starts = vector<Value>(sizes.size(), Value{0});
		}
	}
	
	Data_type_uptr res_type = Data_type::load(PC_get(node, ".type"));
	
	for (size_t ii = 0; ii < sizes.size(); ++ii) {
		res_type.reset(new Array_datatype(move(res_type), std::move(sizes[ii]), std::move(starts[ii]), std::move(subsizes[ii])));
	}
	return res_type;
}

} // namespace <anonymous>


Data_type::~Data_type() {}

Data_type_uptr Data_type::load(PC_tree_t node)
{
	// size or sizes => array
	if ( !PC_status(PC_get(node, ".size")) || !PC_status(PC_get(node, ".sizes")) ) {
		return to_array_datatype(node);
	}
	return to_scalar_datatype(node);
}


Data_type_uptr Scalar_datatype::clone() const
{
	return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size, m_align}};
}

Data_type_uptr Scalar_datatype::densify() const
{
	return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size, m_align}};
}

Data_type_uptr Scalar_datatype::evaluate() const
{
	return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size.to_long(), m_align.to_long()}};
}

Data_type_uptr Array_datatype::clone() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->clone(), m_size, m_start, m_subsize}};
}

Data_type_uptr Array_datatype::densify() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->densify(), m_subsize}};
}

Data_type_uptr Array_datatype::evaluate() const
{
	return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->evaluate(), m_size.to_long(), m_start.to_long(), m_subsize.to_long()}};
}

bool Array_datatype::dense() const
{
	if ( m_size.to_long() != m_subsize.to_long() ) return false;
	return m_subtype->dense();
}

size_t Array_datatype::datasize() const
{
	return m_subsize.to_long() * m_subtype->datasize();
}

size_t Array_datatype::buffersize() const
{
	return m_size.to_long() * m_subtype->datasize();
}

size_t Array_datatype::alignment() const
{
	return m_subtype->alignment();
}

Data_type_uptr Record_datatype::clone() const
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
		evaluated_members.emplace_back(member.displacement().to_long(), member.type().evaluate(), member.name());
	}
	return unique_ptr<Record_datatype>{new Record_datatype{move(evaluated_members), m_buffersize.to_long()}};
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
