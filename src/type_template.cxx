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

#include "pdi.h"
#include "pdi/data_type.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/status.h"
#include "pdi/value.h"

#include "pdi/type_template.h"

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

class Scalar_template:
public Type_template
{
private:
	/// Interpretation of the content
	Scalar_kind m_kind;
	
	/// Size of the content in bytes or 0 if unknown
	Value m_size;
	
	/// Size of the alignment in bytes
	Value m_align;
	
public:
	Scalar_template(Scalar_kind kind, const Value &size): m_kind{kind}, m_size{size}, m_align{size} {}
	
	Scalar_template(Scalar_kind kind, const Value &size, const Value &align): m_kind{kind}, m_size{size}, m_align{align} {}
	
	Type_template_uptr clone() const override
	{
		return unique_ptr<Scalar_template>{new Scalar_template{m_kind, m_size, m_align}};
	}
	
	Data_type_uptr evaluate() const override
	{
		return unique_ptr<Scalar_datatype>{new Scalar_datatype{m_kind, m_size.to_long(), m_align.to_long()}};
	}
	
};

class Array_template:
public Type_template
{
	/// Type of the elements contained in the array.
	Type_template_uptr m_subtype;
	
	/// Number of elements the array can store
	Value m_size;
	
	/// id of the first actual element of the array
	Value m_start;
	
	/// Number of actual elements in the array
	Value m_subsize;
	
public:
	Array_template(Type_template_uptr subtype, Value size, Value start, Value subsize): m_subtype {std::move(subtype)}, m_size{std::move(size)}, m_start{std::move(start)}, m_subsize{std::move(subsize)} {}
	
	Array_template(Type_template_uptr subtype, Value size): Array_template{std::move(subtype), size, 0, std::move(size)} {}
	
	Type_template_uptr clone() const override
	{
		return unique_ptr<Array_template>{new Array_template{m_subtype->clone(), m_size, m_start, m_subsize}};
	}
	
	Data_type_uptr evaluate() const override
	{
		return unique_ptr<Array_datatype>{new Array_datatype{m_subtype->evaluate(), m_size.to_long(), m_start.to_long(), m_subsize.to_long()}};
	}
	
};

class Record_template:
public Type_template
{
	struct Member
	{
		/// Offset or distance in byte from the Record_template start
		Value m_displacement;
		
		/// Type of the contained member
		Type_template_uptr m_type;
		
		std::string m_name;
		
		Member( Value displacement, Type_template_uptr type, const std::string& name ): m_displacement{std::move(displacement)}, m_type{std::move(type)}, m_name{name} {}
		
		Member( const Member& o ): m_displacement{o.m_displacement}, m_type{o.m_type->clone()}, m_name{o.m_name} {}
		
	};
	
	/// All members in increasing displacement order
	std::vector<Member> m_members;
	
	/// The total size of the buffer containing all members
	Value m_buffersize;
	
public:
	Record_template(std::vector<Member> &&members, Value &&size): m_members{move(members)}, m_buffersize{std::move(size)} {}
	
	Type_template_uptr clone() const override
	{
		return unique_ptr<Record_template>{new Record_template{vector<Member>(m_members), Value{m_buffersize}}};
	}
	
	Data_type_uptr evaluate() const override
	{
		vector<Record_datatype::Member> evaluated_members;
		for ( auto&& member: m_members ) {
			evaluated_members.emplace_back(member.m_displacement.to_long(), member.m_type->evaluate(), member.m_name);
		}
		return unique_ptr<Record_datatype>{new Record_datatype{move(evaluated_members), m_buffersize.to_long()}};
	}
	
};

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

Type_template_uptr to_scalar_datatype_template(PC_tree_t node)
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
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::UNSIGNED, sizeof(char)}};
	} else if (type == "int" && kind == 0) {  // C int
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, sizeof(int)}};
	} else if (type == "int8"  && kind == 0) {  // C int8
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, 1}};
	} else if (type == "int16"  && kind == 0) {  // C int16
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, 2}};
	} else if (type == "int32"  && kind == 0) {  // C int32
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, 4}};
	} else if (type == "int64"  && kind == 0) {  // C int64
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, 8}};
	} else if (type == "float"  && kind == 0) {  // C float
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::FLOAT, 4}};
	} else if (type == "double"  && kind == 0) {  // C double
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::FLOAT, 8}};
	} else if (type == "character") {     // Fortran character
		if (kind == 0) kind = PDI_CHARACTER_DEFAULT_KIND;
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	} else if (type == "integer") { // Fortran integer
		if (kind == 0) kind = PDI_INTEGER_DEFAULT_KIND;
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::SIGNED, kind}};
	} else if (type == "logical") { // Fortran logical
		if (kind == 0) kind = PDI_LOGICAL_DEFAULT_KIND;
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	} else if (type == "real") { // Fortran real
		if (kind == 0) kind = PDI_REAL_DEFAULT_KIND;
		return unique_ptr<Scalar_template>{new Scalar_template{Scalar_kind::FLOAT, kind}};
	}
	throw Error{PDI_ERR_VALUE, "Invalid scalar type: `%s(kind=%d)'", type.c_str(), kind};
}

Type_template_uptr to_array_datatype_template(PC_tree_t node)
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
	
	Type_template_uptr res_type = Type_template::load(PC_get(node, ".type"));
	
	for (size_t ii = 0; ii < sizes.size(); ++ii) {
		res_type.reset(new Array_template(move(res_type), std::move(sizes[ii]), std::move(starts[ii]), std::move(subsizes[ii])));
	}
	return res_type;
}

} // namespace <anonymous>

Type_template::~Type_template() {}

Type_template_uptr Type_template::load(PC_tree_t node)
{
	// size or sizes => array
	if ( !PC_status(PC_get(node, ".size")) || !PC_status(PC_get(node, ".sizes")) ) {
		return to_array_datatype_template(node);
	}
	return to_scalar_datatype_template(node);
}
	
} // namespace PDI
