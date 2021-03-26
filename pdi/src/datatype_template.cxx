/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <memory>
#include <string>
#include <vector>

#include <spdlog/spdlog.h>

#include "pdi.h"
#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"

#include "pdi/datatype_template.h"

namespace PDI {

using std::max;
using std::move;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

namespace {

class Scalar_template:
	public Datatype_template
{
	/// Interpretation of the content
	Scalar_kind m_kind;
	
	/// Size of the content in bytes or 0 if unknown
	Expression m_size;
	
	/// Size of the alignment in bytes
	Expression m_align;
	
public:
	Scalar_template(Scalar_kind kind, const Expression& size): m_kind{kind}, m_size{size}, m_align{size} {}
	
	Scalar_template(Scalar_kind kind, const Expression& size, const Expression& align): m_kind{kind}, m_size{size}, m_align{align} {}
	
	Datatype_template_uptr clone() const override
	{
		return unique_ptr<Scalar_template> {new Scalar_template{m_kind, m_size, m_align}};
	}
	
	Datatype_uptr evaluate(Context& ctx) const override
	{
		return unique_ptr<Scalar_datatype> {new Scalar_datatype{
				m_kind,
				static_cast<size_t>(m_size.to_long(ctx)),
				static_cast<size_t>(m_align.to_long(ctx))
			}
		};
	}
	
};

class Array_template:
	public Datatype_template
{
	/// Type of the elements contained in the array.
	Datatype_template_uptr m_subtype;
	
	/// Number of elements the array can store
	Expression m_size;
	
	/// id of the first actual element of the array
	Expression m_start;
	
	/// Number of actual elements in the array
	Expression m_subsize;
	
public:
	Array_template(Datatype_template_uptr subtype, Expression size, Expression start, Expression subsize): m_subtype {move(subtype)}, m_size{move(size)}, m_start{move(start)}, m_subsize{move(subsize)} {}
	
	Datatype_template_uptr clone() const override
	{
		return unique_ptr<Array_template> {new Array_template{m_subtype->clone(), m_size, m_start, m_subsize}};
	}
	
	Datatype_uptr evaluate(Context& ctx) const override
	{
		return unique_ptr<Array_datatype> {new Array_datatype{
				m_subtype->evaluate(ctx),
				static_cast<size_t>(m_size.to_long(ctx)),
				static_cast<size_t>(m_start.to_long(ctx)),
				static_cast<size_t>(m_subsize.to_long(ctx))
			}
		};
	}
	
};

class Record_template:
	public Datatype_template
{
public:
	struct Member {
	
		/// Offset or distance in byte from the Record_template start
		Expression m_displacement;
		
		/// Type of the contained member
		Datatype_template_uptr m_type;
		
		string m_name;
		
		Member(Expression disp, Datatype_template_uptr type, string name):
			m_displacement{move(disp)},
			m_type{move(type)},
			m_name{move(name)}
		{}
		
		Member(const Member& o): m_displacement{o.m_displacement}, m_type{o.m_type->clone()}, m_name{o.m_name} {}
	};
	
private:
	/// All members in increasing displacement order
	vector<Member> m_members;
	
	/// The total size of the buffer containing all members
	Expression m_buffersize;
	
public:
	Record_template(vector<Member>&& members, Expression&& size): m_members{move(members)}, m_buffersize{move(size)} {}
	
	Datatype_template_uptr clone() const override
	{
		return unique_ptr<Record_template> {new Record_template{vector<Member>(m_members), Expression{m_buffersize}}};
	}
	
	Datatype_uptr evaluate(Context& ctx) const override
	{
		vector<Record_datatype::Member> evaluated_members;
		for (auto&& member : m_members) {
			evaluated_members.emplace_back(member.m_displacement.to_long(ctx), member.m_type->evaluate(ctx), member.m_name);
		}
		return unique_ptr<Record_datatype> {new Record_datatype{move(evaluated_members), static_cast<size_t>(m_buffersize.to_long(ctx))}};
	}
	
};

class Struct_template:
	public Datatype_template
{
public:
	struct Member {
		/// Type of the contained member
		Datatype_template_uptr m_type;
		string m_name;
		
		Member(Datatype_template_uptr type, string name):
			m_type{move(type)},
			m_name{move(name)}
		{}
		
		Member(const Member& o):
			m_type{o.m_type->clone()},
			m_name{o.m_name}
		{}
	};
	
private:
	vector<Member> m_members;
	
public:
	Struct_template(vector<Member>&& members):
		m_members(std::move(members))
	{}
	
	Datatype_template_uptr clone() const override
	{
		return unique_ptr<Struct_template> {new Struct_template{vector<Member>(m_members)}};
	}
	
	Datatype_uptr evaluate(Context& ctx) const override
	{
		vector<Record_datatype::Member> evaluated_members;
		size_t displacement = 0;
		size_t struct_alignment = 0;
		for (auto&& member : m_members) {
			Datatype_uptr member_type {member.m_type->evaluate(ctx)};
			size_t alignment = member_type->alignment();
			// align the next member as requested
			displacement += (alignment - (displacement % alignment)) % alignment;
			evaluated_members.emplace_back(displacement, move(member_type), member.m_name);
			displacement += evaluated_members.back().type().buffersize();
			struct_alignment = max(struct_alignment, alignment);
		}
		//add padding at the end of struct
		displacement += (struct_alignment - (displacement % struct_alignment)) % struct_alignment;
		
		// ensure the record size is at least 1 to have a unique address
		displacement = max<size_t>(1, displacement);
		return unique_ptr<Record_datatype> {new Record_datatype{move(evaluated_members), displacement}};
	}
};

class Pointer_template:
	public Datatype_template
{
	Datatype_template_uptr m_subtype;
	
public:
	Pointer_template(Datatype_template_uptr subtype): m_subtype{std::move(subtype)} {}
	
	Datatype_template_uptr clone() const override
	{
		return unique_ptr<Pointer_template> {new Pointer_template{m_subtype->clone()}};
	}
	
	Datatype_uptr evaluate(Context& ctx) const override
	{
		return unique_ptr<Pointer_datatype> {new Pointer_datatype{m_subtype->evaluate(ctx)}};
	}
	
};


vector<Expression> get_array_property(PC_tree_t node, string property)
{
	vector<Expression> prop_vector;
	PC_tree_t config = PC_get(node, property.c_str());
	if (!PC_status(PC_get(config, "[0]")) ) {
		int rank = len(config);
		for (int i = 0; i < rank; i++) {
			prop_vector.emplace_back(to_string(PC_get(node, (property + string("[%d]")).c_str(), i)));
		}
	} else {
		string prop = to_string(config, "");
		if (prop.length() > 0) {
			prop_vector.emplace_back(prop);
		}
	}
	return prop_vector;
}

void validate_array(PC_tree_t node, vector<Expression>& size, vector<Expression>& subsize, vector<Expression>& start)
{
	if (size.empty()) {
		throw Config_error{node, "Array must have defined `size'"};
	}
	if (!start.empty() && subsize.empty()) {
		throw Config_error{node, "Array with a `start` property must have a defined `subsize'"};
		//TODO: handle by setting subsize to size-start
	}
	if (start.empty()) {
		start = vector<Expression>(size.size(), Expression{0l});
	}
	if (subsize.empty()) {
		subsize = size;
	}
	
	//check if rank of array is correct
	if (size.size() != subsize.size()) {
		throw Config_error{node, "`subsize' must have the same rank as `size': {} != {}", subsize.size(), size.size()};
	}
	if (size.size() != start.size()) {
		throw Config_error{node, "`start' must have the same rank as `size': {} != {}", start.size(), size.size()};
	}
}



Datatype_template_uptr to_array_datatype_template(Context& ctx, PC_tree_t node)
{
	{
		string order_str = to_string(PC_get(node, ".order"), "");
		if (order_str == "c" && order_str == "C") {
			ctx.logger()->warn("`order: C' for array is the only supported order and its specification is deprecated");
		} else if (order_str !="" ) {
			throw Config_error{node, "Incorrect array ordering: `{}', only C order is supported", order_str};
		}
	}
	
	vector<Expression> array_size = get_array_property(node, ".size");
	vector<Expression> array_subsize = get_array_property(node, ".subsize");
	vector<Expression> array_start = get_array_property(node, ".start");
	
	validate_array(node, array_size, array_subsize, array_start);
	
	PC_tree_t config_elem = PC_get(node, ".subtype");
	if (PC_status(config_elem)) {
		throw Config_error{node, "Array must have `subtype'"};
	}
	
	Datatype_template_uptr res_type = ctx.datatype(config_elem);
	
	for (ssize_t ii = array_size.size()-1; ii >=0; --ii) {
		res_type.reset(new Array_template(move(res_type), move(array_size[ii]), move(array_start[ii]), move(array_subsize[ii])));
	}
	return res_type;
}

vector<Record_template::Member> get_members(Context& ctx, PC_tree_t member_list_node)
{
	vector<Record_template::Member> members;
	
	int nb_members = len(member_list_node, 0);
	for (int member_id = 0; member_id < nb_members; member_id++) {
		//get current member name
		string member_name = to_string(PC_get(member_list_node, "{%d}", member_id));
		
		//get current member
		PC_tree_t member_node = PC_get(member_list_node, "<%d>", member_id);
		
		PC_tree_t disp_conf = PC_get(member_node, ".disp");
		if (PC_status(disp_conf)) {
			throw Config_error{member_node, "All members must have displacements"};
		}
		Expression disp = to_string(disp_conf);
		
		members.emplace_back(move(disp), ctx.datatype(member_node), move(member_name));
	}
	return members;
}

Datatype_template_uptr to_record_datatype_template(Context& ctx, PC_tree_t node)
{
	PC_tree_t buffersize_conf = PC_get(node, ".buffersize");
	if (PC_status(buffersize_conf)) {
		throw Config_error{node, "Record must have defined buffersize"};
	}
	Expression record_buffersize = to_string(buffersize_conf);
	
	PC_tree_t member_list_node = PC_get(node, ".members");
	
	return unique_ptr<Record_template> {new Record_template{get_members(ctx, member_list_node), move(record_buffersize)}};
}

Datatype_template_uptr to_struct_datatype_template(Context& ctx, PC_tree_t node)
{
	vector<Struct_template::Member> members;
	each_in_omap(PC_get(node, ".members"), [&](PC_tree_t member_name, PC_tree_t member_value_node){
			members.emplace_back(ctx.datatype(member_value_node), to_string(member_name));
	});
	return unique_ptr<Struct_template> {new Struct_template{std::move(members)}};
}

Datatype_template_uptr to_pointer_datatype_template(Context& ctx, PC_tree_t node)
{
	PC_tree_t subtype_conf = PC_get(node, ".subtype");
	if (PC_status(subtype_conf)) {
		throw Config_error{node, "Pointer must have defined subtype"};
	}
	return unique_ptr<Pointer_template> {new Pointer_template{ctx.datatype(subtype_conf)}};
}

} // namespace <anonymous>

Datatype_template::~Datatype_template()
{}

void Datatype_template::load_basic_datatypes(Context& ctx)
{
	// holder types
	ctx.add_datatype("array", to_array_datatype_template);
	ctx.add_datatype("struct", to_struct_datatype_template);
	ctx.add_datatype("record", to_record_datatype_template);
	ctx.add_datatype("pointer", to_pointer_datatype_template);
	
	// C basic types
	ctx.add_datatype("char", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::UNSIGNED, (long)sizeof(char)}};
	});
	ctx.add_datatype("int", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, (long)sizeof(int)}};
	});
	ctx.add_datatype("int8", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 1L}};
	});
	ctx.add_datatype("int16", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 2L}};
	});
	ctx.add_datatype("int32", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 4L}};
	});
	ctx.add_datatype("int64", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 8L}};
	});
	ctx.add_datatype("float", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::FLOAT, 4L}};
	});
	ctx.add_datatype("double", [](Context&, PC_tree_t) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::FLOAT, 8L}};
	});
	
	// Fortran basic types
#ifdef BUILD_FORTRAN
	ctx.add_datatype("character", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_CHARACTER_DEFAULT_KIND);
		if (kind == 0) kind = PDI_CHARACTER_DEFAULT_KIND;
		else if (kind < 0) throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	});
	ctx.add_datatype("integer", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_INTEGER_DEFAULT_KIND);
		if (kind == 0) kind = PDI_INTEGER_DEFAULT_KIND;
		else if (kind < 0) throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::SIGNED, kind}};
	});
	ctx.add_datatype("logical", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_LOGICAL_DEFAULT_KIND);
		if (kind == 0) kind = PDI_LOGICAL_DEFAULT_KIND;
		else if (kind < 0) throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	});
	ctx.add_datatype("real", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_REAL_DEFAULT_KIND);
		if (kind == 0) kind = PDI_REAL_DEFAULT_KIND;
		else if (kind < 0) throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::FLOAT, kind}};
	});
#endif // BUILD_FORTRAN
}

} // namespace PDI
