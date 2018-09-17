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
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
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
	struct Member {
	
		/// Offset or distance in byte from the Record_template start
		Expression m_displacement;
		
		/// Type of the contained member
		Datatype_template_uptr m_type;
		
		string m_name;
		
		
		Member(const Member& o): m_displacement{o.m_displacement}, m_type{o.m_type->clone()}, m_name{o.m_name} {}
		
	};
	
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


Datatype_template_uptr to_scalar_datatype_template(Context& ctx, PC_tree_t node)
{
	string type;
	try {
		type = to_string(PC_get(node, ".type"));
	} catch (Error e) {
		type = to_string(node);
	}
	return ctx.datatype(std::move(type))(node);
}


Datatype_template_uptr to_array_datatype_template(Context& ctx, PC_tree_t node)
{
	{
		string order_str = to_string(PC_get(node, ".order"), "");
		if (order_str == "c" && order_str == "C") {
			ctx.logger()->warn("`order: C' for array is the only supported order and its specification is deprecated");
		} else if (order_str !="" ) {
			throw Error{PDI_ERR_CONFIG, "Incorrect array ordering: `%s', only C order is supported", order_str.c_str()};
		}
	}
	
	vector<Expression> sizes;
	PC_tree_t conf_sizes = PC_get(node, ".sizes");
	if (!PC_status(conf_sizes)) {   // multi dim array
		int nsizes = len(conf_sizes);
		for (int ii = 0; ii < nsizes; ++ii) {
			sizes.emplace_back(to_string(PC_get(node, ".sizes[%d]", ii)));
		}
	} else { // else we expect a single dim array
		sizes.emplace_back(to_string(PC_get(node, ".size")));
	}
	
	vector<Expression> subsizes;
	PC_tree_t conf_subsizes = PC_get(node, ".subsizes");
	if (!PC_status(conf_subsizes)) {
		int nsubsizes = len(conf_subsizes);
		if (nsubsizes != static_cast<int>(sizes.size())) {
			throw Error{PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", nsubsizes, sizes.size()};
		}
		for (int ii = 0; ii < nsubsizes; ++ii) {
			subsizes.emplace_back(to_string(PC_get(node, ".subsizes[%d]", ii)));
		}
	} else {
		PC_tree_t conf_subsize = PC_get(node, ".subsize");
		if (!PC_status(conf_subsize)) {
			if (sizes.size() != 1) {
				throw Error{PDI_ERR_CONFIG, "Invalid single subsize for %dD array", sizes.size()};
			}
			subsizes.emplace_back(to_string(conf_subsize));
		} else {
			subsizes = sizes;
		}
	}
	
	vector<Expression> starts;
	PC_tree_t conf_starts = PC_get(node, ".starts");
	if (!PC_status(conf_starts)) {
		int nstarts = len(conf_starts);
		if (nstarts != static_cast<int>(sizes.size())) {
			throw Error{PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", nstarts, sizes.size()};
		}
		for (int ii = 0; ii < nstarts; ++ii) {
			starts.emplace_back(to_string(PC_get(node, ".starts[%d]", ii)));
		}
	} else {
		PC_tree_t conf_start = PC_get(node, ".start");
		if (!PC_status(conf_start)) {
			if (sizes.size() != 1) {
				throw Error{PDI_ERR_CONFIG, "Invalid single start for %dD array", sizes.size()};
			}
			starts.emplace_back(to_string(conf_start));
		} else {
			starts = vector<Expression>(sizes.size(), Expression{0l});
		}
	}
	
	Datatype_template_uptr res_type = Datatype_template::load(ctx, PC_get(node, ".type"));
	
	for (ssize_t ii = sizes.size()-1; ii >=0; --ii) {
		res_type.reset(new Array_template(move(res_type), move(sizes[ii]), move(starts[ii]), move(subsizes[ii])));
	}
	return res_type;
}

} // namespace <anonymous>

Datatype_template::~Datatype_template()
{}

Datatype_template_uptr Datatype_template::load(Context& ctx, PC_tree_t node)
{
	// size or sizes => array
	if (!PC_status(PC_get(node, ".size")) || !PC_status(PC_get(node, ".sizes"))) {
		return to_array_datatype_template(ctx, node);
	}
	return to_scalar_datatype_template(ctx, node);
}

void Datatype_template::load_basic_datatypes(Context& ctx)
{
	// C basic types
	ctx.add_datatype("char", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::UNSIGNED, sizeof(char)}};
	});
	ctx.add_datatype("int", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, sizeof(int)}};
	});
	ctx.add_datatype("int8", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 1}};
	});
	ctx.add_datatype("int16", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 2}};
	});
	ctx.add_datatype("int32", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 4}};
	});
	ctx.add_datatype("int64", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::SIGNED, 8}};
	});
	ctx.add_datatype("float", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::FLOAT, 4}};
	});
	ctx.add_datatype("double", [](const PC_tree_t&) {
		return Datatype_template_uptr {new Scalar_template{Scalar_kind::FLOAT, 8}};
	});
	
	// Fortran basic types
	ctx.add_datatype("character", [](const PC_tree_t& tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_CHARACTER_DEFAULT_KIND);
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	});
	ctx.add_datatype("integer", [](const PC_tree_t& tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_INTEGER_DEFAULT_KIND);
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::SIGNED, kind}};
	});
	ctx.add_datatype("logical", [](const PC_tree_t& tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_LOGICAL_DEFAULT_KIND);
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::UNSIGNED, kind}};
	});
	ctx.add_datatype("real", [](const PC_tree_t& tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_REAL_DEFAULT_KIND);
		return Datatype_template_uptr{new Scalar_template{Scalar_kind::FLOAT, std::move(kind)}};
	});
	
}

} // namespace PDI
