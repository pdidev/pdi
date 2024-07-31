/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "pdi.h"
#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"
#include "pdi/tuple_datatype.h"

#include "pdi/datatype_template.h"

namespace PDI {

using std::exception;
using std::make_shared;
using std::max;
using std::move;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

namespace {

class Scalar_template: public Datatype_template
{
	/// Interpretation of the content
	Scalar_kind m_kind;

	/// Size of the content in bytes or 0 if unknown
	Expression m_size;

	/// Size of the alignment in bytes
	Expression m_align;

public:
	Scalar_template(Scalar_kind kind, const Expression& size, const Expression& align, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_kind{kind}
	    , m_size{size}
	    , m_align{align}
	{}

	Scalar_template(Scalar_kind kind, const Expression& size, PC_tree_t datatype_tree)
	    : Scalar_template(kind, size, size, datatype_tree)
	{}

	Scalar_template(Scalar_kind kind, const Expression& size, const Expression& align, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_kind{kind}
	    , m_size{size}
	    , m_align{align}
	{}

	Scalar_template(Scalar_kind kind, const Expression& size, const Attributes_map& attributes = {})
	    : Scalar_template(kind, size, size, attributes)
	{}

	Datatype_sptr evaluate(Context& ctx) const override
	{
		return Scalar_datatype::make(m_kind, static_cast<size_t>(m_size.to_long(ctx)), static_cast<size_t>(m_align.to_long(ctx)), m_attributes);
	}
};

class Array_template: public Datatype_template
{
	/// Type of the elements contained in the array.
	Datatype_template_sptr m_subtype;

	/// Number of elements the array can store
	Expression m_size;

	/// id of the first actual element of the array
	Expression m_start;

	/// Number of actual elements in the array
	Expression m_subsize;

public:
	Array_template(Datatype_template_sptr subtype, Expression size, Expression start, Expression subsize, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_subtype{move(subtype)}
	    , m_size{move(size)}
	    , m_start{move(start)}
	    , m_subsize{move(subsize)}
	{}

	Array_template(Datatype_template_sptr subtype, Expression size, Expression start, Expression subsize, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_subtype{move(subtype)}
	    , m_size{move(size)}
	    , m_start{move(start)}
	    , m_subsize{move(subsize)}
	{}

	Datatype_sptr evaluate(Context& ctx) const override
	{
		return Array_datatype::make(
		    m_subtype->evaluate(ctx),
		    static_cast<size_t>(m_size.to_long(ctx)),
		    static_cast<size_t>(m_start.to_long(ctx)),
		    static_cast<size_t>(m_subsize.to_long(ctx)),
		    m_attributes
		);
	}
};

class Record_template: public Datatype_template
{
public:
	struct Member {
		/// Offset or distance in byte from the Record_template start
		Expression m_displacement;

		/// Type of the contained member
		Datatype_template_sptr m_type;

		string m_name;

		Member(Expression disp, Datatype_template_sptr type, string name)
		    : m_displacement{move(disp)}
		    , m_type{move(type)}
		    , m_name{move(name)}
		{}

		Member(const Member& o)
		    : m_displacement{o.m_displacement}
		    , m_type{o.m_type}
		    , m_name{o.m_name}
		{}
	};

private:
	/// All members in increasing displacement order
	vector<Member> m_members;

	/// The total size of the buffer containing all members
	Expression m_buffersize;

public:
	Record_template(vector<Member>&& members, Expression&& size, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_members{move(members)}
	    , m_buffersize{move(size)}
	{}

	Record_template(vector<Member>&& members, Expression&& size, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_members{move(members)}
	    , m_buffersize{move(size)}
	{}

	Datatype_sptr evaluate(Context& ctx) const override
	{
		vector<Record_datatype::Member> evaluated_members;
		for (auto&& member: m_members) {
			evaluated_members.emplace_back(member.m_displacement.to_long(ctx), member.m_type->evaluate(ctx), member.m_name);
		}
		return Record_datatype::make(move(evaluated_members), static_cast<size_t>(m_buffersize.to_long(ctx)), m_attributes);
	}
};

class Struct_template: public Datatype_template
{
public:
	struct Member {
		/// Type of the contained member
		Datatype_template_sptr m_type;
		string m_name;

		Member(Datatype_template_sptr type, string name)
		    : m_type{move(type)}
		    , m_name{move(name)}
		{}

		Member(const Member& o)
		    : m_type{o.m_type}
		    , m_name{o.m_name}
		{}
	};

private:
	vector<Member> m_members;

public:
	Struct_template(vector<Member>&& members, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_members(std::move(members))
	{}

	Struct_template(vector<Member>&& members, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_members(std::move(members))
	{}

	Datatype_sptr evaluate(Context& ctx) const override
	{
		vector<Record_datatype::Member> evaluated_members;
		size_t displacement = 0;
		size_t struct_alignment = 0;
		for (auto&& member: m_members) {
			Datatype_sptr member_type{member.m_type->evaluate(ctx)};
			size_t alignment = member_type->alignment();
			// align the next member as requested
			displacement += (alignment - (displacement % alignment)) % alignment;
			evaluated_members.emplace_back(displacement, move(member_type), member.m_name);
			displacement += evaluated_members.back().type()->buffersize();
			struct_alignment = max(struct_alignment, alignment);
		}
		//add padding at the end of struct
		displacement += (struct_alignment - (displacement % struct_alignment)) % struct_alignment;

		// ensure the record size is at least 1 to have a unique address
		displacement = max<size_t>(1, displacement);
		return Record_datatype::make(move(evaluated_members), displacement, m_attributes);
	}
};

class Pointer_template: public Datatype_template
{
	Datatype_template_sptr m_subtype;

public:
	Pointer_template(Datatype_template_sptr subtype, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_subtype{std::move(subtype)}
	{}

	Pointer_template(Datatype_template_sptr subtype, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_subtype{std::move(subtype)}
	{}

	Datatype_sptr evaluate(Context& ctx) const override { return Pointer_datatype::make(m_subtype->evaluate(ctx), m_attributes); }
};

class Tuple_template: public Datatype_template
{
public:
	struct Element {
		/// Offset or distance in byte from the Tuple_template start
		Expression m_displacement;

		/// Type of the contained member
		Datatype_template_sptr m_type;

		/** Creates new Element template with only type defined
		 * 
		 * \param[in] type type of the element
		 */
		Element(Datatype_template_sptr type)
		    : m_type{move(type)}
		{}

		/** Creates new Element template with only type defined
		 * 
		 * \param[in] disp displacement of the element
		 * \param[in] type type of the element
		 */
		Element(Expression disp, Datatype_template_sptr type)
		    : m_displacement{move(disp)}
		    , m_type{move(type)}
		{}

		/** Creates a copy of an element template
		 * 
		 * \param[in] o an element to copy
		 */
		Element(const Element& o)
		    : m_displacement{o.m_displacement}
		    , m_type{o.m_type}
		{}
	};

private:
	/// All elements in increasing displacement order
	vector<Element> m_elements;

	/// The total size of the buffer containing all elements
	Expression m_buffersize;

public:
	Tuple_template(vector<Element>&& elements, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_elements{move(elements)}
	{}

	Tuple_template(vector<Element>&& elements, Expression&& size, PC_tree_t datatype_tree)
	    : Datatype_template(datatype_tree)
	    , m_elements{move(elements)}
	    , m_buffersize{move(size)}
	{}

	Tuple_template(vector<Element>&& elements, Expression&& size, const Attributes_map& attributes = {})
	    : Datatype_template(attributes)
	    , m_elements{move(elements)}
	    , m_buffersize{move(size)}
	{}

	Datatype_sptr evaluate(Context& ctx) const override
	{
		size_t tuple_buffersize;
		vector<Tuple_datatype::Element> evaluated_elements;
		if (m_elements[0].m_displacement) {
			// if one element has displacement, all have (and buffersize is defined)
			tuple_buffersize = static_cast<size_t>(m_buffersize.to_long(ctx));
			for (auto&& element: m_elements) {
				evaluated_elements.emplace_back(element.m_displacement.to_long(ctx), element.m_type->evaluate(ctx));
			}
		} else {
			// if one element doesn't have displacement, no element has (and buffersize is not defined)
			size_t displacement = 0;
			size_t tuple_alignment = 0;
			for (auto&& element: m_elements) {
				Datatype_sptr element_type{element.m_type->evaluate(ctx)};
				size_t alignment = element_type->alignment();
				// align the next element as requested
				displacement += (alignment - (displacement % alignment)) % alignment;
				evaluated_elements.emplace_back(displacement, move(element_type));
				displacement += evaluated_elements.back().type()->buffersize();
				tuple_alignment = max(tuple_alignment, alignment);
			}
			//add padding at the end of tuple
			displacement += (tuple_alignment - (displacement % tuple_alignment)) % tuple_alignment;

			// ensure the tuple size is at least 1 to have a unique address
			displacement = max<size_t>(1, displacement);
			tuple_buffersize = displacement;
		}


		return Tuple_datatype::make(move(evaluated_elements), tuple_buffersize, m_attributes);
	}
};

vector<Expression> get_array_property(PC_tree_t node, string property)
{
	vector<Expression> prop_vector;
	PC_tree_t config = PC_get(node, property.c_str());
	if (!PC_status(PC_get(config, "[0]"))) {
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

Datatype_template_sptr to_array_datatype_template(Context& ctx, PC_tree_t node)
{
	{
		string order_str = to_string(PC_get(node, ".order"), "");
		if (order_str == "c" && order_str == "C") {
			ctx.logger().warn("`order: C' for array is the only supported order and its specification is deprecated");
		} else if (order_str != "") {
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

	Datatype_template_sptr res_type = ctx.datatype(config_elem);

	for (ssize_t ii = array_size.size() - 1; ii >= 0; --ii) {
		res_type.reset(new Array_template(move(res_type), move(array_size[ii]), move(array_start[ii]), move(array_subsize[ii]), node));
	}
	return res_type;
}

vector<Tuple_template::Element> get_tuple_elements(Context& ctx, PC_tree_t elements_node, bool buffersize_defined)
{
	int displacement_counter = 0;
	vector<Tuple_template::Element> result;
	int nb_elements = len(elements_node, 0);
	if (is_list(elements_node)) {
		for (int element_id = 0; element_id < nb_elements; element_id++) {
			// get element type
			PC_tree_t element_node = PC_get(elements_node, "[%d]", element_id);

			// get element displacement
			PC_tree_t disp_conf = PC_get(element_node, ".disp");
			Expression disp;
			if (!PC_status(disp_conf)) {
				disp = to_string(disp_conf);
				displacement_counter++;
			}
			result.emplace_back(disp, ctx.datatype(element_node));
		}
	} else {
		throw Config_error{elements_node, "Tuple elements subtree must be a seqence or ordered mapping"};
	}

	// check if non or all of elements have diplacement defined
	if (displacement_counter != 0 && displacement_counter != nb_elements) {
		throw Config_error{elements_node, "None or all of tuple elements must to have `disp' defined"};
	}

	// buffersize defined, but no displacement in elements
	if (buffersize_defined && displacement_counter == 0) {
		throw Config_error{elements_node, "If tuple buffersize is defined, all `disp' must be defined also"};
	}

	// buffersize not defined, but displacement is defined in elements
	if (!buffersize_defined && displacement_counter != 0) {
		throw Config_error{elements_node, "If tuple buffersize is not defined, `disp' cannot be defined also"};
	}

	return result;
}

Datatype_template_sptr to_tuple_datatype_template(Context& ctx, PC_tree_t node)
{
	PC_tree_t buffersize_conf = PC_get(node, ".buffersize");
	Expression tuple_buffersize;
	if (!PC_status(buffersize_conf)) {
		tuple_buffersize = to_string(buffersize_conf);
	}

	PC_tree_t elements_node = PC_get(node, ".elements");
	if (PC_status(elements_node)) {
		throw Config_error{node, "Tuple datatype must have `elements' subtree"};
	}
	bool tuple_buffersize_defined = static_cast<bool>(tuple_buffersize);
	return unique_ptr<Tuple_template>{
	    new Tuple_template{get_tuple_elements(ctx, elements_node, tuple_buffersize_defined), move(tuple_buffersize), node}};
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

Datatype_template_sptr to_record_datatype_template(Context& ctx, PC_tree_t node)
{
	PC_tree_t buffersize_conf = PC_get(node, ".buffersize");
	if (PC_status(buffersize_conf)) {
		throw Config_error{node, "Record must have defined buffersize"};
	}
	Expression record_buffersize = to_string(buffersize_conf);

	PC_tree_t member_list_node = PC_get(node, ".members");

	return unique_ptr<Record_template>{new Record_template{get_members(ctx, member_list_node), move(record_buffersize), node}};
}

Datatype_template_sptr to_struct_datatype_template(Context& ctx, PC_tree_t node)
{
	vector<Struct_template::Member> members;
	each_in_omap(PC_get(node, ".members"), [&](PC_tree_t member_name, PC_tree_t member_value_node) {
		members.emplace_back(ctx.datatype(member_value_node), to_string(member_name));
	});
	return unique_ptr<Struct_template>{new Struct_template{std::move(members), node}};
}

Datatype_template_sptr to_pointer_datatype_template(Context& ctx, PC_tree_t node)
{
	PC_tree_t subtype_conf = PC_get(node, ".subtype");
	if (PC_status(subtype_conf)) {
		throw Config_error{node, "Pointer must have defined subtype"};
	}
	return unique_ptr<Pointer_template>{new Pointer_template{ctx.datatype(subtype_conf), node}};
}

} // namespace

Datatype_template::Datatype_template(const Attributes_map& attributes)
    : m_attributes{attributes}
{}

Datatype_template::Datatype_template(PC_tree_t datatype_tree)
{
	if (!PC_status(datatype_tree) && !is_scalar(datatype_tree)) {
		int len;
		PC_len(datatype_tree, &len);
		for (int i = 0; i < len; i++) {
			string key = to_string(PC_get(datatype_tree, "{%d}", i));
			if (key[0] == '+') {
				// is an attribute
				m_attributes[key.substr(1)] = PC_get(datatype_tree, "<%d>", i);
			}
		}
	}
}

Expression Datatype_template::attribute(const std::string& attribute_name) const
{
	try {
		return m_attributes.at(attribute_name);
	} catch (const exception& e) {
		throw Type_error{"`{}' type attribute not found", attribute_name};
	}
}

const Attributes_map& Datatype_template::attributes() const
{
	return m_attributes;
}

Datatype_template::~Datatype_template() {}

void add_scalar_datatype(Context& ctx, const string& name, Scalar_kind kind, size_t size)
{
	ctx.add_datatype(name, [kind, size](Context&, PC_tree_t tree) {
		return Datatype_template_sptr{new Scalar_template{kind, static_cast<long>(size), tree}};
	});
}

template <class S, S s>
void add_scalar_datatype_T(Context& ctx, const string& name, size_t size)
{
	ctx.add_datatype(name, [&, size](Context&, PC_tree_t tree) {
		return Datatype_template_sptr{new Scalar_template{s, static_cast<long>(size), tree}};
	});
}

inline void add_unsigned(Context& ctx, const string& name, size_t size)
{
	add_scalar_datatype_T<Scalar_kind, Scalar_kind::UNSIGNED>(ctx, name, size);
}

inline void add_signed(Context& ctx, const string& name, size_t size)
{
	add_scalar_datatype_T<Scalar_kind, Scalar_kind::SIGNED>(ctx, name, size);
}

inline void add_float(Context& ctx, const string& name, size_t size)
{
	add_scalar_datatype_T<Scalar_kind, Scalar_kind::FLOAT>(ctx, name, size);
}

void Datatype_template::load_basic_datatypes(Context& ctx)
{
	// holder types
	ctx.add_datatype("array", to_array_datatype_template);
	ctx.add_datatype("struct", to_struct_datatype_template);
	ctx.add_datatype("record", to_record_datatype_template);
	ctx.add_datatype("pointer", to_pointer_datatype_template);
	ctx.add_datatype("tuple", to_tuple_datatype_template);

	// C basic types

	static const std::unordered_map<string, size_t> signed_types = {
	    {"int", sizeof(int)},
	    {"short", sizeof(short)},
	    {"long", sizeof(long)},
	    {"long long", sizeof(long long)},
	    {"intmax", sizeof(intmax_t)},
	    {"intmax_t", sizeof(intmax_t)},
	    {"int8", 1L},
	    {"int16", 2L},
	    {"int32", 4L},
	    {"int64", 8L},
	    {"int8_t", 1L},
	    {"int16_t", 2L},
	    {"int32_t", 4L},
	    {"int64_t", 8L},
	    {"int_least8", sizeof(int_least8_t)},
	    {"int_least16", sizeof(int_least16_t)},
	    {"int_least32", sizeof(int_least32_t)},
	    {"int_least64", sizeof(int_least64_t)},
	    {"int_least8_t", sizeof(int_least8_t)},
	    {"int_least16_t", sizeof(int_least16_t)},
	    {"int_least32_t", sizeof(int_least32_t)},
	    {"int_least64_t", sizeof(int_least64_t)},
	    {"int_fast8", sizeof(int_fast8_t)},
	    {"int_fast16", sizeof(int_fast16_t)},
	    {"int_fast32", sizeof(int_fast32_t)},
	    {"int_fast64", sizeof(int_fast64_t)},
	    {"int_fast8_t", sizeof(int_fast8_t)},
	    {"int_fast16_t", sizeof(int_fast16_t)},
	    {"int_fast32_t", sizeof(int_fast32_t)},
	    {"int_fast64_t", sizeof(int_fast64_t)},
	    {"intptr", sizeof(intptr_t)},
	    {"intptr_t", sizeof(intptr_t)},
	    {"ptrdiff", sizeof(ptrdiff_t)},
	    {"ptrdiff_t", sizeof(ptrdiff_t)}};

	for (auto& pair: signed_types) {
		add_signed(ctx, pair.first, pair.second);
	}

	static const std::unordered_map<string, size_t> unsigned_types = {
	    {"char", sizeof(char)},
	    {"unsigned short", sizeof(unsigned short)},
	    {"unsigned long", sizeof(unsigned long)},
	    {"unsigned long long", sizeof(unsigned long long)},
	    {"uintmax", sizeof(uintmax_t)},
	    {"uintmax_t", sizeof(uintmax_t)},
	    {"uint8", 1L},
	    {"uint16", 2L},
	    {"uint32", 4L},
	    {"uint64", 8L},
	    {"uint8_t", 1L},
	    {"uint16_t", 2L},
	    {"uint32_t", 4L},
	    {"uint64_t", 8L},
	    {"uint_least8", sizeof(uint_least8_t)},
	    {"uint_least16", sizeof(uint_least16_t)},
	    {"uint_least32", sizeof(uint_least32_t)},
	    {"uint_least64", sizeof(uint_least64_t)},
	    {"uint_least8_t", sizeof(uint_least8_t)},
	    {"uint_least16_t", sizeof(uint_least16_t)},
	    {"uint_least32_t", sizeof(uint_least32_t)},
	    {"uint_least64_t", sizeof(uint_least64_t)},
	    {"uint_fast8", sizeof(uint_fast8_t)},
	    {"uint_fast16", sizeof(uint_fast16_t)},
	    {"uint_fast32", sizeof(uint_fast32_t)},
	    {"uint_fast64", sizeof(uint_fast64_t)},
	    {"uint_fast8_t", sizeof(uint_fast8_t)},
	    {"uint_fast16_t", sizeof(uint_fast16_t)},
	    {"uint_fast32_t", sizeof(uint_fast32_t)},
	    {"uint_fast64_t", sizeof(uint_fast64_t)},
	    {"uintptr", sizeof(uintptr_t)},
	    {"uintptr_t", sizeof(uintptr_t)},
	    {"size_t", sizeof(size_t)}};

	for (auto& pair: unsigned_types) {
		add_unsigned(ctx, pair.first, pair.second);
	}

	add_float(ctx, "float", 4L);
	add_float(ctx, "double", 8L);
	add_scalar_datatype(ctx, "byte", Scalar_kind::UNKNOWN, 1L);

	// Fortran basic types
#ifdef BUILD_FORTRAN
	ctx.add_datatype("character", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_CHARACTER_DEFAULT_KIND);
		if (kind == 0)
			kind = PDI_CHARACTER_DEFAULT_KIND;
		else if (kind < 0)
			throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_sptr{new Scalar_template{Scalar_kind::UNSIGNED, kind, tree}};
	});
	ctx.add_datatype("integer", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_INTEGER_DEFAULT_KIND);
		if (kind == 0)
			kind = PDI_INTEGER_DEFAULT_KIND;
		else if (kind < 0)
			throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_sptr{new Scalar_template{Scalar_kind::SIGNED, kind, tree}};
	});
	ctx.add_datatype("logical", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_LOGICAL_DEFAULT_KIND);
		if (kind == 0)
			kind = PDI_LOGICAL_DEFAULT_KIND;
		else if (kind < 0)
			throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_sptr{new Scalar_template{Scalar_kind::UNSIGNED, kind, tree}};
	});
	ctx.add_datatype("real", [](Context&, PC_tree_t tree) {
		long kind = to_long(PC_get(tree, ".kind"), PDI_REAL_DEFAULT_KIND);
		if (kind == 0)
			kind = PDI_REAL_DEFAULT_KIND;
		else if (kind < 0)
			throw Config_error{PC_get(tree, ".kind"), "`kind' of the datatype cannot be less than 0"};
		return Datatype_template_sptr{new Scalar_template{Scalar_kind::FLOAT, kind, tree}};
	});
#endif // BUILD_FORTRAN
}

void Datatype_template::load_user_datatypes(Context& ctx, PC_tree_t types_tree)
{
	if (!PC_status(types_tree)) {
		int types_len;
		PC_len(types_tree, &types_len);
		for (int i = 0; i < types_len; i++) {
			ctx.add_datatype(to_string(PC_get(types_tree, "{%d}", i)), [datatype_tree = PC_get(types_tree, "<%d>", i)](Context& ctx, PC_tree_t tree) {
				return ctx.datatype(datatype_tree);
			});
		}
	}
}

} // namespace PDI
