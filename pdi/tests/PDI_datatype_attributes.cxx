/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gtest/gtest.h>

#include <pdi/expression.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>

#include "global_context.h"

#include "PDI_record_datatype_cases.h"

using namespace PDI;
using namespace std;

/*
 * Name:                TypeAttrTest.simple_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, simple_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {attr_array: {type: array, subtype: int, size: 10, +attr: test } }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("attr_array"));
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test"));

	auto attr_map = result->attributes();
	ASSERT_EQ(attr_map.at("attr").to_string(global_ctx), string("test"));
};

/*
 * Name:                TypeAttrTest.inner_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, inner_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {inner_attr: {type: int, +attr: test_inner }, outer_attr: {type: array, subtype: inner_attr, size: 10, +attr: test_outer} }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("outer_attr"));
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_outer"));

	auto outer_attr_map = result->attributes();
	ASSERT_EQ(outer_attr_map.at("attr").to_string(global_ctx), string("test_outer"));

	Datatype_sptr evaluated = result->evaluate(global_ctx);
	auto&& array_type = static_pointer_cast<const Array_datatype>(evaluated);
	ASSERT_EQ(array_type->subtype()->attribute("attr").to_string(global_ctx), string("test_inner"));

	auto inner_attr_map = array_type->subtype()->attributes();
	ASSERT_EQ(inner_attr_map.at("attr").to_string(global_ctx), string("test_inner"));
};

/*
 * Name:                TypeAttrTest.member_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, member_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {member_type: {type: int, +attr: test_member}, record_type: {type: struct, members: [first_member: member_type, second_member: member_type], +attr: test_record}}")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("record_type"));
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_record"));

	auto record_attr_map = result->attributes();
	ASSERT_EQ(record_attr_map.at("attr").to_string(global_ctx), string("test_record"));

	auto&& record_type = static_pointer_cast<const Record_datatype>(result->evaluate(global_ctx));
	ASSERT_EQ(record_type->members()[0].type()->attribute("attr").to_string(global_ctx), string("test_member"));

	auto member_attr_map = record_type->members()[0].type()->attributes();
	ASSERT_EQ(member_attr_map.at("attr").to_string(global_ctx), string("test_member"));

	ASSERT_EQ(record_type->members()[1].type()->attribute("attr").to_string(global_ctx), string("test_member"));

	auto member2_attr_map = record_type->members()[1].type()->attributes();
	ASSERT_EQ(member2_attr_map.at("attr").to_string(global_ctx), string("test_member"));
};

/*
 * Name:                TypeAttrTest.ptr_inner_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, ptr_inner_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {inner_attr: {type: int, +attr: test_inner }, outer_attr: {type: pointer, subtype: inner_attr, +attr: test_outer} }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("outer_attr"));
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_outer"));

	auto outer_attr_map = result->attributes();
	ASSERT_EQ(outer_attr_map.at("attr").to_string(global_ctx), string("test_outer"));

	auto&& pointer_type = static_pointer_cast<const Pointer_datatype>(result->evaluate(global_ctx));
	ASSERT_EQ(pointer_type->subtype()->attribute("attr").to_string(global_ctx), string("test_inner"));

	auto inner_attr_map = pointer_type->subtype()->attributes();
	ASSERT_EQ(inner_attr_map.at("attr").to_string(global_ctx), string("test_inner"));
};


/*
 * Name:                TypeAttrTest.array_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, array_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {attr_array: {type: array, subtype: int, size: 10, +attr: [test0, test1] } }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("attr_array"));
	Ref_r attr_ref_0 = result->attribute("attr").to_ref(global_ctx).operator[](0UL);
	Ref_r attr_ref_1 = result->attribute("attr").to_ref(global_ctx).operator[](1UL);
	ASSERT_STREQ(static_cast<const char*>(attr_ref_0.get()), "test0");
	ASSERT_STREQ(static_cast<const char*>(attr_ref_1.get()), "test1");
};

/*
 * Name:                TypeAttrTest.map_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, map_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {attr_array: {type: array, subtype: int, size: 10, +attr: {key: value} } }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("attr_array"));
	Ref_r attr_ref_value = result->attribute("attr").to_ref(global_ctx).operator[]("key");
	ASSERT_STREQ(static_cast<const char*>(attr_ref_value.get()), "value");
};

/*
 * Name:                TypeAttrTest.array_in_map_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(TypeAttrTest, array_in_map_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {attr_array: {type: array, subtype: int, size: 10, +attr: {key: [value0, value1]} } }")};
	Datatype_template_ptr result = global_ctx.datatype(PC_parse_string("attr_array"));
	Ref_r map_attr_ref_value = result->attribute("attr").to_ref(global_ctx).operator[]("key");
	Ref_r attr_ref_0 = map_attr_ref_value.operator[](0UL);
	Ref_r attr_ref_1 = map_attr_ref_value.operator[](1UL);
	ASSERT_STREQ(static_cast<const char*>(attr_ref_0.get()), "value0");
	ASSERT_STREQ(static_cast<const char*>(attr_ref_1.get()), "value1");
};

/*
 * Name:                DataAttrTest.simple_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, simple_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {attr_int: {type: int, +attr: test}}")};
	Datatype_template_ptr result = global_ctx["attr_int"].default_type();
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test"));

	auto attr_map = result->attributes();
	ASSERT_EQ(attr_map.at("attr").to_string(global_ctx), string("test"));
};

/*
 * Name:                DataAttrTest.inner_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, inner_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("{types: {inner_attr: {type: int, +attr: test_inner }}, data: {outer_attr: {type: array, subtype: inner_attr, size: 10, +attr: test_outer}}}")};
	Datatype_template_ptr result = global_ctx["outer_attr"].default_type();
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_outer"));

	auto outer_attr_map = result->attributes();
	ASSERT_EQ(outer_attr_map.at("attr").to_string(global_ctx), string("test_outer"));

	auto&& array_type = dynamic_pointer_cast<const Array_datatype>(result->evaluate(global_ctx));
	ASSERT_EQ(array_type->subtype()->attribute("attr").to_string(global_ctx), string("test_inner"));

	auto inner_attr_map = array_type->subtype()->attributes();
	ASSERT_EQ(inner_attr_map.at("attr").to_string(global_ctx), string("test_inner"));
};

/*
 * Name:                DataAttrTest.member_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, member_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {record_data: {type: struct, members: [first_member: {type: int, +attr: test_member}, second_member: {type: int, +attr: test_member}], +attr: test_record}}")};
	Datatype_template_ptr result = global_ctx["record_data"].default_type();
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_record"));

	auto record_attr_map = result->attributes();
	ASSERT_EQ(record_attr_map.at("attr").to_string(global_ctx), string("test_record"));

	auto&& record_type = static_pointer_cast<const Record_datatype>(result->evaluate(global_ctx));
	ASSERT_EQ(record_type->members()[0].type()->attribute("attr").to_string(global_ctx), string("test_member"));

	auto member_attr_map = record_type->members()[0].type()->attributes();
	ASSERT_EQ(member_attr_map.at("attr").to_string(global_ctx), string("test_member"));

	ASSERT_EQ(record_type->members()[1].type()->attribute("attr").to_string(global_ctx), string("test_member"));

	auto member2_attr_map = record_type->members()[1].type()->attributes();
	ASSERT_EQ(member2_attr_map.at("attr").to_string(global_ctx), string("test_member"));
};

/*
 * Name:                DataAttrTest.ptr_inner_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, ptr_inner_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {outer_attr: {type: pointer, subtype: {type: int, +attr: test_inner}, +attr: test_outer} }")};
	Datatype_template_ptr result = global_ctx["outer_attr"].default_type();
	ASSERT_EQ(result->attribute("attr").to_string(global_ctx), string("test_outer"));

	auto outer_attr_map = result->attributes();
	ASSERT_EQ(outer_attr_map.at("attr").to_string(global_ctx), string("test_outer"));

	auto&& pointer_type = static_pointer_cast<const Pointer_datatype>(result->evaluate(global_ctx));
	ASSERT_EQ(pointer_type->subtype()->attribute("attr").to_string(global_ctx), string("test_inner"));

	auto inner_attr_map = pointer_type->subtype()->attributes();
	ASSERT_EQ(inner_attr_map.at("attr").to_string(global_ctx), string("test_inner"));
};


/*
 * Name:                DataAttrTest.array_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, array_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {attr_array: {type: array, subtype: int, size: 10, +attr: [test0, test1] } }")};
	Datatype_template_ptr result = global_ctx["attr_array"].default_type();
	Ref_r attr_ref_0 = result->attribute("attr").to_ref(global_ctx).operator[](0UL);
	Ref_r attr_ref_1 = result->attribute("attr").to_ref(global_ctx).operator[](1UL);
	ASSERT_STREQ(static_cast<const char*>(attr_ref_0.get()), "test0");
	ASSERT_STREQ(static_cast<const char*>(attr_ref_1.get()), "test1");
};

/*
 * Name:                DataAttrTest.map_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, map_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {attr_array: {type: array, subtype: int, size: 10, +attr: {key: value} } }")};
	Datatype_template_ptr result = global_ctx["attr_array"].default_type();
	Ref_r attr_ref_value = result->attribute("attr").to_ref(global_ctx).operator[]("key");
	ASSERT_STREQ(static_cast<const char*>(attr_ref_value.get()), "value");
};

/*
 * Name:                DataAttrTest.array_in_map_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataAttrTest, array_in_map_attr)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("data: {value: int, attr_array: {type: array, subtype: int, size: 10, +attr: {key: [$value, $value+1]} } }")};
	int value = 42;
	auto&& value_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	global_ctx["value"].share(Ref{(void*)&value, free, value_type, true, true}, false, false);
	Datatype_template_ptr result = global_ctx["attr_array"].default_type();
	Ref_r map_attr_ref_value = result->attribute("attr").to_ref(global_ctx).operator[]("key");
	Ref_r attr_ref_0 = map_attr_ref_value.operator[](0UL);
	Ref_r attr_ref_1 = map_attr_ref_value.operator[](1UL);
	ASSERT_EQ(attr_ref_0.scalar_value<int>(), 42);
	ASSERT_EQ(attr_ref_1.scalar_value<int>(), 43);
	global_ctx["value"].reclaim();
};
