/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the names of CEA, nor the names of the contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/


/// \file
/// The datatype examples of \ref Specification_tree_ref.
/// They reach the datatypes %PDI builds, which is beyond the public C API, so
/// they are plain GoogleTest unit tests over PDI::Global_context, like the
/// other datatype tests of the core library.

#include <cstdint>
#include <string>

#include <gtest/gtest.h>

#include <pdi/data_descriptor.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "global_context.h"

using namespace PDI;

/*
 * Name:                DocDatatypeTest.attributes
 *
 * Tested functions:    PDI::Datatype::attribute
 *
 * Description:         Checks that the attributes the reference documents are
 *                      really created on the datatype it describes.
 */
TEST(DocDatatypeTest, attributes)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {}")};
	Datatype_template_sptr result = global_ctx.datatype(PC_parse_string(
		R"PDIYAML(
#! [attributes]
type: int
+first_attr: attr_value_1
+second_attr: [attr, value]
+third_attr: {key_0: 0, key_1: 1}
#! [attributes]
)PDIYAML"
	));

	ASSERT_EQ(std::string("attr_value_1"), result->attribute("first_attr").to_string(global_ctx));
	ASSERT_EQ(3, result->attributes().size());
}

/*
 * Name:                DocDatatypeTest.scalar_shortcut
 *
 * Tested functions:    PDI::Context::datatype
 *
 * Description:         The reference states that a scalar naming a type is a
 *                      shortcut for a mapping with a single `type' key; checks
 *                      that both really yield the same datatype.
 */
TEST(DocDatatypeTest, scalar_shortcut)
{
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {my_type: int}")};

	Datatype_sptr shortcut
		= global_ctx
	          .datatype(PC_parse_string(
				  R"PDIYAML(
#! [type_shortcut]
"my_type"
#! [type_shortcut]
)PDIYAML"
			  ))
	          ->evaluate(global_ctx);
	Datatype_sptr expanded
		= global_ctx
	          .datatype(PC_parse_string(
				  R"PDIYAML(
#! [type_expanded]
{ type: "my_type" }
#! [type_expanded]
)PDIYAML"
			  ))
	          ->evaluate(global_ctx);

	ASSERT_EQ(*shortcut, *expanded);
}

/*
 * Name:                DocDatatypeTest.struct_layout
 *
 * Tested functions:    PDI::Datatype::buffersize, PDI::Datatype::member
 *
 * Description:         The reference shows a `struct' type beside the C struct
 *                      it "matches"; checks that it really describes that
 *                      struct, both in memory layout and when its members are
 *                      read back through a reference to a real instance.
 */
/// The C struct that \ref struct_type_node says the type describes.
//! [struct_c]
struct Data {
	int32_t first_int;
	int32_t second_int;
};

//! [struct_c]

TEST(DocDatatypeTest, struct_layout)
{
	Data data;

	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("types: {}")};
	Datatype_sptr type = global_ctx
	                         .datatype(PC_parse_string(R"PDIYAML(
type: struct
members:
  - first_int: int32
  - second_int: int32
)PDIYAML"))
	                         ->evaluate(global_ctx);

	// the type describes the very same memory layout as the C struct
	ASSERT_EQ(sizeof(data), type->buffersize());
	ASSERT_EQ(offsetof(Data, first_int), reinterpret_cast<char*>(type->member("first_int", &data).first) - reinterpret_cast<char*>(&data));
	ASSERT_EQ(offsetof(Data, second_int), reinterpret_cast<char*>(type->member("second_int", &data).first) - reinterpret_cast<char*>(&data));

	// and the members of a shared instance are really reachable through it
	data.first_int = 42;
	data.second_int = 51;
	Ref_r ref{&data, [](void*) {}, type, true, false};
	ASSERT_EQ(42, *static_cast<const int32_t*>(Ref_r{ref["first_int"]}.get()));
	ASSERT_EQ(51, *static_cast<const int32_t*>(Ref_r{ref["second_int"]}.get()));
}

/*
 * Name:                DocDatatypeTest.data_map
 *
 * Tested functions:    PDI::Context::desc
 *
 * Description:         The reference shows what the entries of a `data' mapping
 *                      look like; checks that this very example still declares
 *                      the two data it describes.
 */
TEST(DocDatatypeTest, data_map)
{
	PDI::Paraconf_wrapper fw;
	// the reference shows the entries of the mapping, so they are indented into
	// the `data' key they belong to
	Global_context global_ctx{PC_parse_string(R"PDIYAML(
data:
  #! [data_map]
  my_data_1: int
  my_data_2: {type: array, subtype: double, size: 5}
  #! [data_map]
)PDIYAML")};

	// both entries really declare the datatype the reference gives them
	Datatype_sptr scalar = global_ctx.desc("my_data_1").default_type()->evaluate(global_ctx);
	ASSERT_EQ(sizeof(int), scalar->buffersize());

	Datatype_sptr array = global_ctx.desc("my_data_2").default_type()->evaluate(global_ctx);
	ASSERT_EQ(5 * sizeof(double), array->buffersize());
}
