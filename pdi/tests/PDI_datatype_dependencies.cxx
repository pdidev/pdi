/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <iostream>
#include <gtest/gtest.h>

#include <pdi/array_datatype.h>
#include <pdi/expression.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include "global_context.h"


using namespace PDI;

// function to check the list of dependencies
void check_dependecies(const std::unordered_set<std::string>& expected_dependencies, const std::unordered_set<std::string>& result)
{
	// check the size
	EXPECT_EQ(expected_dependencies.size(), result.size()) << "number of dependencies is not correct";

	// check each data name is in the set
	for (auto&& expected_elem: expected_dependencies) {
		EXPECT_TRUE(result.find(expected_elem) != result.end()) << "The dependencies on data " << expected_elem << " is not found";
	}
}

/*
 * Name:                DataAttrTest.simple_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataDependenciesTest, ArrayDataTypeNoDependencies)
{
	PC_tree_t tree = PC_parse_string(R"==(
metadata: {array_size: int}
data: {inner_array: {type: array, subtype: double, size: '10'}}
)==");
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{tree};
	std::unordered_set<std::string> result;
	Datatype_template_sptr data_template = global_ctx["inner_array"].default_type();
	data_template->get_dependencies(global_ctx, result);

	std::unordered_set<std::string> expected_dependencies = {""};

	// check there is no dependencies
	ASSERT_EQ(0, result.size()) << "No dependencies case";
}

/*
 * Name:                DataAttrTest.simple_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataDependenciesTest, ArrayDataType)
{
	PC_tree_t tree = PC_parse_string(R"==(
metadata: {array_size: int}
data: {inner_array: {type: array, subtype: double, size: $array_size}}
)==");
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{tree};
	std::unordered_set<std::string> result;
	Datatype_template_sptr data_template = global_ctx["inner_array"].default_type();
	data_template->get_dependencies(global_ctx, result);


	std::unordered_set<std::string> expected_dependencies = {"array_size"};

	check_dependecies(expected_dependencies, result);
}

/*
 * Name:                DataAttrTest.simple_attr
 *
 * Tested functions:    PDI::Datatype_template::load_basic_datatypes
 *
 * Description:         Test checks if correct type attribute is returned
 *
 */
TEST(DataDependenciesTest, ArrayOfArrayDataType)
{
	PC_tree_t tree = PC_parse_string(R"==(
types: {inner_attr: {type: array, subtype: int, size: $type_size}}
metadata: {dim_size: int, array_size: {type: array, subtype: int, size: $dim_size}}
data: {inner_array: {type: array, subtype: inner_attr, size: [ "$array_size[0]", "$array_size[1]", "$array_size[2]"]}}
)==");
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{tree};
	std::unordered_set<std::string> result;
	Datatype_template_sptr data_template = global_ctx["inner_array"].default_type();
	data_template->get_dependencies(global_ctx, result);

	// check for direct dependencies
	std::unordered_set<std::string> expected_dependencies = {"array_size", "type_size"};
	check_dependecies(expected_dependencies, result);


	// check for all dependencies
	std::unordered_set<std::string> expected_dependencies22 = {"array_size", "dim_size", "type_size"};
	std::unordered_set<std::string> result22 = global_ctx.m_data_all_dependencies["inner_array"];

	for (auto& elem: result22) {
		std::cout << "result 22: elem=" << elem << std::endl;
	}

	for (auto& elem: result) {
		std::cout << "result: elem=" << elem << std::endl;
	}

	check_dependecies(expected_dependencies22, result22);
}
