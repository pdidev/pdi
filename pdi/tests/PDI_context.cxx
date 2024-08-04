/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

#include "global_context.h"


using namespace PDI;
using std::set;
using std::string;
using std::unique_ptr;

/*
 * Struct prepared for ContextTest.
 */
struct ContextTest: public ::testing::Test {
	ContextTest()
		: test_conf{PC_parse_string("logging: trace")}
	{}

	void SetUp() override { test_context.reset(new Global_context{test_conf}); }

	Paraconf_wrapper fw;
	PC_tree_t test_conf;
	unique_ptr<Context> test_context;
};

/*
 * Name:                ContextTest.desc_string_uninitialized
 *
 * Tested functions:    PDI::Context::desc(string)
 *
 * Description:         Checks if accessesing uninitialzied descriptor
 *                      creates a new one.
 */
TEST_F(ContextTest, desc_string_uninitialized)
{
	string desc_name{"test_desc"};
	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_EQ(desc_name, desc.name());
}

/*
 * Name:                ContextTest.desc_string_initialized
 *
 * Tested functions:    PDI::Context::desc(string)
 *
 * Description:         Checks if accessesing a descriptor
 *                      returns correct one.
 */
TEST_F(ContextTest, desc_string_initialized)
{
	string desc_name{"desc1"};
	//put desc1 first to check if the same desc is returned later
	Data_descriptor& desc1 = this->test_context->desc(desc_name);

	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_EQ(desc_name, desc.name());
	//desc1 and desc should have the same address if they are the same desc
	ASSERT_EQ(&desc1, &desc);
}

/*
 * Name:                ContextTest.desc_cstring_uninitialized
 *
 * Tested functions:    PDI::Context::desc(const char*)
 *
 * Description:         Checks if accessesing uninitialzied descriptor
 *                      creates a new one.
 */
TEST_F(ContextTest, desc_cstring_uninitialized)
{
	const char* desc_name = "test_desc";
	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_STREQ(desc_name, desc.name().c_str());
}

/*
 * Name:                ContextTest.desc_cstring_initialized
 *
 * Tested functions:    PDI::Context::desc(const char*)
 *
 * Description:         Checks if accessesing a descriptor
 *                      returns correct one.
 */
TEST_F(ContextTest, desc_cstring_initialized)
{
	const char* desc_name = "desc1";
	//put desc1 first to check if the same desc is returned later
	Data_descriptor& desc1 = this->test_context->desc(desc_name);

	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_STREQ(desc_name, desc.name().c_str());
	//desc1 and desc should have the same address if they are the same desc
	ASSERT_EQ(&desc1, &desc);
}

/*
 * Name:                ContextTest.operator_string_uninitialized
 *
 * Tested functions:    PDI::Context::operator[](string)
 *
 * Description:         Checks if accessesing uninitialzied descriptor
 *                      creates a new one.
 */
TEST_F(ContextTest, operator_string_uninitialized)
{
	string desc_name{"test_desc"};
	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_EQ(desc_name, desc.name());
}

/*
 * Name:                ContextTest.operator_string_initialized
 *
 * Tested functions:    PDI::Context::operator[](string)
 *
 * Description:         Checks if accessesing a descriptor
 *                      returns correct one.
 */
TEST_F(ContextTest, operator_string_initialized)
{
	string desc_name{"desc1"};
	//put desc1 first to check if the same desc is returned later
	Data_descriptor& desc1 = this->test_context->desc(desc_name);

	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_EQ(desc_name, desc.name());
	//desc1 and desc should have the same address if they are the same desc
	ASSERT_EQ(&desc1, &desc);
}

/*
 * Name:                ContextTest.operator_cstring_uninitialized
 *
 * Tested functions:    PDI::Context::operator[](const char*)
 *
 * Description:         Checks if accessesing uninitialzied descriptor
 *                      creates a new one.
 */
TEST_F(ContextTest, operator_cstring_uninitialized)
{
	const char* desc_name = "test_desc";
	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_STREQ(desc_name, desc.name().c_str());
}

/*
 * Name:                ContextTest.operator_cstring_initialized
 *
 * Tested functions:    PDI::Context::operator[](const char*)
 *
 * Description:         Checks if accessesing a descriptor
 *                      returns correct one.
 */
TEST_F(ContextTest, operator_cstring_initialized)
{
	const char* desc_name = "desc1";
	//put desc1 first to check if the same desc is returned later
	Data_descriptor& desc1 = this->test_context->desc(desc_name);

	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_STREQ(desc_name, desc.name().c_str());
	//desc1 and desc should have the same address if they are the same desc
	ASSERT_EQ(&desc1, &desc);
}

/*
 * Name:                ContextTest.iterator
 *
 * Tested functions:    PDI::Context::begin(),
 *                      PDI::Context::end()
 *
 * Description:         Checks if tested functions
 *                      return correct iterators.
 */
TEST_F(ContextTest, iterator)
{
	//put some descriptors inside context
	set<string> desc_names{"desc1", "desc2", "desc3"};
	for (auto& desc_name: desc_names) {
		this->test_context->desc(desc_name);
	}
	auto begin = this->test_context->begin();
	auto end = this->test_context->end();

	for (auto it = begin; it != end; ++it) {
		auto name = desc_names.find((*it).name());
		ASSERT_EQ(it->name(), (*it).name());
		ASSERT_TRUE(name != desc_names.end());
	}
}

/*
 * Name:                ContextTest.iterator
 *
 * Tested functions:    PDI::Context::find(),
 *                      PDI::Context::end(),
 *
 * Description:         Checks if tested functions
 *                      return correct iterators.
 */
TEST_F(ContextTest, iterator_find)
{
	// Put some descriptors inside context
	set<string> desc_names{"desc1", "desc2", "desc3"};
	for (auto& desc_name: desc_names) {
		this->test_context->desc(desc_name);
	}

	// Iterating through the descriptors to ensure we can find() them
	for (set<string>::iterator it = desc_names.begin(); it != desc_names.end(); ++it) {
		Context::Iterator descriptor = this->test_context->find(*it);
		ASSERT_EQ(descriptor->name(), (*it));
		ASSERT_TRUE(descriptor != this->test_context->end());
	}

	// test case where search key is not found
	ASSERT_FALSE(this->test_context->find("desc4") != this->test_context->end()); // TODO: add operator== to Context::Iterator
}
