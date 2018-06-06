/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <pdi/global_context.h>
#include <pdi/plugin.h>
#include <pdi/paraconf_wrapper.h>

using namespace PDI;
using std::string;
using std::unique_ptr;
using std::set;

/*
 * Struct prepared for ContextTest.
 */
struct ContextTest : public ::testing::Test {
	ContextTest():
		test_conf{PC_parse_string("")},
		test_world{0}
	{}
	
	void SetUp() override
	{
		test_context.reset(new Global_context{test_conf, &test_world});
	}
	
	Paraconf_wrapper fw;
	PC_tree_t test_conf;
	MPI_Comm test_world;
	unique_ptr<Context> test_context;
};

TEST_F(ContextTest, desc_string_uninitialized)
{
	string desc_name{"test_desc"};
	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_EQ(desc_name, desc.name());
}

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

TEST_F(ContextTest, desc_cstring_uninitialized)
{
	const char* desc_name = "test_desc";
	Data_descriptor& desc = this->test_context->desc(desc_name);
	ASSERT_STREQ(desc_name, desc.name().c_str());
}

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

TEST_F(ContextTest, operator_string_uninitialized)
{
	string desc_name{"test_desc"};
	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_EQ(desc_name, desc.name());
}

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

TEST_F(ContextTest, operator_cstring_uninitialized)
{
	const char* desc_name = "test_desc";
	Data_descriptor& desc = (*this->test_context)[desc_name];
	ASSERT_STREQ(desc_name, desc.name().c_str());
}

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

//TODO? Test Context::event() somehow