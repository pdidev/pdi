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
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

#include "global_context.h"


using namespace PDI;
using std::string;
using std::unique_ptr;
using std::set;

/*
 * Struct prepared for ContextTest.
 */
struct ContextTest : public ::testing::Test {
	ContextTest():
		test_conf{PC_parse_string("")}
	{}
	
	void SetUp() override
	{
		test_context.reset(new Global_context{test_conf});
	}
	
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
 * Name:                ContextTest.add_event
 *
 * Tested functions:    PDI::Context::add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event.
 *
 */
TEST_F(ContextTest, add_event)
{
	int x = 0;
	this->test_context->add_event_callback([&x](const std::string&) {
		x += 42;
	}, "event");
	ASSERT_EQ(x, 0);
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
	this->test_context->event("event");
	ASSERT_EQ(x, 84);
}

/*
 * Name:                ContextTest.remove_event
 *
 * Tested functions:    PDI::Context::add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event
 *                      and removes it.
 *
 */
TEST_F(ContextTest, remove_event)
{
	int x = 0;
	auto erase_f = this->test_context->add_event_callback([&x](const std::string&) {
		x += 42;
	}, "event");
	ASSERT_EQ(x, 0);
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
	erase_f();
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
}

/*
 * Name:                ContextTest.add_remove_event
 *
 * Tested functions:    PDI::Context::add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event
 *                      and removes it several times.
 *
 */
TEST_F(ContextTest, add_remove_event)
{
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->add_event_callback([&x](const std::string&) {
		x += 42;
	}, "event_x");
	auto erase_y = this->test_context->add_event_callback([&y](const std::string&) {
		y += 53;
	}, "event_y");
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->event("event_x");
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
	this->test_context->event("event_y");
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 53);
	erase_x();
	this->test_context->event("event_x");
	this->test_context->event("event_y");
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 106);
	auto erase_x_2 = this->test_context->add_event_callback([&x](const std::string&) {
		x += 42;
	}, "event_x_2");
	this->test_context->event("event_x_2");
	this->test_context->event("event_y");
	ASSERT_EQ(x, 84);
	ASSERT_EQ(y, 159);
	erase_y();
	this->test_context->event("event_x_2");
	this->test_context->event("event_y");
	ASSERT_EQ(x, 126);
	ASSERT_EQ(y, 159);
	erase_x_2();
	ASSERT_EQ(x, 126);
	ASSERT_EQ(y, 159);
}

/*
 * Name:                ContextTest.add_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, add_data_callback)
{
	string data_x {"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	ASSERT_EQ(x, 0);
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
}

/*
 * Name:                ContextTest.add_named_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, add_named_data_callback)
{
	string data_x {"data_x"};
	string data_y {"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	this->test_context->desc(data_y).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	int y = 0;
	this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	}, "data_x");
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
}

/*
 * Name:                ContextTest.remove_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it.
 */
TEST_F(ContextTest, remove_data_callback)
{
	string data_x {"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	auto erase_x = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	ASSERT_EQ(x, 0);
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	erase_x();
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
}

/*
 * Name:                ContextTest.remove_named_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, remove_named_data_callback)
{
	string data_x {"data_x"};
	string data_y {"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	this->test_context->desc(data_y).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	}, "data_x");
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
	erase_x();
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
}

/*
 * Name:                ContextTest.add_remove_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(ContextTest, add_remove_data_callback)
{
	string data_x {"data_x"};
	string data_y {"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	this->test_context->desc(data_y).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += std::stoi(name);
	});
	auto erase_y = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* y = static_cast<int*>(ref_write.get());
		*y += std::stoi(name) + 1;
	});
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("1").share(&x, true, true);
	this->test_context->desc("1").reclaim();
	ASSERT_EQ(x, 3);
	ASSERT_EQ(y, 0);
	this->test_context->desc("2").share(&y, true, true);
	this->test_context->desc("2").reclaim();
	ASSERT_EQ(x, 3);
	ASSERT_EQ(y, 5);
	erase_x();
	this->test_context->desc("3").share(&x, true, true);
	this->test_context->desc("3").reclaim();
	ASSERT_EQ(x, 7);
	ASSERT_EQ(y, 5);
	this->test_context->desc("4").share(&y, true, true);
	this->test_context->desc("4").reclaim();
	ASSERT_EQ(x, 7);
	ASSERT_EQ(y, 10);
	erase_y();
	this->test_context->desc("5").share(&x, true, true);
	this->test_context->desc("6").share(&y, true, true);
	this->test_context->desc("5").reclaim();
	this->test_context->desc("6").reclaim();
	ASSERT_EQ(x, 7);
	ASSERT_EQ(y, 10);
}

/*
 * Name:                ContextTest.add_remove_named_data_callback
 *
 * Tested functions:    PDI::Context::add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(ContextTest, add_remove_named_data_callback)
{
	string data_x {"data_x"};
	string data_y {"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	this->test_context->desc(data_y).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	}, "data_x");
	auto erase_y = this->test_context->add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write {ref};
		int* y = static_cast<int*>(ref_write.get());
		*y += 53;
		ASSERT_STREQ(name.c_str(), "data_y");
	}, "data_y");
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_y").share(&y, true, true);
	this->test_context->desc("data_y").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 53);
	erase_x();
	this->test_context->desc("data_x").share(&x, true, true);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 53);
	this->test_context->desc("data_y").share(&y, true, true);
	this->test_context->desc("data_y").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 106);
	erase_y();
	this->test_context->desc("data_y").share(&y, true, true);
	this->test_context->desc("data_y").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 106);
}

/*
 * Name:                ContextTest.add_empty_desc_callback
 *
 * Tested functions:    PDI::Context::add_empty_desc_access_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access.
 */
TEST_F(ContextTest, add_empty_desc_callback)
{
	string data_x {"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	this->test_context->add_empty_desc_access_callback([this](const std::string& name) {
		int* x = new int;
		*x = 42;
		this->test_context->desc(name).share(x, true, true);
	});
	Ref_r ref_read {this->test_context->desc(data_x).ref()};
	int x = *static_cast<const int*>(ref_read.get());
	ASSERT_EQ(x, 42);
	int* data = static_cast<int*>(this->test_context->desc(data_x).reclaim());
	delete data;
}

/*
 * Name:                ContextTest.remove_empty_desc_callback
 *
 * Tested functions:    PDI::Context::add_empty_desc_access_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access
 *                      and removes it.
 */
TEST_F(ContextTest, remove_empty_desc_callback)
{
	string data_x {"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}.clone_type());
	auto erase_x = this->test_context->add_empty_desc_access_callback([this](const std::string& name) {
		int* x = new int;
		*x = 42;
		this->test_context->desc(name).share(x, true, true);
	});
	Ref_r ref_read {this->test_context->desc(data_x).ref()};
	int x = *static_cast<const int*>(ref_read.get());
	ASSERT_EQ(x, 42);
	int* data = static_cast<int*>(this->test_context->desc(data_x).reclaim());
	delete data;
	erase_x();
	try {
		Ref ref_x {this->test_context->desc(data_x).ref()};
		FAIL();
	} catch (Error e) {
		ASSERT_EQ(e.status(), PDI_ERR_VALUE);
	}
}
