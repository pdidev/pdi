/*******************************************************************************
 * Copyright (C) 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

#include "data_descriptor_impl.h"

using namespace PDI;
using namespace std;

using param_pair = pair<string, shared_ptr<Datatype>>;

namespace PDI {

//handler to private fields of Descriptor
class Descriptor_test_handler {
public:
	static unique_ptr<Data_descriptor> default_desc(Global_context& global_ctx)
	{
		return unique_ptr<Data_descriptor>{new Data_descriptor_impl{global_ctx, "default_desc"}};
	}

	static Datatype_sptr desc_get_type(unique_ptr<Data_descriptor>& desc, Global_context& global_ctx)
	{
		Datatype_template_sptr desc_template = dynamic_cast<Data_descriptor_impl*>(desc.get())->m_type;
		return desc_template->evaluate(global_ctx);
	}

	static int desc_get_refs_number(unique_ptr<Data_descriptor>& desc) { return dynamic_cast<Data_descriptor_impl*>(desc.get())->m_refs.size(); }
};

} // namespace PDI

/*
 * Struct prepared for DataDescTest
 */
struct DataDescTest: public ::testing::Test {
	int array[10];
	PC_tree_t array_config{PC_parse_string("{ size: 10, type: array, subtype: int }")};
	shared_ptr<Array_datatype> array_datatype{Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10)};
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("")};
	unique_ptr<Data_descriptor> m_desc_default = Descriptor_test_handler::default_desc(global_ctx);
};

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
 * Struct prepared for PositiveTypeParseTest.
 */
struct PositiveTypeParseTest: public ::testing::TestWithParam<param_pair> {
	PC_tree_t conf = PC_parse_string("logging: trace");
	Paraconf_wrapper _;
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
	ASSERT_FALSE(this->test_context->find("desc4") != this->test_context->end());
	ASSERT_TRUE(this->test_context->find("desc5") == this->test_context->end());
}

/*
 * Name:                ContextTest.iterator
 *
 * Tested functions:    operator==,
 *                      operator!=
 *
 * Description:         Checks if tested functions
 *                      return correct iterators.
 */
TEST_F(ContextTest, iterator_operator_equal_equal)
{
	//put some descriptors inside context
	set<string> desc_names{"desc1", "desc2", "desc3"};
	for (auto& desc_name: desc_names) {
		this->test_context->desc(desc_name);
	}
	auto begin = this->test_context->begin();
	auto end = this->test_context->end();

	// test operator==
	for (set<string>::iterator it2 = desc_names.begin(); it2 != desc_names.end(); ++it2) {
		Context::Iterator descriptor = this->test_context->find(*it2);

		int counter_ok = 0;

		for (auto it = begin; it != end; ++it) {
			if (it == descriptor) {
				counter_ok++;
				ASSERT_FALSE(it != descriptor);
			}
		}
		ASSERT_EQ(counter_ok, 1);
	}

	// test operator!=
	for (set<string>::iterator it2 = desc_names.begin(); it2 != desc_names.end(); ++it2) {
		Context::Iterator descriptor = this->test_context->find(*it2);

		int counter_false = 0;

		for (auto it = begin; it != end; ++it) {
			if (it != descriptor) {
				counter_false++;
				ASSERT_FALSE(it == descriptor);
			}
		}

		ASSERT_EQ(counter_false, 2);
	}
}

/*
 * Name:                ContextTest.add_event
 *
 * Tested functions:    PDI::Context::on_event
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event.
 *
 */
TEST_F(ContextTest, add_event)
{
	int x = 0;
	this->test_context->on_event([&x](const std::string&) { x += 42; }, "event");
	ASSERT_EQ(x, 0);
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
	this->test_context->event("event");
	ASSERT_EQ(x, 84);
}

/*
 * Name:                ContextTest.remove_event
 *
 * Tested functions:    PDI::Context::on_event
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
	auto erase_f = this->test_context->on_event([&x](const std::string&) { x += 42; }, "event");
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
 * Tested functions:    PDI::Context::on_event
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
	auto erase_x = this->test_context->on_event([&x](const std::string&) { x += 42; }, "event_x");
	auto erase_y = this->test_context->on_event([&y](const std::string&) { y += 53; }, "event_y");
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
	auto erase_x_2 = this->test_context->on_event([&x](const std::string&) { x += 42; }, "event_x_2");
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
 * Name:                ContextTest.on_data
 *
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, on_data)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->on_data([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
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
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, add_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	this->test_context->on_data(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
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
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it.
 */
TEST_F(ContextTest, remove_data_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto erase_x = this->test_context->on_data([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
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
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(ContextTest, remove_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->on_data(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
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
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(ContextTest, add_remove_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->on_data([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += std::stoi(name);
	});
	auto erase_y = this->test_context->on_data([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
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
 * Tested functions:    PDI::Context::on_data
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(ContextTest, add_remove_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->on_data(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	auto erase_y = this->test_context->on_data(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* y = static_cast<int*>(ref_write.get());
			*y += 53;
			ASSERT_STREQ(name.c_str(), "data_y");
		},
		"data_y"
	);
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
 * Tested functions:    PDI::Context::on_missing_data
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access.
 */
TEST_F(ContextTest, add_empty_desc_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->on_missing_data([this](const std::string& name) {
		int* x = new int;
		*x = 42;
		this->test_context->desc(name).share(x, true, true);
	});
	Ref_r ref_read{this->test_context->desc(data_x).ref()};
	int x = *static_cast<const int*>(ref_read.get());
	ASSERT_EQ(x, 42);
	int* data = static_cast<int*>(this->test_context->desc(data_x).reclaim());
	delete data;
}

/*
 * Name:                ContextTest.remove_empty_desc_callback
 *
 * Tested functions:    PDI::Context::on_missing_data
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access
 *                      and removes it.
 */
TEST_F(ContextTest, remove_empty_desc_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	auto erase_x = this->test_context->on_missing_data([this](const std::string& name) {
		int* x = new int;
		*x = 42;
		this->test_context->desc(name).share(x, true, true);
	});
	Ref_r ref_read{this->test_context->desc(data_x).ref()};
	int x = *static_cast<const int*>(ref_read.get());
	ASSERT_EQ(x, 42);
	int* data = static_cast<int*>(this->test_context->desc(data_x).reclaim());
	delete data;
	erase_x();
	try {
		Ref ref_x{this->test_context->desc(data_x).ref()};
		FAIL();
	} catch (Value_error& e) {
		ASSERT_EQ(e.status(), PDI_ERR_VALUE);
	}
}

/*
 * Name:                ContextTest.add_data_remove_callback_reclaim
 *
 * Tested functions:    PDI::Context::add_data_remove_callback_reclaim
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(ContextTest, add_data_remove_callback_reclaim)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->on_data_remove([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	this->test_context->desc("data_x").share(&x, true, true);
	ASSERT_EQ(x, 0);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
}

/*
 * Name:                ContextTest.add_data_remove_callback_release
 *
 * Tested functions:    PDI::Context::add_data_remove_callback_release
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(ContextTest, add_data_remove_callback_release)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->on_data_remove([&x](const std::string& name, Ref ref) {
		x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	void* memory_to_free = malloc(sizeof(int));
	this->test_context->desc("data_x").share(memory_to_free, true, true);
	ASSERT_EQ(x, 0);
	this->test_context->desc("data_x").release();
	ASSERT_EQ(x, 42);
}

/*
 * Name:                ContextTest.add_named_data_remove_callback_reclaim
 *
 * Tested functions:    PDI::Context::add_named_data_remove_callback_reclaim
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(ContextTest, add_named_data_remove_callback_reclaim)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	this->test_context->on_data_remove(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	this->test_context->on_data_remove(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* y = static_cast<int*>(ref_write.get());
			*y += 42;
			ASSERT_STREQ(name.c_str(), "data_y");
		},
		"data_y"
	);
	this->test_context->desc("data_x").share(&x, true, true);
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
}

/*
 * Name:                ContextTest.add_named_data_remove_callback_release
 *
 * Tested functions:    PDI::Context::add_named_data_remove_callback_release
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(ContextTest, add_named_data_remove_callback_release)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	void* memory_to_free = malloc(sizeof(int));
	int y = 0;
	this->test_context->on_data_remove(
		[&x](const std::string& name, Ref ref) {
			x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	this->test_context->on_data_remove(
		[&y](const std::string& name, Ref ref) {
			y += 42;
			ASSERT_STREQ(name.c_str(), "data_y");
		},
		"data_y"
	);
	this->test_context->desc("data_x").share(memory_to_free, true, true);
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);
	this->test_context->desc("data_x").release();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 0);
}

/*
 * Name:                ContextTest.add_data_remove_callback_reclaim_remove
 *
 * Tested functions:    PDI::Context::add_data_remove_callback_reclaim_remove
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(ContextTest, add_data_remove_callback_reclaim_remove)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto remove_callback = this->test_context->on_data_remove([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	this->test_context->desc("data_x").share(&x, true, true);
	ASSERT_EQ(x, 0);
	remove_callback();
	this->test_context->desc("data_x").reclaim();
	ASSERT_EQ(x, 0);
}

/*
 * Name:                ContextTest.add_data_remove_callback_release_remove
 *
 * Tested functions:    PDI::Context::add_data_remove_callback_release_remove
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(ContextTest, add_data_remove_callback_release_remove)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto remove_callback = this->test_context->on_data_remove([&x](const std::string& name, Ref ref) {
		x += 42;
		ASSERT_STREQ(name.c_str(), "data_x");
	});
	void* memory_to_free = malloc(sizeof(int));
	this->test_context->desc("data_x").share(memory_to_free, true, true);
	ASSERT_EQ(x, 0);
	remove_callback();
	this->test_context->desc("data_x").release();
	ASSERT_EQ(x, 0);
}

/*
 * Name:                DataDescTest.check_default_fields
 *
 * Tested functions:    PDI::Data_descriptor::Data_descriptor(Context, const char*)
 *
 * Description:         Checks if default values are correct.
 */
TEST_F(DataDescTest, check_default_fields)
{
	Datatype_sptr desc_type = Descriptor_test_handler::desc_get_type(this->m_desc_default, global_ctx);
	shared_ptr<const Scalar_datatype> default_scalar = static_pointer_cast<const Scalar_datatype>(desc_type);
	ASSERT_EQ(Scalar_kind::UNKNOWN, default_scalar->kind());
	ASSERT_EQ(0, desc_type->datasize());
	ASSERT_STREQ("default_desc", this->m_desc_default->name().c_str());
	ASSERT_FALSE(this->m_desc_default->metadata());
}

/*
 * Name:                DataDescTest.check_metadata_update
 *
 * Tested functions:    PDI::Data_descriptor::metadata()
 *                      PDI::Data_descriptor::metadata(bool)
 *
 * Description:         Checks if can change metadata flag.
 */
TEST_F(DataDescTest, check_metadata_update)
{
	ASSERT_FALSE(this->m_desc_default->metadata());
	this->m_desc_default->metadata(true);
	ASSERT_TRUE(this->m_desc_default->metadata());
}

/*
 * Name:                DataDescTest.default_type
 *
 * Tested functions:    PDI::Data_descriptor::default_type(PC_tree_t)
 *
 * Description:         Check if config is parsed and type is correctly read by PDI.
 */
TEST_F(DataDescTest, default_type)
{
	Paraconf_wrapper fw;
	this->m_desc_default->default_type(array_datatype);
	Datatype_sptr datatype = Descriptor_test_handler::desc_get_type(this->m_desc_default, global_ctx);
	ASSERT_EQ(10 * sizeof(int), datatype->datasize());
	ASSERT_EQ(10 * sizeof(int), datatype->buffersize());
}

/*
 * Name:                DataDescTest.catch_empty_exception
 *
 * Tested functions:    PDI::Data_descriptor::ref()
 *
 * Description:         Checks if exception is thrown when accessing empty ref.
 */
TEST_F(DataDescTest, catch_empty_exception)
{
	try {
		this->m_desc_default->ref();
		FAIL();
	} catch (const Value_error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_VALUE, err.status());
	}
}

/*
 * Name:                DataDescTest.simply_share_data
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *
 * Description:         Shares data and checks if good privilege was granted.
 */
TEST_F(DataDescTest, simply_share_data)
{
	this->m_desc_default->share(this->array, false, true);

	Ref created_ref = this->m_desc_default->ref();
	void* ptr = Ref_w{created_ref}.get();
	ASSERT_EQ(this->array, ptr);

	this->m_desc_default->reclaim();
}

/*
 * Name:                DataDescTest.multi_read_share_data
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *                      PDI::Data_descriptor::share(Ref, bool, bool)
 *                      PDI::Data_descriptor::ref()
 *                      PDI::Data_descriptor::release()
 *                      PDI::Data_descriptor::reclaim()
 *
 * Description:         Shares multiple times same data ref and checks
 *                      if correct numbers of ref owners is returned.
 */
TEST_F(DataDescTest, multi_read_share_data)
{
	this->m_desc_default->share(this->array, true, false);
	void* ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(this->array, ptr);
	ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(this->array, ptr);

	int refs_number = Descriptor_test_handler::desc_get_refs_number(this->m_desc_default);

	ASSERT_EQ(3, refs_number);

	this->m_desc_default->reclaim();
	ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(nullptr, ptr);
	this->m_desc_default->release();

	refs_number = Descriptor_test_handler::desc_get_refs_number(this->m_desc_default);
	ASSERT_EQ(2, refs_number);

	this->m_desc_default->release();
	refs_number = Descriptor_test_handler::desc_get_refs_number(this->m_desc_default);
	ASSERT_EQ(1, refs_number);

	this->m_desc_default->release();
	refs_number = Descriptor_test_handler::desc_get_refs_number(this->m_desc_default);
	ASSERT_EQ(0, refs_number);
}

/*
 * Name:                DataDescTest.multi_write_share_data
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *                      PDI::Data_descriptor::share(Ref, bool, bool)
 *                      PDI::Data_descriptor::ref()
 *
 * Description:         Expect exception while tring to get write access
 *                      second time.
 */
TEST_F(DataDescTest, multi_write_share_data)
{
	this->m_desc_default->share(this->array, false, true);
	this->m_desc_default->share(this->m_desc_default->ref(), false, true);
	try {
		this->m_desc_default->share(this->m_desc_default->ref(), false, true);
		FAIL();
	} catch (const Error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_PERMISSION, err.status());
	}

	this->m_desc_default->reclaim();
}

/*
 * Name:                DataDescTest.read_write_share_data
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *                      PDI::Data_descriptor::share(Ref, bool, bool)
 *                      PDI::Data_descriptor::ref()
 *                      PDI::Data_descriptor::release()
 *                      PDI::Data_descriptor::reclaim()
 *
 * Description:         Expect exception while tring to get write access
 *                      after granted read access.
 */
TEST_F(DataDescTest, read_write_share_data)
{
	this->m_desc_default->share(this->array, true, true);
	this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	try {
		this->m_desc_default->share(this->m_desc_default->ref(), false, true);
		FAIL();
	} catch (const Error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_PERMISSION, err.status());
	}
	this->m_desc_default->release();

	void* ptr = this->m_desc_default->share(this->m_desc_default->ref(), false, true);
	ASSERT_EQ(this->array, ptr);
	try {
		this->m_desc_default->share(this->m_desc_default->ref(), true, false);
		FAIL();
	} catch (const Error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_PERMISSION, err.status());
	}
	this->m_desc_default->reclaim();
}

/*
 * Name:                DataDescTest.simply_share_meta
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *                      PDI::Data_descriptor::ref()
 *
 * Description:         Shares metadata and checks if good privilege was granted.
 */
TEST_F(DataDescTest, simply_share_meta)
{
	this->m_desc_default->share(this->array, true, false);

	Ref created_ref = this->m_desc_default->ref();
	ASSERT_TRUE(created_ref);
	ASSERT_FALSE(Ref_w{created_ref});

	this->m_desc_default->reclaim();
}

/*
 * Name:                DataDescTest.share_meta_without_read
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *
 * Description:         Expects exception while sharing metadata without
 *                      read privilege.
 */
TEST_F(DataDescTest, share_meta_without_read)
{
	this->m_desc_default->metadata(true);
	try {
		this->m_desc_default->share(this->array, false, true);
		FAIL();
	} catch (const Error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_PERMISSION, err.status());
	}
}

/*
 * Name:                DataDescTest.multi_read_share_meta
 *
 * Tested functions:    PDI::Data_descriptor::share(void*, bool, bool)
 *                      PDI::Data_descriptor::ref()
 *
 * Description:         Shares multiple times same metadata ref and checks
 *                      if correct numbers of ref owners is returned.
 */
TEST_F(DataDescTest, multi_read_share_meta)
{
	this->m_desc_default->default_type(array_datatype);
	ASSERT_EQ(0, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));
	this->m_desc_default->metadata(true);
	ASSERT_EQ(1, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));

	this->m_desc_default->share(this->array, true, false);
	ASSERT_EQ(2, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));

	const void* ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(3, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));
	ASSERT_EQ(this->array, ptr);

	ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(4, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));
	ASSERT_EQ(this->array, ptr);

	this->m_desc_default->release();
	ASSERT_EQ(3, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));

	ptr = this->m_desc_default->share(this->m_desc_default->ref(), true, false);
	ASSERT_EQ(4, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));
	ASSERT_EQ(this->array, ptr);

	this->m_desc_default->release();
	ASSERT_EQ(3, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));

	this->m_desc_default->release();
	ASSERT_EQ(2, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));

	ptr = this->m_desc_default->reclaim();
	ASSERT_EQ(1, Descriptor_test_handler::desc_get_refs_number(this->m_desc_default));
	ASSERT_EQ(this->array, ptr);

	ptr = Ref_r{this->m_desc_default->ref()}.get();
	ASSERT_NE(this->array, ptr);
}

/*
 * Name:                PositiveTypeParseTest.parse
 *
 * Tested functions:    PDI::Datatype_template::load()
 *                      PDI::Datatype_template::evaluate(Context&)
 *
 * Description:         Checks if correct type is parsed from tree.
 */
TEST_P(PositiveTypeParseTest, parse)
{
	Global_context g_context{this->conf};
	auto&& params = GetParam();
	auto&& parsed_datatype = g_context.datatype(PC_parse_string(params.first.c_str()))->evaluate(g_context);
	ASSERT_TRUE(*parsed_datatype == *params.second)
		<< "When parsing: \"" << params.first << "\"" << std::endl
		<< "Expected: \"" << params.second->debug_string() << "\"" << std::endl
		<< "Actual: \"" << parsed_datatype->debug_string() << "\"" << std::endl;
}

/*
 * Struct prepared for NegativeTypeParseTest.
 */
struct NegativeTypeParseTest: public ::testing::TestWithParam<string> {
	PC_tree_t conf = PC_parse_string("");
	Paraconf_wrapper _;
};

/*
 * Name:                NegativeTypeParseTest.parse
 *
 * Tested functions:    PDI::Datatype_template::load()
 *                      PDI::Datatype_template::evaluate(Context&)
 *
 * Description:         Checks if error is thrown when given invalid data.
 */
TEST_P(NegativeTypeParseTest, parse)
{
	Global_context g_context{this->conf};
	ASSERT_THROW(g_context.datatype(PC_parse_string(GetParam().c_str())), PDI::Error);
}

vector<param_pair> scalar_types{
	{"char", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"type: char", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"{type: char, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
	{"int", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"type: int", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"{type: int, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
	{"short", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"type: short", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"{type: short, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(short))},
	{"unsigned short", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"type: unsigned short", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"{type: unsigned short, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned short))},
	{"int8", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"type: int8", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"{type: int8, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"int16", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"type: int16", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"{type: int16, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"int32", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"type: int32", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"{type: int32, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"int64", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"type: int64", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"{type: int64, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"int8_t", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"type: int8_t", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"{type: int8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 1)},
	{"int16_t", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"type: int16_t", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"{type: int16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 2)},
	{"int32_t", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"type: int32_t", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"{type: int32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 4)},
	{"int64_t", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"type: int64_t", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"{type: int64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, 8)},
	{"uint8", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"type: uint8", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"{type: uint8, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"uint16", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"type: uint16", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"{type: uint16, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"uint32", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"type: uint32", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"{type: uint32, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"uint64", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"type: uint64", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"{type: uint64, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"uint8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"type: uint8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"{type: uint8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 1)},
	{"uint16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"type: uint16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"{type: uint16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 2)},
	{"uint32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"type: uint32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"{type: uint32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 4)},
	{"uint64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"type: uint64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"{type: uint64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 8)},
	{"int_least8", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"type: int_least8", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"{type: int_least8, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"int_least16", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"type: int_least16", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"{type: int_least16, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"int_least32", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"type: int_least32", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"{type: int_least32, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"int_least64", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"type: int_least64", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"{type: int_least64, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"int_least8_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"type: int_least8_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"{type: int_least8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least8_t))},
	{"int_least16_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"type: int_least16_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"{type: int_least16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least16_t))},
	{"int_least32_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"type: int_least32_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"{type: int_least32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least32_t))},
	{"int_least64_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"type: int_least64_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"{type: int_least64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_least64_t))},
	{"uint_least8", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"type: uint_least8", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"{type: uint_least8, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"uint_least16", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"type: uint_least16", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"{type: uint_least16, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"uint_least32", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"type: uint_least32", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"{type: uint_least32, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"uint_least64", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"type: uint_least64", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"{type: uint_least64, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"uint_least8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"type: uint_least8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"{type: uint_least8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least8_t))},
	{"uint_least16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"type: uint_least16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"{type: uint_least16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least16_t))},
	{"uint_least32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"type: uint_least32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"{type: uint_least32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least32_t))},
	{"uint_least64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"type: uint_least64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"{type: uint_least64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_least64_t))},
	{"int_fast8", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"type: int_fast8", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"{type: int_fast8, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"int_fast16", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"type: int_fast16", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"{type: int_fast16, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"int_fast32", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"type: int_fast32", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"{type: int_fast32, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"int_fast64", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"type: int_fast64", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"{type: int_fast64, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"int_fast8_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"type: int_fast8_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"{type: int_fast8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast8_t))},
	{"int_fast16_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"type: int_fast16_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"{type: int_fast16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast16_t))},
	{"int_fast32_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"type: int_fast32_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"{type: int_fast32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast32_t))},
	{"int_fast64_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"type: int_fast64_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"{type: int_fast64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int_fast64_t))},
	{"uint_fast8", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"type: uint_fast8", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"{type: uint_fast8, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"uint_fast16", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"type: uint_fast16", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"{type: uint_fast16, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"uint_fast32", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"type: uint_fast32", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"{type: uint_fast32, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"uint_fast64", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"type: uint_fast64", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"{type: uint_fast64, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"uint_fast8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"type: uint_fast8_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"{type: uint_fast8_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast8_t))},
	{"uint_fast16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"type: uint_fast16_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"{type: uint_fast16_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast16_t))},
	{"uint_fast32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"type: uint_fast32_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"{type: uint_fast32_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast32_t))},
	{"uint_fast64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"type: uint_fast64_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"{type: uint_fast64_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uint_fast64_t))},
	{"intmax", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"type: intmax", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"{type: intmax, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"intmax_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"type: intmax_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"{type: intmax_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intmax_t))},
	{"uintmax", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"type: uintmax", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"{type: uintmax, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"uintmax_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"type: uintmax_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"{type: uintmax_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintmax_t))},
	{"intptr", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"type: intptr", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"{type: intptr, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"intptr_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"type: intptr_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"{type: intptr_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(intptr_t))},
	{"uintptr", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"type: uintptr", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"{type: uintptr, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"uintptr_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"type: uintptr_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"{type: uintptr_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(uintptr_t))},
	{"long", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"type: long", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"{type: long, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long))},
	{"unsigned long", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"type: unsigned long", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"{type: unsigned long, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long))},
	{"long long", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"type: long long", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"{type: long long, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long long))},
	{"unsigned long long", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"type: unsigned long long", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"{type: unsigned long long, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(unsigned long long))},
	{"float", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"type: float", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"{type: float, kind: 0}", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(float))},
	{"double", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"type: double", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"{type: double, kind: 0}", Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))},
	{"size_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"type: size_t", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"{type: size_t, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(size_t))},
	{"ptrdiff_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"type: ptrdiff_t", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"{type: ptrdiff_t, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(ptrdiff_t))},
	{"byte", Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},
	{"type: byte", Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},
	{"{type: byte, kind: 0}", Scalar_datatype::make(Scalar_kind::UNKNOWN, 1)},


#ifdef BUILD_FORTRAN
	{"character", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"type: character", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"{type: character, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_CHARACTER_DEFAULT_KIND)},
	{"{type: character, kind: 256}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 256)},
	{"integer", Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"type: integer", Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"{type: integer, kind: 0}", Scalar_datatype::make(Scalar_kind::SIGNED, PDI_INTEGER_DEFAULT_KIND)},
	{"{type: integer, kind: 256}", Scalar_datatype::make(Scalar_kind::SIGNED, 256)},
	{"logical", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"type: logical", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"{type: logical, kind: 0}", Scalar_datatype::make(Scalar_kind::UNSIGNED, PDI_LOGICAL_DEFAULT_KIND)},
	{"{type: logical, kind: 256}", Scalar_datatype::make(Scalar_kind::UNSIGNED, 256)},
	{"real", Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"type: real", Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"{type: real, kind: 0}", Scalar_datatype::make(Scalar_kind::FLOAT, PDI_REAL_DEFAULT_KIND)},
	{"{type: real, kind: 256}", Scalar_datatype::make(Scalar_kind::FLOAT, 256)},
#endif // BUILD_FORTRAN
};

vector<param_pair> array_types{
	{"{size: 10, type: array, subtype: char}", Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 10)},
	{"{type: array, size: 20, subsize: 15, start: 5, subtype: char}",
     Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 20, 5, 15)},
	{"{size: 30, subsize: 15, type: array, subtype: char}",
     Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 30, 0, 15)},
	{"{size: 40, start: 20, subsize: 10, type: array, subtype: char}",
     Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 40, 20, 10)},
	{"{size: [10000, 10000], type: array, subtype: int}",
     Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10000), 10000)},
	// TODO: FIX THE BUG WITH PARSING
	// {
	//  "{size: [10000, 10000], start: [512, 512], type: array, subtype: int}",
	//  shared_ptr<Datatype> {
	//      new Array_datatype {
	//          unique_ptr<Datatype> (new Array_datatype {
	//              unique_ptr<Datatype>(new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}), 10000, 512, 10000 - 512
	//          }),
	//          10000,
	//          512,
	//          10000 - 512
	//      }
	//  }
	// },
	{"{size: [10000, 10000], subsize: [200, 400], type: array, subtype: int}",
     Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10000, 0, 400), 10000, 0, 200)},
	{"{size: [10000, 10000], start: [256, 128], subsize: [1000, 2000], type: array, subtype: int}",
     Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10000, 128, 2000), 10000, 256, 1000)}
};

vector<param_pair> record_types{
	{"type: record    \n"
     "buffersize: 8   \n"
     "members:        \n"
     "   my_char:     \n"
     "     disp: 0    \n"
     "     type: char \n"
     "   my_int:      \n"
     "     disp: 4    \n"
     "     type: int  \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{4, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "my_int"}
		 },
		 8
	 )},
	{"type: record            \n"
     "buffersize: 808         \n"
     "members:                \n"
     "   my_char:             \n"
     "     disp: 0            \n"
     "     type: char         \n"
     "   my_array:            \n"
     "     disp: 8            \n"
     "     type: array        \n"
     "     subtype: int64\n"
     "     size: [10, 10]     \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10),
				 "my_array"
			 }
		 },
		 808
	 )},
	{"type: record            \n"
     "buffersize: 808         \n"
     "members:                \n"
     "   my_char:             \n"
     "     disp: 0            \n"
     "     type: char         \n"
     "   my_array:            \n"
     "     disp: 8            \n"
     "     type: array        \n"
     "     subtype: int64     \n"
     "     size: [10, 10]     \n"
     "     start: [2, 3]      \n"
     "     subsize: [6, 5]    \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10, 3, 5), 10, 2, 6),
				 "my_array"
			 }
		 },
		 808
	 )},
	{"type: record                \n"
     "buffersize: 816             \n"
     "members:                    \n"
     "   my_char:                 \n"
     "     disp: 0                \n"
     "     type: char             \n"
     "   my_record:               \n"
     "     disp: 8                \n"
     "     type: record           \n"
     "     buffersize: 808        \n"
     "     members:               \n"
     "       my_char:             \n"
     "         disp: 0            \n"
     "         type: char         \n"
     "       my_array:            \n"
     "         disp: 8            \n"
     "         type: array        \n"
     "         subtype: int64\n"
     "         size: [10, 10]     \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Record_datatype::make(
					 vector<Record_datatype::Member>{
						 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
						 Record_datatype::Member{
							 8,
							 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10),
							 "my_array"
						 }
					 },
					 808
				 ),
				 "my_record"
			 }
		 },
		 816
	 )}
};

vector<param_pair> struct_types{
	{"type: struct    \n"
     "members:        \n"
     "   - my_char: char\n"
     "   - my_int: int  \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{4, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "my_int"}
		 },
		 8
	 )},
	{"type: struct            \n"
     "members:                \n"
     "   - my_char: char        \n"
     "   - my_array:            \n"
     "       type: array        \n"
     "       subtype: int64     \n"
     "       size: [10, 10]     \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10),
				 "my_array"
			 }
		 },
		 808
	 )},
	{"type: struct              \n"
     "members:                  \n"
     "   - my_char: char        \n"
     "   - my_array:            \n"
     "       type: array        \n"
     "       subtype: int64     \n"
     "       size: [10, 10]     \n"
     "       start: [2, 3]      \n"
     "       subsize: [6, 5]    \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10, 3, 5), 10, 2, 6),
				 "my_array"
			 }
		 },
		 808
	 )},
	{"type: struct                \n"
     "members:                    \n"
     "   - my_char: char            \n"
     "   - my_record:               \n"
     "       type: struct           \n"
     "       members:               \n"
     "         - my_char:             \n"
     "             type: char         \n"
     "         - my_array:            \n"
     "             type: array        \n"
     "             subtype: int64     \n"
     "             size: [10, 10]     \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{
				 8,
				 Record_datatype::make(
					 vector<Record_datatype::Member>{
						 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
						 Record_datatype::Member{
							 8,
							 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10),
							 "my_array"
						 }
					 },
					 808
				 ),
				 "my_record"
			 }
		 },
		 816
	 )}
};

vector<param_pair> pointer_types{
	{"{type: pointer, subtype: int}", Pointer_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)))},
	{"{type: pointer, subtype: {type: pointer, subtype: int}}",
     Pointer_datatype::make(Pointer_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))))},
	{"{type: pointer, subtype: {type: array, subtype: int, size: 32}}",
     Pointer_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 32))},
	{"{type: array, subtype: {type: pointer, subtype: int}, size: 32}",
     Array_datatype::make(Pointer_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))), 32)},
	{"type: pointer     \n"
     "subtype:          \n"
     "  type: record    \n"
     "  buffersize: 8   \n"
     "  members:        \n"
     "     my_char:     \n"
     "       disp: 0    \n"
     "       type: char \n"
     "     my_int:      \n"
     "       disp: 4    \n"
     "       type: int  \n",
     Pointer_datatype::make(Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
			 Record_datatype::Member{4, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "my_int"}
		 },
		 8
	 ))},
	{"type: record       \n"
     "buffersize: 16     \n"
     "members:           \n"
     "   my_ptr:         \n"
     "     disp: 0       \n"
     "     type: pointer \n"
     "     subtype: char \n"
     "   my_int:         \n"
     "     disp: 8       \n"
     "     type: int     \n",
     Record_datatype::make(
		 vector<Record_datatype::Member>{
			 Record_datatype::Member{0, Pointer_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))), "my_ptr"},
			 Record_datatype::Member{8, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "my_int"}
		 },
		 16
	 )}
};

vector<param_pair> tuple_types{
	{"type: tuple    \n"
     "elements:        \n"
     "   - char\n"
     "   - int  \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{4, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))}
		 },
		 8
	 )},
	{"type: tuple       \n"
     "buffersize: 13    \n"
     "elements:         \n"
     "   - type: char   \n"
     "     disp: 0      \n"
     "   - type: int    \n"
     "     disp: 1      \n"
     "   - type: double \n"
     "     disp: 5      \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{1, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int))},
			 Tuple_datatype::Element{5, Scalar_datatype::make(Scalar_kind::FLOAT, sizeof(double))}
		 },
		 13
	 )},
	{"type: tuple             \n"
     "elements:               \n"
     "   - char               \n"
     "   - type: array        \n"
     "     subtype: int64     \n"
     "     size: [10, 10]     \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{8, Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10)}
		 },
		 808
	 )},
	{"type: tuple             \n"
     "elements:               \n"
     "   - char               \n"
     "   - type: array        \n"
     "     subtype: int64     \n"
     "     size: [10, 10]     \n"
     "     start: [2, 3]      \n"
     "     subsize: [6, 5]    \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{
				 8,
				 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10, 3, 5), 10, 2, 6)
			 }
		 },
		 808
	 )},
	{"type: tuple                   \n"
     "elements:                     \n"
     "   - char                     \n"
     "   - type: struct             \n"
     "     members:                 \n"
     "       - my_char:             \n"
     "           type: char         \n"
     "       - my_array:            \n"
     "           type: array        \n"
     "           subtype: int64     \n"
     "           size: [10, 10]     \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{
				 8,
				 Record_datatype::make(
					 vector<Record_datatype::Member>{
						 Record_datatype::Member{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), "my_char"},
						 Record_datatype::Member{
							 8,
							 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10),
							 "my_array"
						 }
					 },
					 808
				 )
			 }
		 },
		 816
	 )},
	{"type: tuple                     \n"
     "elements:                       \n"
     "   - char                       \n"
     "   - type: tuple              \n"
     "     elements:                \n"
     "       - type: char           \n"
     "       - type: array          \n"
     "         subtype: int64       \n"
     "         size: [10, 10]       \n",
     Tuple_datatype::make(
		 vector<Tuple_datatype::Element>{
			 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			 Tuple_datatype::Element{
				 8,
				 Tuple_datatype::make(
					 vector<Tuple_datatype::Element>{
						 Tuple_datatype::Element{0, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
						 Tuple_datatype::Element{
							 8,
							 Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10), 10)
						 }
					 },
					 808
				 )
			 }
		 },
		 816
	 )}
};

vector<string> invalid_data{
	"",
	"{size: [10, 20], type: array}"
	"{size: [10, 20], type: char}",
	"{size: [10, 20], start: [30, 20, 20], type: array, subtype: char}",
	"{size: [10, 20], subsize: 10, type: array, subtype: char}",
	"{subsize: 10, start: [30, 20, 20], type: array, subtype: char}",
	"{sizes: [10, 20], type: array, subtype: char}",
	"{type: record, members: {my_char: {disp: 0, type: char}, my_int: {disp: 4, type: int} }}",
	"{type: record, buffersize: 8, members: {my_char: {type: char}, my_int: {disp: 4, type: int} }}",
	"{type: tuple, buffersize: 13, elements: [char, int, double]}",
	"{type: tuple, elements: [{type: char, disp: 0}, {type: int, disp: 4}, {type: double, disp: 8}]}",
	"{type: tuple, elements: [{type: char, disp: 0}, {type: int}, {type: double, disp: 8}]}"
};
INSTANTIATE_TEST_SUITE_P(ScalarTypes, PositiveTypeParseTest, testing::ValuesIn(scalar_types));
INSTANTIATE_TEST_SUITE_P(ArrayTypes, PositiveTypeParseTest, testing::ValuesIn(array_types));
INSTANTIATE_TEST_SUITE_P(RecordTypes, PositiveTypeParseTest, testing::ValuesIn(record_types));
INSTANTIATE_TEST_SUITE_P(StructTypes, PositiveTypeParseTest, testing::ValuesIn(struct_types));
INSTANTIATE_TEST_SUITE_P(TupleTypes, PositiveTypeParseTest, testing::ValuesIn(tuple_types));
INSTANTIATE_TEST_SUITE_P(PointerTypes, PositiveTypeParseTest, testing::ValuesIn(pointer_types));

INSTANTIATE_TEST_SUITE_P(, NegativeTypeParseTest, testing::ValuesIn(invalid_data));

