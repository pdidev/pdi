// SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

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
 * Struct prepared for CallbacksTest.
 */
struct CallbacksTest: public ::testing::Test {
	CallbacksTest()
		: test_conf{PC_parse_string("logging: trace")}
	{}

	void SetUp() override { test_context.reset(new Global_context{test_conf}); }

	Paraconf_wrapper fw;
	PC_tree_t test_conf;
	unique_ptr<Context> test_context;
};

/*
 * Name:                CallbacksTest.add_event
 *
 * Tested functions:    PDI::Context::callbacks().add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event.
 *
 */
TEST_F(CallbacksTest, add_event)
{
	int x = 0;
	this->test_context->callbacks().add_event_callback([&x](const std::string&) { x += 42; }, "event");
	ASSERT_EQ(x, 0);
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
	this->test_context->event("event");
	ASSERT_EQ(x, 84);
}

/*
 * Name:                CallbacksTest.remove_event
 *
 * Tested functions:    PDI::Context::callbacks().add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event
 *                      and removes it.
 *
 */
TEST_F(CallbacksTest, remove_event)
{
	int x = 0;
	auto erase_f = this->test_context->callbacks().add_event_callback([&x](const std::string&) { x += 42; }, "event");
	ASSERT_EQ(x, 0);
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
	erase_f();
	this->test_context->event("event");
	ASSERT_EQ(x, 42);
}

/*
 * Name:                CallbacksTest.add_remove_event
 *
 * Tested functions:    PDI::Context::callbacks().add_event_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on event
 *                      and removes it several times.
 *
 */
TEST_F(CallbacksTest, add_remove_event)
{
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->callbacks().add_event_callback([&x](const std::string&) { x += 42; }, "event_x");
	auto erase_y = this->test_context->callbacks().add_event_callback([&y](const std::string&) { y += 53; }, "event_y");
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
	auto erase_x_2 = this->test_context->callbacks().add_event_callback([&x](const std::string&) { x += 42; }, "event_x_2");
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
 * Name:                CallbacksTest.callbacks().add_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data share.
 *
 */
TEST_F(CallbacksTest, add_data_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->callbacks().add_data_callback([](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.add_named_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(CallbacksTest, add_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	this->test_context->callbacks().add_data_callback(
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
 * Name:                CallbacksTest.remove_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it.
 */
TEST_F(CallbacksTest, remove_data_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto erase_x = this->test_context->callbacks().add_data_callback([](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.remove_named_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on data share.
 *
 */
TEST_F(CallbacksTest, remove_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->callbacks().add_data_callback(
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
 * Name:                CallbacksTest.add_remove_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(CallbacksTest, add_remove_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->callbacks().add_data_callback([](const std::string& name, Ref ref) {
		Ref_w ref_write{ref};
		int* x = static_cast<int*>(ref_write.get());
		*x += std::stoi(name);
	});
	auto erase_y = this->test_context->callbacks().add_data_callback([](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.add_remove_named_data_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_data_callback
 *
 *
 * Description:         Checks if named callback is
 *                      correctly called on share
 *                      and removes it several times.
 *
 */
TEST_F(CallbacksTest, add_remove_named_data_callback)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	Data_descriptor& desc_x = this->test_context->desc(data_x);
	Data_descriptor& desc_y = this->test_context->desc(data_y);
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	auto erase_x = this->test_context->callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	auto erase_y = this->test_context->callbacks().add_data_callback(
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
 * Name:                CallbacksTest.add_empty_desc_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_empty_desc_access_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access.
 */
TEST_F(CallbacksTest, add_empty_desc_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->callbacks().add_empty_desc_access_callback([this](const std::string& name) {
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
 * Name:                CallbacksTest.remove_empty_desc_callback
 *
 * Tested functions:    PDI::Context::callbacks().add_empty_desc_access_callback
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on empty desc access
 *                      and removes it.
 */
TEST_F(CallbacksTest, remove_empty_desc_callback)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	auto erase_x = this->test_context->callbacks().add_empty_desc_access_callback([this](const std::string& name) {
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
 * Name:                CallbacksTest.callbacks().add_data_remove_callback_reclaim
 *
 * Tested functions:    PDI::Context::callbacks().add_data_remove_callback_reclaim
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(CallbacksTest, add_data_remove_callback_reclaim)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->callbacks().add_data_remove_callback([](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.callbacks().add_data_remove_callback_release
 *
 * Tested functions:    PDI::Context::callbacks().add_data_remove_callback_release
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(CallbacksTest, add_data_remove_callback_release)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	this->test_context->callbacks().add_data_remove_callback([&x](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.callbacks().add_named_data_remove_callback_reclaim
 *
 * Tested functions:    PDI::Context::callbacks().add_named_data_remove_callback_reclaim
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(CallbacksTest, add_named_data_remove_callback_reclaim)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;
	this->test_context->callbacks().add_data_remove_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	this->test_context->callbacks().add_data_remove_callback(
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
 * Name:                CallbacksTest.callbacks().add_named_data_remove_callback_release
 *
 * Tested functions:    PDI::Context::callbacks().add_named_data_remove_callback_release
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(CallbacksTest, add_named_data_remove_callback_release)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	this->test_context->desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	void* memory_to_free = malloc(sizeof(int));
	int y = 0;
	this->test_context->callbacks().add_data_remove_callback(
		[&x](const std::string& name, Ref ref) {
			x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);
	this->test_context->callbacks().add_data_remove_callback(
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
 * Name:                CallbacksTest.callbacks().add_data_remove_callback_reclaim_remove
 *
 * Tested functions:    PDI::Context::callbacks().add_data_remove_callback_reclaim_remove
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data reclaim.
 *
 */
TEST_F(CallbacksTest, add_data_remove_callback_reclaim_remove)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto remove_callback = this->test_context->callbacks().add_data_remove_callback([](const std::string& name, Ref ref) {
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
 * Name:                CallbacksTest.callbacks().add_data_remove_callback_release_remove
 *
 * Tested functions:    PDI::Context::callbacks().add_data_remove_callback_release_remove
 *
 *
 * Description:         Checks if callback is
 *                      correctly called on data release.
 *
 */
TEST_F(CallbacksTest, add_data_remove_callback_release_remove)
{
	string data_x{"data_x"};
	this->test_context->desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	auto remove_callback = this->test_context->callbacks().add_data_remove_callback([&x](const std::string& name, Ref ref) {
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
