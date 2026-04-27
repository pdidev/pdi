/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <pdi/array_datatype.h>
#include <pdi/data_descriptor.h>
#include <pdi/delayed_data_callbacks.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>

#include <data_descriptor_impl.h>

#include "global_context.h"

using namespace PDI;
using namespace std;

/*
 * Struct prepared for DataDescDelayed
 */
struct DataDescDelayed: public ::testing::Test {
	Paraconf_wrapper fw;
	Global_context context{PC_parse_string("logging: trace")};
};

/*
 * Name:                DataDescDelayed.multiple_data_callbacks
 *
 * Description:         TODO: Jacques
 *
 */
TEST_F(DataDescDelayed, multiple_delayed_data_callbacks)
{
	string data_x{"data_x"};
	string data_y{"data_y"};
	Data_descriptor& desc_x = context.desc(data_x);
	Data_descriptor& desc_y = context.desc(data_y);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	context.desc(data_y).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;
	int y = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	context.callbacks().add_data_callback(
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

	Delayed_data_callbacks delayed_callbacks(context);
	context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));
	context.desc("data_y").share(&y, true, true, std::move(delayed_callbacks));
	ASSERT_EQ(x, 0);
	ASSERT_EQ(y, 0);

	delayed_callbacks.trigger();

	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 53);

	context.desc("data_y").reclaim();
	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
	ASSERT_EQ(y, 53);
}

/*
 * Name:                DataDescDelayed.test_scope_guard
 *
 * Tested functions:    ~Delayed_data_callbacks()
 * 
 * Description:         Check the destructor is called at the end of the scope
 *
 */
TEST_F(DataDescDelayed, test_scope_guard)
{
	string data_x{"data_x"};
	Data_descriptor& desc_x = context.desc(data_x);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	ASSERT_EQ(x, 0);
	{
		Delayed_data_callbacks delayed_callbacks(context);
		context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));
		ASSERT_EQ(x, 0);
	}
	// check that delayed_callbacks.trigger() is called in dtor of delayed_callbacks
	ASSERT_EQ(x, 42);

	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 42);
}

/*
 * Name:                DataDescDelayed.test_scope_guard_2_callback
 *
 * Tested functions:    ~Delayed_data_callbacks()
 *
 * Description:         Check the destructor is called at the end of the scope with two callbacks
 *
 */
TEST_F(DataDescDelayed, test_scope_guard_2_callback)
{
	string data_x{"data_x"};
	Data_descriptor& desc_x = context.desc(data_x);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	ASSERT_EQ(x, 0);
	{
		Delayed_data_callbacks delayed_callbacks(context);
		context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));

		context.callbacks().add_data_callback(
			[](const std::string& name, Ref ref) {
				Ref_w ref_write{ref};
				int* x = static_cast<int*>(ref_write.get());
				*x += 10;
				ASSERT_STREQ(name.c_str(), "data_x");
			},
			"data_x"
		);

		ASSERT_EQ(x, 0);
	}
	// check that delayed_callbacks.trigger() is called in the end of dtor of delayed_callbacks
	ASSERT_EQ(x, 52);

	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 52);
}

/*
 * Name:                DataDescDelayed.reclaim_before_trigger
 *
 * Description:         Check the behavior of trigger when a data name is not shared
 *
 */
TEST_F(DataDescDelayed, reclaim_before_trigger)
{
	string data_x{"data_x"};
	Data_descriptor& desc_x = context.desc(data_x);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	ASSERT_EQ(x, 0);

	Delayed_data_callbacks delayed_callbacks(context);
	context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));
	ASSERT_EQ(x, 0);

	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 0);


	// Check the error message is trigger is called before reclaim
	try {
		delayed_callbacks.trigger();
	} catch (Error& e) {
		ASSERT_STREQ("In triggering on_data for `1' data, Cannot access a non shared value: `data_x'", e.what());
	} catch (...) {
		FAIL();
	}
}

/*
 * Name:                DataDescDelayed.reclaim_before_trigger
 *
 * Description:         Check the behavior of trigger when a data name is defined twice.
 *
 */
TEST_F(DataDescDelayed, same_name_added)
{
	string data_x{"data_x"};
	Data_descriptor& desc_x = context.desc(data_x);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	ASSERT_EQ(x, 0);

	Delayed_data_callbacks delayed_callbacks(context);
	context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));
	delayed_callbacks.add_dataname("data_x"); // the list of data to trigger contains two times "data_x"

	delayed_callbacks.trigger();
	ASSERT_EQ(x, 84); // the callback on "data_x" is called twice
	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 84);
}

/*
 * Name:                DataDescDelayed.two_trigger_calls
 *
 * Description:         Check the second call of trigger doesn't change the data
 *
 */
TEST_F(DataDescDelayed, two_trigger_calls)
{
	string data_x{"data_x"};
	Data_descriptor& desc_x = context.desc(data_x);
	context.desc(data_x).default_type(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));
	int x = 0;

	context.callbacks().add_data_callback(
		[](const std::string& name, Ref ref) {
			Ref_w ref_write{ref};
			int* x = static_cast<int*>(ref_write.get());
			*x += 42;
			ASSERT_STREQ(name.c_str(), "data_x");
		},
		"data_x"
	);

	ASSERT_EQ(x, 0);
	{
		Delayed_data_callbacks delayed_callbacks(context);
		context.desc("data_x").share(&x, true, true, std::move(delayed_callbacks));
		ASSERT_EQ(x, 0);

		delayed_callbacks.trigger();
		ASSERT_EQ(x, 42);
	} // the second call of trigger at the end of this scope
	context.desc("data_x").reclaim();
	ASSERT_EQ(x, 42); // Check the list of data in second trigger is empty
}
