/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>

#include <data_descriptor_impl.h>

#include "global_context.h"

using namespace PDI;
using namespace std;

namespace PDI {
//handler to private fields of Descriptor
struct Descriptor_test_handler {
	static unique_ptr<Data_descriptor> default_desc(Global_context& global_ctx)
	{
		return unique_ptr<Data_descriptor> {new Data_descriptor_impl{global_ctx, "default_desc"}};
	}
	
	static Datatype_sptr desc_get_type(unique_ptr<Data_descriptor>& desc, Global_context& global_ctx)
	{
		Datatype_template_ptr desc_template = dynamic_cast<Data_descriptor_impl*>(desc.get())->m_type;
		return desc_template->evaluate(global_ctx);
	}
	
	static int desc_get_refs_number(unique_ptr<Data_descriptor>& desc)
	{
		return dynamic_cast<Data_descriptor_impl*>(desc.get())->m_refs.size();
	}
};
}

/*
 * Struct prepared for DataDescTest
 */
struct DataDescTest : public ::testing::Test {
	int array[10];
	PC_tree_t array_config {PC_parse_string("{ size: 10, type: array, subtype: int }")};
	shared_ptr<Array_datatype> array_datatype{Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10)};
	PDI::Paraconf_wrapper fw;
	Global_context global_ctx{PC_parse_string("")};
	unique_ptr<Data_descriptor> m_desc_default = Descriptor_test_handler::default_desc(global_ctx);
};

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
		ASSERT_EQ(PDI_status_t::PDI_ERR_RIGHT, err.status());
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
		ASSERT_EQ(PDI_status_t::PDI_ERR_RIGHT, err.status());
	}
	this->m_desc_default->release();
	
	void* ptr = this->m_desc_default->share(this->m_desc_default->ref(), false, true);
	ASSERT_EQ(this->array, ptr);
	try {
		this->m_desc_default->share(this->m_desc_default->ref(), true, false);
		FAIL();
	} catch (const Error& err) {
		ASSERT_EQ(PDI_status_t::PDI_ERR_RIGHT, err.status());
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
		ASSERT_EQ(PDI_status_t::PDI_ERR_RIGHT, err.status());
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
