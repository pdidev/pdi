/*******************************************************************************
 * Copyright (C) 2018-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <gmock/gmock.h>

#include <pdi/array_datatype.h>
#include <pdi/scalar_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/pdi_fwd.h>

#include "mocks/datatype_mock.h"

using namespace PDI;
using namespace std;

using ::testing::Return;

/*
 * Struct prepared for DataRefAnyTest.
 */
struct DataRefAnyTest : public ::testing::Test, Reference_base {
	DataRefAnyTest():
		m_data {new int[1024]}
	{
		for (int i = 0; i < 1024; i++) {
			m_data[i] = i;
		}
		
		Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
		Array_datatype array_type {int_type.clone_type(), 32};
		m_tested_ref = unique_ptr<Ref> {new Ref{m_data.get(), [](void* d){*static_cast<int*>(d)=-1;},
			array_type.clone_type(), true, true}
		};
	}
	
	unique_ptr<int[]> m_data;
	
	unique_ptr<Ref> m_tested_ref;
};

/*
 * Name:                DataRefAnyTest.newReferenceConstructor
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 *
 * Description:         Test checks if reference was correctly created.
 */
TEST_F(DataRefAnyTest, newReferenceConstructor)
{
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(1, Reference_base::get_content(*this->m_tested_ref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(*this->m_tested_ref)->m_buffer->m_owners);
	ASSERT_EQ(0, Reference_base::get_content(*this->m_tested_ref)->m_buffer->m_read_locks);
	ASSERT_EQ(0, Reference_base::get_content(*this->m_tested_ref)->m_buffer->m_write_locks);
}

/*
 * Name:                DataRefAnyTest.copyConstructor
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref& )
 *
 * Description:         Test checks if reference copy is correctly
 *                      created and ref properties updated.
 */
TEST_F(DataRefAnyTest, copyConstructor)
{
	Ref copied_ref(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref);
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, copied_ref);
	ASSERT_EQ(2, Reference_base::get_content(copied_ref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(copied_ref)->m_buffer->m_owners);
	ASSERT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	ASSERT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);
}

/*
 * Name:                DataRefAnyTest.moveConstructor
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref&& )
 *
 * Description:         Test checks if reference move is correctly
 *                      handled and ref properties updated.
 */
TEST_F(DataRefAnyTest, moveConstructor)
{
	Ref moved_ref(move(*this->m_tested_ref));
	ASSERT_TRUE(moved_ref);
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_EQ(nullptr, Reference_base::get_content(*this->m_tested_ref));
	ASSERT_EQ(1, Reference_base::get_content(moved_ref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(moved_ref)->m_buffer->m_owners);
	ASSERT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_read_locks);
	ASSERT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_write_locks);
}

/*
 * Name:                DataRefAnyTest.unlinkToDestroy
 *
 * Tested functions:    PDI::Ref_any::reset()
 *                      PDI::Ref_any::unlink()
 *
 * Description:         Test checks if destructor is called
 *                      on reset().
 */
TEST_F(DataRefAnyTest, unlinkToDestroy)
{
	this->m_tested_ref->reset();
	ASSERT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.unlinkButNotDestroy
 *
 * Tested functions:    PDI::Ref_any::reset()
 *                      PDI::Ref_any::unlink()
 *
 * Description:         Test checks if destructor isn't called
 *                      on reset() if more than 1 owner.
 */
TEST_F(DataRefAnyTest, unlinkButNotDestroy)
{
	Ref copied_ref(*this->m_tested_ref);
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, copied_ref);
	ASSERT_EQ(2, Reference_base::get_content(copied_ref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(copied_ref)->m_buffer->m_owners);
	
	this->m_tested_ref->reset();
	
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref);
	ASSERT_EQ(this->m_data[0], 0);
	ASSERT_EQ(1, Reference_base::get_content(copied_ref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(copied_ref)->m_buffer->m_owners);
	ASSERT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	ASSERT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);
	
	copied_ref.reset();
	
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_FALSE(copied_ref);
	ASSERT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.getAccess
 *
 * Tested functions:    PDI::Ref_any::reset()
 *                      PDI::Ref_any::unlink()
 *
 * Description:         Test checks if correct address is
 *                      returned from get().
 */
TEST_F(DataRefAnyTest, getAccess)
{
	Ref_r copied_ref_r(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref_r);
	const void* ptr_r = copied_ref_r.get();
	ASSERT_EQ(this->m_data.get(), ptr_r);
	
	copied_ref_r.reset();
	
	Ref_rw copied_ref_rw(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref_rw);
	void* ptr_rw = copied_ref_rw.get();
	ASSERT_EQ(this->m_data.get(), ptr_rw);
}

/*
 * Name:                DataRefAnyTest.releaseTest
 *
 * Tested functions:    PDI::Ref_any::release()
 *
 * Description:         Test checks if release returns correct address
 *                      and destructor isn't called.
 */
TEST_F(DataRefAnyTest, releaseTest)
{
	ASSERT_EQ(this->m_data.get(), this->m_tested_ref->release());
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_EQ(this->m_data[0], 0);
}

/*
 * Name:                DataRefAnyTest.nullifyTest
 *
 * Tested functions:    PDI::Ref_any::on_nullify()
 *
 * Description:         Test checks if the function passed in on_nullify
 *                      is called on realese by different owner.
 */
TEST_F(DataRefAnyTest, nullifyTest)
{
	//the address to replace
	char c;
	void* address = &c;
	this->m_tested_ref->on_nullify([address](Ref whoCalled) {
		Reference_base::get_content(whoCalled)->m_data = address;
	});
	Ref otherRef(*this->m_tested_ref);
	void* recvAddress = otherRef.release();
	ASSERT_EQ(address, recvAddress);
	ASSERT_EQ(this->m_data[0], 0);
}

/*
 * Name:                DataRefAnyTest.get_content
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, get_content)
{
	Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
	
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 1);
	
	Ref_r sub {*this->m_tested_ref, Array_datatype::Index_accessor{4}};
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 2);
	ASSERT_NE(get_content(sub), get_content(*this->m_tested_ref));
	ASSERT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 2);
	
	
	this->m_tested_ref->reset();
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 1);
	
	ASSERT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	ASSERT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.content_chain
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, content_chain)
{
	Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
	Array_datatype subarray {int_type.clone_type(), 4};
	
	Ref_r sub_array_ref {*this->m_tested_ref, Array_datatype::Slice_accessor{16, 20}}; // array [16:20]
	Ref_r sub_scalar_ref {sub_array_ref, Array_datatype::Index_accessor{0}}; // array[16]
	
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_EQ(get_content(sub_array_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(sub_array_ref)->m_owners, 1);
	ASSERT_EQ(get_content(sub_scalar_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(sub_scalar_ref)->m_owners, 1);
	
	ASSERT_EQ(get_content(sub_array_ref)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	ASSERT_EQ(get_content(sub_array_ref)->m_buffer, get_content(sub_scalar_ref)->m_buffer);
	
	ASSERT_NE(get_content(sub_array_ref), get_content(*this->m_tested_ref));
	ASSERT_NE(get_content(sub_array_ref), get_content(sub_scalar_ref));
	
	this->m_tested_ref->reset();
	ASSERT_EQ(get_content(sub_array_ref)->m_buffer->m_owners, 2);
	
	ASSERT_EQ(16, static_cast<const int*>(sub_array_ref.get())[0]);
	ASSERT_EQ(17, static_cast<const int*>(sub_array_ref.get())[1]);
	ASSERT_EQ(18, static_cast<const int*>(sub_array_ref.get())[2]);
	ASSERT_EQ(19, static_cast<const int*>(sub_array_ref.get())[3]);
	sub_array_ref.reset();
	
	ASSERT_EQ(16, *static_cast<const int*>(sub_scalar_ref.get()));
	ASSERT_EQ(get_content(sub_scalar_ref)->m_buffer->m_owners, 1);
	
	sub_scalar_ref.reset();
	ASSERT_EQ(-1, this->m_data[0]);
}

/*
 * Name:                DataRefAnyTest.content_record
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, content_record)
{
	Scalar_datatype char_type {Scalar_kind::SIGNED, sizeof(char)};
	struct Record {
		char x;
		int y[32];
	};
	
	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record, x), char_type.clone_type(), "x");
	members.emplace_back(offsetof(Record, y), this->m_tested_ref->type().clone_type(), "y");
	Record_datatype record_type {std::move(members), sizeof(Record)};
	
	Record data;
	data.x = 42;
	for (int i = 0; i < 32; i++) {
		data.y[i] = i;
	}
	
	Ref base_ref {&data, [](void* p){static_cast<Record*>(p)->x = -1;}, record_type.clone_type(), true, true};
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 1);

	{
		std::vector<std::unique_ptr<Datatype::Accessor_base>> accessors;
		accessors.emplace_back(new Record_datatype::Member_accessor{"y"});
		accessors.emplace_back(new Array_datatype::Index_accessor{12});
		Ref_r result {base_ref, accessors};
		ASSERT_EQ(12, *static_cast<const int*>(result.get()));
	}
	
	Ref_r data_x_ref{base_ref, Record_datatype::Member_accessor{"x"}}; // data.x
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	
	Ref_r data_y_ref {base_ref, Record_datatype::Member_accessor{"y"}}; // data.y
	for (int i = 0; i < 32; i++) {
		ASSERT_EQ(i, static_cast<const int*>(data_y_ref.get())[i]);
	}
	
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	
	Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
	
	Ref data_y_scalar_ref {data_y_ref, Array_datatype::Index_accessor{12}};
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 1);
	
	if (Ref_w failed {data_y_scalar_ref}) {
		FAIL();
	}
	if (Ref_w failed {base_ref}) {
		FAIL();
	}
	
	Ref_r data_y_scalar_ref_r {data_y_scalar_ref};
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	ASSERT_EQ(12, *static_cast<const int*>(data_y_scalar_ref_r.get()));
	
	data_y_ref.reset();
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	ASSERT_EQ(42, *static_cast<const char*>(data_x_ref.get()));
	
	data_x_ref.reset();
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	
	base_ref.reset();
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	
	data_y_scalar_ref.reset();
	ASSERT_EQ(get_content(data_y_scalar_ref_r)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref_r)->m_owners, 1);
	ASSERT_EQ(42, data.x);
	
	data_y_scalar_ref_r.reset();
	
	ASSERT_EQ(-1, data.x);
}

/*
 * Name:                DataRefAnyTest.content_deep_copy
 *
 * Tested functions:    PDI::Ref_any::do_copy()
 */
TEST_F(DataRefAnyTest, content_deep_copy)
{
	Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
	
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	
	Ref_r sub {*this->m_tested_ref, Array_datatype::Index_accessor{4}};
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_NE(get_content(sub), get_content(*this->m_tested_ref));
	ASSERT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	
	Ref_r copied = do_copy(sub);
	ASSERT_EQ(copied.type().buffersize(), sub.type().buffersize());
	ASSERT_NE(copied.get(), sub.get());
	ASSERT_EQ(*static_cast<const int*>(copied.get()), *static_cast<const int*>(sub.get()));
	
	this->m_tested_ref->reset();
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	
	ASSERT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	ASSERT_EQ(this->m_data[0], -1);
	
	ASSERT_EQ(4, *static_cast<const int*>(copied.get()));
}

/*
 * Name:                DataRefAnyTest.index_access
 *
 * Tested functions:    PDI::Ref_any::operator[](size_t)
 */
TEST_F(DataRefAnyTest, ref_index_access)
{
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 1);
	
	Ref_r sub = (*m_tested_ref)[4];
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_owners, 1);
	ASSERT_EQ(get_content(*this->m_tested_ref)->m_buffer->m_owners, 2);
	ASSERT_NE(get_content(sub), get_content(*this->m_tested_ref));
	ASSERT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 2);
	
	
	this->m_tested_ref->reset();
	ASSERT_EQ(get_content(sub)->m_owners, 1);
	ASSERT_EQ(get_content(sub)->m_buffer->m_owners, 1);
	
	ASSERT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	ASSERT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.member_access
 *
 * Tested functions:    PDI::Ref_any::operator[](std::string)
 */
TEST_F(DataRefAnyTest, ref_member_access)
{
	Scalar_datatype char_type {Scalar_kind::SIGNED, sizeof(char)};
	struct Record {
		char x;
		int y[32];
	};
	
	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record, x), char_type.clone_type(), "x");
	members.emplace_back(offsetof(Record, y), this->m_tested_ref->type().clone_type(), "y");
	Record_datatype record_type {std::move(members), sizeof(Record)};
	
	Record data;
	data.x = 42;
	for (int i = 0; i < 32; i++) {
		data.y[i] = i;
	}
	
	Ref base_ref {&data, [](void* p){static_cast<Record*>(p)->x = -1;}, record_type.clone_type(), true, true};
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 1);
	
	Ref_r data_x_ref = base_ref["x"];
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	
	Ref_r data_y_ref = base_ref["y"];
	for (int i = 0; i < 32; i++) {
		ASSERT_EQ(i, static_cast<const int*>(data_y_ref.get())[i]);
	}
	
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	
	Scalar_datatype int_type {Scalar_kind::SIGNED, sizeof(int)};
	
	Ref data_y_scalar_ref = data_y_ref[12];
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 1);
	
	if (Ref_w failed {data_y_scalar_ref}) {
		FAIL();
	}
	if (Ref_w failed {base_ref}) {
		FAIL();
	}
	
	Ref_r data_y_scalar_ref_r {data_y_scalar_ref};
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 4);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	ASSERT_EQ(12, *static_cast<const int*>(data_y_scalar_ref_r.get()));
	
	data_y_ref.reset();
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_x_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_x_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 3);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	ASSERT_EQ(42, *static_cast<const char*>(data_x_ref.get()));
	
	data_x_ref.reset();
	ASSERT_EQ(get_content(base_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(base_ref)->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 2);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	
	base_ref.reset();
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref)->m_owners, 2);
	
	data_y_scalar_ref.reset();
	ASSERT_EQ(get_content(data_y_scalar_ref_r)->m_buffer->m_owners, 1);
	ASSERT_EQ(get_content(data_y_scalar_ref_r)->m_owners, 1);
	ASSERT_EQ(42, data.x);
	
	data_y_scalar_ref_r.reset();
	
	ASSERT_EQ(-1, data.x);
}

/*
 * Name:                DataRefAnyTest.wrong_index_access
 *
 * Tested functions:    PDI::Ref_any::operator[]
 */
TEST_F(DataRefAnyTest, wrong_index_access)
{
	try {
	(*m_tested_ref)["example"];
		FAIL();
	} catch (const Type_error& e) {}
	try {
		Ref sub = (*m_tested_ref)[4];
		sub["example"];
		FAIL();
	} catch (const Type_error& e) {}
	try {
		Ref sub = (*m_tested_ref)[4];
		sub[4];
		FAIL();
	} catch (const Type_error& e) {}
	try {
		Scalar_datatype char_type {Scalar_kind::SIGNED, sizeof(char)};
		struct Record {
			char x;
			int y[32];
		};
		
		std::vector<Record_datatype::Member> members;
		members.emplace_back(offsetof(Record, x), char_type.clone_type(), "x");
		members.emplace_back(offsetof(Record, y), this->m_tested_ref->type().clone_type(), "y");
		Record_datatype record_type {std::move(members), sizeof(Record)};
		
		Record data;
		Ref base_ref {&data, [](void* p){static_cast<Record*>(p)->x = -1;}, record_type.clone_type(), true, true};
		base_ref[4];
		FAIL();
	} catch (const Type_error& e) {}
}


/*
 * Struct prepared for DataRefAnyTypedTest.
 */
template<typename T>
struct DataRefAnyTypedTest : public DataRefAnyTest, T {
	DataRefAnyTypedTest() : DataRefAnyTest()
	{
		if (is_same<T, Ref>::value) {
			locks = 0;
		} else if (is_same<T, Ref_r>::value) {
			locks = 1;
		} else if (is_same<T, Ref_w>::value) {
			locks = 2;
		} else { //is_same<T, Ref_rw>::value
			locks = 3;
		}
	}
	char locks;
};

typedef ::testing::Types<Ref, Ref_r, Ref_w, Ref_rw> RefTypes;
TYPED_TEST_CASE(DataRefAnyTypedTest, RefTypes);

/*
 * Name:                DataRefAnyTypedTest.NullRef
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 *
 * Description:         Test checks if default Ref is nullptr.
 */
TYPED_TEST(DataRefAnyTypedTest, NullRef)
{
	TypeParam ref;
	ASSERT_FALSE(ref);
}

/*
 * Name:                DataRefAnyTypedTest.PossibleNullRefCopy
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref& )
 *
 * Description:         Test checks if default Ref copy is nullptr.
 */
TYPED_TEST(DataRefAnyTypedTest, PossibleNullRefCopy)
{
	TypeParam ref;
	TypeParam ref_copy(ref);
	ASSERT_FALSE(ref_copy);
}

/*
 * Name:                DataRefAnyTypedTest.PossibleNullRefMove
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref&& )
 *
 * Description:         Test checks if default moved Ref is nullptr.
 */
TYPED_TEST(DataRefAnyTypedTest, PossibleNullRefMove)
{
	TypeParam ref;
	TypeParam ref_moved(std::move(ref));
	ASSERT_FALSE(ref_moved);
}

/*
 * Name:                DataRefAnyTypedTest.chmodConstructor
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref& )
 *
 * Description:         Test checks if correct copy is created
 *                      and ref properties updated.
 */
TYPED_TEST(DataRefAnyTypedTest, chmodConstructor)
{
	//copies a reference with different privileges
	TypeParam chref(*this->m_tested_ref);
	
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, chref);
	ASSERT_EQ(2, Reference_base::get_content(chref)->m_owners);
	ASSERT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_owners);
	
	if (this->locks & 2) {
		//if granted with write access
		ASSERT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_read_locks);
		ASSERT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_write_locks);
	} else {
		//if not granted with write access
		if (this->locks & 1) {
			//if granted with read access
			ASSERT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_read_locks);
			ASSERT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_write_locks);
		} else {
			//if not granted with read access
			ASSERT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_read_locks);
			ASSERT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_write_locks);
		}
	}
}


/*
 * Struct prepared for DenseArrayRefAnyTest.
 */
struct DenseArrayRefAnyTest : public DataRefAnyTest {
	DenseArrayRefAnyTest() : DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();
		
		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}
		
		DataRefAnyTest::m_tested_ref = unique_ptr<Ref> {new Ref{array_to_share, [](void*){},
			datatype->clone_type(), true, true}
		};
	}
	
	Datatype_uptr datatype {
		new Array_datatype
		{
			Datatype_uptr {
				new Array_datatype
				{
					Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
					10
				}
			},
			10
		}
	};
	
	int array_to_share[100]; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)
};

/*
 * Name:                DenseArrayRefAnyTest.checkDeepCopy
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref& )
 *                      PDI::Ref_any::copy()
 *
 * Description:         Test checks if correct deep copy is created
 *                      on dense array.
 */
TEST_F(DenseArrayRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	ASSERT_TRUE(changed_ref);
	
	Ref_rw cloned_ref(changed_ref.copy());
	int* cloned_array = static_cast<int*>(cloned_ref.get());
	for (int i = 0; i < 100; i++) {
		ASSERT_EQ(this->array_to_share[i], cloned_array[i]);
	}
}

/*
 * Struct prepared for SparseArrayRefAnyTest.
 *
 */
struct SparseArrayRefAnyTest : public DataRefAnyTest {
	int* array_to_share {new int[100]}; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)
	
	SparseArrayRefAnyTest() : DataRefAnyTest()
	{
		m_tested_ref->reset();
		
		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}
		
		m_tested_ref = unique_ptr<Ref> {new Ref{array_to_share, [](void* ptr){operator delete[] (ptr);}, datatype->clone_type(), true, true}};
	}
	
	Datatype_uptr datatype {
		new Array_datatype
		{
			Datatype_uptr {
				new Array_datatype
				{
					Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
					10,
					3,
					4
				}
			},
			10,
			3,
			4
		}
	};
};

/*
 * Name:                SparseArrayRefAnyTest.checkDeepCopy
 *
 * Tested functions:    PDI::Ref_any::Ref_any(Ref& )
 *                      PDI::Ref_any::copy()
 *
 * Description:         Test checks if correct deep copy is created
 *                      on sparse array.
 */
TEST_F(SparseArrayRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	ASSERT_TRUE(changed_ref);
	
	Ref_r cloned_ref(changed_ref.copy());
	const int* cloned_array = static_cast<const int*>(cloned_ref.get());
	for (int i = 0; i < 16; i++) {
		std::cerr << i << ": " << cloned_array[i] << std::endl;
		ASSERT_EQ(this->array_to_share[(i/4 + 3)*10 + (i%4)+3], cloned_array[i]);
	}
}

/*
 * Struct prepared for DenseRecordRefAnyTest.
 */
struct DenseRecordRefAnyTest : public DataRefAnyTest {
	struct Struct_def {
		char char_array[25];    //disp = 0
		long long_scalar;       //disp = 32
		int int_scalar;         //disp = 40
	}; //sizeof = 48
	
	Struct_def* record_to_share = new Struct_def;
	
	DenseRecordRefAnyTest() : DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();
		
		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;
		
		DataRefAnyTest::m_tested_ref = unique_ptr<Ref> {new Ref{record_to_share, [](void* ptr){operator delete (ptr);}, datatype->clone_type(), true, true}
		};
	}
	
	vector<Record_datatype::Member> get_members()
	{
		return  {
			Record_datatype::Member{
				offsetof(Struct_def, char_array),
				Datatype_uptr {
					new Array_datatype
					{
						Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(char)}},
						25
					}
				},
				"char_array"
			},
			Record_datatype::Member{
				offsetof(Struct_def, long_scalar),
				Datatype_uptr {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
				"long_scalar"
			},
			Record_datatype::Member{
				offsetof(Struct_def, int_scalar),
				Datatype_uptr {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
				"int_scalar"
			}
		};
	}
	
	Datatype_uptr datatype { new Record_datatype {get_members(), sizeof(Struct_def)} };
};

TEST_F(DenseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	ASSERT_TRUE(changed_ref);
	
	Ref_r cloned_ref(changed_ref.copy());
	const Struct_def* cloned_array = static_cast<const Struct_def*>(cloned_ref.get());
	for (int i = 0; i < 25; i++) {
		ASSERT_EQ(cloned_array->char_array[i], i);
	}
	ASSERT_EQ(cloned_array->long_scalar, 10);
	ASSERT_EQ(cloned_array->int_scalar, -10);
}

/*
 * Struct prepared for SparseRecordRefAnyTest.
 */
struct SparseRecordRefAnyTest : public DataRefAnyTest {
	struct Sparse_struct_def {
		char char_array[25];    //disp = 0 //buffer: 5 x 5; data: 3 x 3; start: (1, 1)
		long long_scalar;       //disp = 32
		int int_scalar;         //disp = 40
	}; //sizeof = 48
	
	struct Dense_struct_def {
		char char_array[9];     //disp = 0 //buffer: 3 x 3; data: 3 x 3;
		long long_scalar;       //disp = 24
		int int_scalar;         //disp = 28
	}; //sizeof = 32
	
	Sparse_struct_def* record_to_share = new Sparse_struct_def;
	
	SparseRecordRefAnyTest() : DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();
		
		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;
		
		DataRefAnyTest::m_tested_ref = unique_ptr<Ref> {new Ref{record_to_share, [](void* ptr){operator delete (ptr);}, datatype->clone_type(), true, true}
		};
	}
	
	vector<Record_datatype::Member> get_members()
	{
		return  {
			Record_datatype::Member{
				offsetof(Sparse_struct_def, char_array),
				Datatype_uptr {
					new Array_datatype
					{
						Datatype_uptr {
							new Array_datatype
							{
								Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(char)}},
								5,
								1,
								3
							}
						},
						5,
						1,
						3
					}
				},
				"char_array"
			},
			Record_datatype::Member{
				offsetof(Sparse_struct_def, long_scalar),
				Datatype_uptr {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
				"long_scalar"
			},
			Record_datatype::Member{
				offsetof(Sparse_struct_def, int_scalar),
				Datatype_uptr {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
				"int_scalar"
			}
		};
	}
	
	Datatype_uptr datatype { new Record_datatype {get_members(), sizeof(Sparse_struct_def)} };
};

TEST_F(SparseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	ASSERT_TRUE(changed_ref);
	
	Ref_r cloned_ref(changed_ref.copy());
	const Dense_struct_def* cloned_array = static_cast<const Dense_struct_def*>(cloned_ref.get());
	for (int i = 0; i < 9; i++) {
		ASSERT_EQ(cloned_array->char_array[i], (i/3+1)*5 + i%3 + 1);
	}
	ASSERT_EQ(cloned_array->long_scalar, 10);
	ASSERT_EQ(cloned_array->int_scalar, -10);
}
