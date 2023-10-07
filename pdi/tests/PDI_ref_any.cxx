/*******************************************************************************
 * Copyright (C) 2021-2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

#include "mocks/datatype_mock.h"

using namespace PDI;
using namespace std;

using ::testing::Return;

/*
 * Struct prepared for DataRefAnyTest.
 */
struct DataRefAnyTest
	: public ::testing::Test
	, Reference_base {
	DataRefAnyTest()
		: m_data(std::make_unique<int[]>(1024))
	{
		for (int i = 0; i < 1024; i++) {
			m_data[i] = i;
		}

		auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
		auto&& array_type = Array_datatype::make(int_type, 32);
		m_tested_ref = std::make_unique<Ref>(
			m_data.get(),
			[this](void*) {
				for (int i = 0; i < 1024; i++) {
					m_data[i] = -1;
				}
			},
			array_type,
			true,
			true
		);
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
	EXPECT_TRUE(*this->m_tested_ref);
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(*this->m_tested_ref)->m_buffer->m_locks);
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
	EXPECT_TRUE(copied_ref);
	EXPECT_TRUE(*this->m_tested_ref);
	EXPECT_EQ(*this->m_tested_ref, copied_ref);
	EXPECT_FALSE(!Reference_base::get_content(copied_ref));
	EXPECT_FALSE(!Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_locks);
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
	EXPECT_TRUE(moved_ref);
	EXPECT_FALSE(*this->m_tested_ref);
	EXPECT_EQ(nullptr, Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(moved_ref));
	EXPECT_FALSE(!Reference_base::get_content(moved_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_locks);
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
	EXPECT_EQ(this->m_data[0], -1);
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
	EXPECT_TRUE(*this->m_tested_ref);
	EXPECT_EQ(*this->m_tested_ref, copied_ref);
	EXPECT_FALSE(!Reference_base::get_content(copied_ref));
	EXPECT_FALSE(!Reference_base::get_content(copied_ref)->m_buffer);

	this->m_tested_ref->reset();

	EXPECT_FALSE(*this->m_tested_ref);
	EXPECT_TRUE(copied_ref);
	EXPECT_EQ(this->m_data[0], 0);
	EXPECT_FALSE(!Reference_base::get_content(copied_ref));
	EXPECT_FALSE(!Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_locks);

	copied_ref.reset();

	EXPECT_FALSE(*this->m_tested_ref);
	EXPECT_FALSE(copied_ref);
	EXPECT_EQ(this->m_data[0], -1);
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
	EXPECT_TRUE(copied_ref_r);
	const void* ptr_r = copied_ref_r.get();
	EXPECT_EQ(this->m_data.get(), ptr_r);

	copied_ref_r.reset();

	Ref_rw copied_ref_rw(*this->m_tested_ref);
	EXPECT_TRUE(copied_ref_rw);
	void* ptr_rw = copied_ref_rw.get();
	EXPECT_EQ(this->m_data.get(), ptr_rw);
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
	EXPECT_EQ(this->m_data.get(), this->m_tested_ref->release());
	EXPECT_FALSE(*this->m_tested_ref);
	EXPECT_EQ(this->m_data[0], 0);
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
	this->m_tested_ref->on_nullify([address](Ref whoCalled) { Reference_base::get_content(whoCalled)->m_data = address; });
	Ref otherRef(*this->m_tested_ref);
	void* recvAddress = otherRef.release();
	EXPECT_EQ(address, recvAddress);
	EXPECT_EQ(this->m_data[0], 0);
}

/*
 * Name:                DataRefAnyTest.get_content
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, get_content)
{
	auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));

	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);

	Ref_r sub = (*this->m_tested_ref)[4];
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_NE(get_content(sub), get_content(*this->m_tested_ref));
	EXPECT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);


	this->m_tested_ref->reset();
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);

	EXPECT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	EXPECT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.content_chain
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, content_chain)
{
	Ref_r sub_array_ref = (*this->m_tested_ref)[std::pair<size_t, size_t>(16, 20)]; // array [16:20]
	Ref_r sub_scalar_ref = sub_array_ref[0]; // array[16]

	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub_array_ref));
	EXPECT_FALSE(!get_content(sub_array_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub_scalar_ref));
	EXPECT_FALSE(!get_content(sub_scalar_ref)->m_buffer);

	EXPECT_EQ(get_content(sub_array_ref)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_EQ(get_content(sub_array_ref)->m_buffer, get_content(sub_scalar_ref)->m_buffer);

	EXPECT_NE(get_content(sub_array_ref), get_content(*this->m_tested_ref));
	EXPECT_NE(get_content(sub_array_ref), get_content(sub_scalar_ref));

	this->m_tested_ref->reset();
	EXPECT_TRUE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!get_content(sub_array_ref));
	EXPECT_FALSE(!get_content(sub_array_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub_scalar_ref));
	EXPECT_FALSE(!get_content(sub_scalar_ref)->m_buffer);

	EXPECT_EQ(16, static_cast<const int*>(sub_array_ref.get())[0]);
	EXPECT_EQ(17, static_cast<const int*>(sub_array_ref.get())[1]);
	EXPECT_EQ(18, static_cast<const int*>(sub_array_ref.get())[2]);
	EXPECT_EQ(19, static_cast<const int*>(sub_array_ref.get())[3]);

	sub_array_ref.reset();
	EXPECT_TRUE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_TRUE(!get_content(sub_array_ref));
	EXPECT_FALSE(!get_content(sub_scalar_ref));
	EXPECT_FALSE(!get_content(sub_scalar_ref)->m_buffer);

	EXPECT_EQ(16, *static_cast<const int*>(sub_scalar_ref.get()));

	sub_scalar_ref.reset();
	EXPECT_TRUE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_TRUE(!get_content(sub_array_ref));
	EXPECT_TRUE(!get_content(sub_scalar_ref));

	EXPECT_EQ(-1, this->m_data[0]);
}

/*
 * Name:                DataRefAnyTest.content_record
 *
 * Tested functions:    PDI::Ref_any::Ref_any()
 */
TEST_F(DataRefAnyTest, content_record)
{
	auto&& char_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(char));

	struct Record {
		char x;
		int y[32];
	};

	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record, x), char_type, "x");
	members.emplace_back(offsetof(Record, y), this->m_tested_ref->type(), "y");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Record));

	Record data;
	data.x = 42;
	for (int i = 0; i < 32; i++) {
		data.y[i] = i;
	}

	Ref base_ref{&data, [](void* p) { static_cast<Record*>(p)->x = -1; }, record_type, true, true};
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);

	{
		Ref_r result = base_ref["y"][12];
		EXPECT_EQ(12, *static_cast<const int*>(result.get()));
	}

	Ref_r data_x_ref = base_ref["x"]; // data.x
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);

	Ref_r data_y_ref = base_ref["y"]; // data.y
	for (int i = 0; i < 32; i++) {
		EXPECT_EQ(i, static_cast<const int*>(data_y_ref.get())[i]);
	}

	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);

	auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));

	Ref data_y_scalar_ref = data_y_ref[12];
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	if (Ref_w failed{data_y_scalar_ref}) {
		FAIL();
	}
	if (Ref_w failed{base_ref}) {
		FAIL();
	}

	Ref_r data_y_scalar_ref_r{data_y_scalar_ref};
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);
	EXPECT_EQ(12, *static_cast<const int*>(data_y_scalar_ref_r.get()));

	data_y_ref.reset();
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);
	EXPECT_EQ(42, *static_cast<const char*>(data_x_ref.get()));

	data_x_ref.reset();
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	base_ref.reset();
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	data_y_scalar_ref.reset();
	EXPECT_FALSE(!get_content(data_y_scalar_ref_r));
	EXPECT_FALSE(!get_content(data_y_scalar_ref_r)->m_buffer);
	EXPECT_EQ(42, data.x);

	data_y_scalar_ref_r.reset();

	EXPECT_EQ(-1, data.x);
}

/*
 * Name:                DataRefAnyTest.content_deep_copy
 *
 * Tested functions:    PDI::Ref_any::do_copy()
 */
TEST_F(DataRefAnyTest, content_deep_copy)
{
	auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));

	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);

	Ref_r sub = (*this->m_tested_ref)[4];
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_NE(get_content(sub), get_content(*this->m_tested_ref));
	EXPECT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);

	Ref_r copied = do_copy(sub);
	EXPECT_EQ(copied.type()->buffersize(), sub.type()->buffersize());
	EXPECT_NE(copied.get(), sub.get());
	EXPECT_EQ(*static_cast<const int*>(copied.get()), *static_cast<const int*>(sub.get()));

	this->m_tested_ref->reset();
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);

	EXPECT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	EXPECT_EQ(this->m_data[0], -1);

	EXPECT_EQ(4, *static_cast<const int*>(copied.get()));
}

/*
 * Name:                DataRefAnyTest.index_access
 *
 * Tested functions:    PDI::Ref_any::operator[](size_t)
 */
TEST_F(DataRefAnyTest, ref_index_access)
{
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);

	Ref_r sub = (*m_tested_ref)[4];
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_NE(get_content(sub), get_content(*this->m_tested_ref));
	EXPECT_EQ(get_content(sub)->m_buffer, get_content(*this->m_tested_ref)->m_buffer);
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);


	this->m_tested_ref->reset();
	EXPECT_FALSE(!get_content(sub));
	EXPECT_FALSE(!get_content(sub)->m_buffer);

	EXPECT_EQ(4, *static_cast<const int*>(sub.get()));
	sub.reset();
	EXPECT_EQ(this->m_data[0], -1);
}

/*
 * Name:                DataRefAnyTest.member_access
 *
 * Tested functions:    PDI::Ref_any::operator[](std::string)
 */
TEST_F(DataRefAnyTest, ref_member_access)
{
	auto&& char_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(char));

	struct Record {
		char x;
		int y[32];
	};

	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record, x), char_type, "x");
	members.emplace_back(offsetof(Record, y), this->m_tested_ref->type(), "y");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Record));

	Record data;
	data.x = 42;
	for (int i = 0; i < 32; i++) {
		data.y[i] = i;
	}

	Ref base_ref{&data, [](void* p) { static_cast<Record*>(p)->x = -1; }, record_type, true, true};
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref));
	EXPECT_FALSE(!Reference_base::get_content(*this->m_tested_ref)->m_buffer);

	Ref_r data_x_ref = base_ref["x"];
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);

	Ref_r data_y_ref = base_ref["y"];
	for (int i = 0; i < 32; i++) {
		EXPECT_EQ(i, static_cast<const int*>(data_y_ref.get())[i]);
	}

	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);

	auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));

	Ref data_y_scalar_ref = data_y_ref[12];
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	if (Ref_w failed{data_y_scalar_ref}) {
		FAIL();
	}
	if (Ref_w failed{base_ref}) {
		FAIL();
	}

	Ref_r data_y_scalar_ref_r{data_y_scalar_ref};
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_ref));
	EXPECT_FALSE(!get_content(data_y_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);
	EXPECT_EQ(12, *static_cast<const int*>(data_y_scalar_ref_r.get()));

	data_y_ref.reset();
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_x_ref));
	EXPECT_FALSE(!get_content(data_x_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);
	EXPECT_EQ(42, *static_cast<const char*>(data_x_ref.get()));

	data_x_ref.reset();
	EXPECT_FALSE(!get_content(base_ref));
	EXPECT_FALSE(!get_content(base_ref)->m_buffer);
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	base_ref.reset();
	EXPECT_FALSE(!get_content(data_y_scalar_ref));
	EXPECT_FALSE(!get_content(data_y_scalar_ref)->m_buffer);

	data_y_scalar_ref.reset();
	EXPECT_FALSE(!get_content(data_y_scalar_ref_r));
	EXPECT_FALSE(!get_content(data_y_scalar_ref_r)->m_buffer);
	EXPECT_EQ(42, data.x);

	data_y_scalar_ref_r.reset();

	EXPECT_EQ(-1, data.x);
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
	} catch (const Type_error& e) {
	}
	try {
		Ref sub = (*m_tested_ref)[4];
		sub["example"];
		FAIL();
	} catch (const Type_error& e) {
	}
	try {
		Ref sub = (*m_tested_ref)[4];
		sub[4];
		FAIL();
	} catch (const Type_error& e) {
	}
	try {
		auto&& char_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(char));

		struct Record {
			char x;
			int y[32];
		};

		std::vector<Record_datatype::Member> members;
		members.emplace_back(offsetof(Record, x), char_type, "x");
		members.emplace_back(offsetof(Record, y), this->m_tested_ref->type(), "y");
		auto&& record_type = Record_datatype::make(std::move(members), sizeof(Record));

		Record data;
		Ref base_ref{&data, [](void* p) { static_cast<Record*>(p)->x = -1; }, record_type, true, true};
		base_ref[4];
		FAIL();
	} catch (const Type_error& e) {
	}
}

/*
 * Name:                DataRefAnyTest.dereference_pointer
 *
 * Tested functions:    PDI::Ref_any::dereference()
 */
TEST_F(DataRefAnyTest, dereference_pointer)
{
	/* SCALAR */
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	auto&& ptr_scalar_type = Pointer_datatype::make(scalar_type);

	int m_scalar = 12;
	int* m_ptr_scalar = &m_scalar;

	Ref_r base_scalar_ref{&m_scalar, [](void*) {}, scalar_type, true, true};
	Ref_r pointer_scalar_ref{&m_ptr_scalar, [](void*) {}, ptr_scalar_type, true, true};
	Ref_r dereferenced_scalar_ref = pointer_scalar_ref.dereference();

	// Check that the reference is valid.
	EXPECT_FALSE(!dereferenced_scalar_ref);
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_scalar_ref));
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_scalar_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(dereferenced_scalar_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(dereferenced_scalar_ref)->m_buffer->m_write_locks);

	// Check that the reference has been dereferenced.
	EXPECT_EQ(base_scalar_ref.type(), dereferenced_scalar_ref.type())
		<< "Pointer datatype:\n"
		<< dereferenced_scalar_ref.type()->debug_string() << "\n\nScalar datatype:\n"
		<< base_scalar_ref.type()->debug_string();

	const void* base_scalar_ptr = base_scalar_ref.get();
	const void* dereferenced_scalar_ptr = dereferenced_scalar_ref.get();
	EXPECT_EQ(reinterpret_cast<int*>(const_cast<void*>(base_scalar_ptr)), reinterpret_cast<int*>(const_cast<void*>(dereferenced_scalar_ptr)));


	/* ARRAY */
	auto&& array_type = Array_datatype::make(scalar_type, 32);
	auto&& ptr_array_type = Pointer_datatype::make(array_type);

	int m_scalar_array[32];
	for (size_t i = 0; i < 32; ++i)
		m_scalar_array[i] = i;
	int(*m_ptr_scalar_array)[32] = &m_scalar_array;

	Ref_r base_array_ref{m_scalar_array, [](void*) {}, array_type, true, true};
	Ref_r pointer_array_ref{&m_ptr_scalar_array, [](void*) {}, ptr_array_type, true, true};
	Ref_r dereferenced_array_ref = pointer_array_ref.dereference();

	// Check that the reference is valid.
	EXPECT_FALSE(!dereferenced_array_ref);
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_array_ref));
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_array_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(dereferenced_array_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(dereferenced_array_ref)->m_buffer->m_write_locks);

	// Check that the reference has been dereferenced.
	EXPECT_EQ(base_array_ref.type(), dereferenced_array_ref.type())
		<< "Pointer datatype:\n"
		<< dereferenced_array_ref.type()->debug_string() << "\n\nArray datatype:\n"
		<< base_array_ref.type()->debug_string();


	/* RECORD */
	auto&& char_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(char));

	struct Record_t {
		char x;
		int y[32];
	};

	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record_t, x), char_type, "x");
	members.emplace_back(offsetof(Record_t, y), array_type, "y");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Record_t));
	auto&& ptr_record_type = Pointer_datatype::make(record_type);

	Record_t m_record_data;
	m_record_data.x = 42;
	for (int i = 0; i < 32; i++) {
		m_record_data.y[i] = i;
	}

	Ref_r base_record_ref{&m_record_data, [](void*) {}, record_type, true, true};

	Record_t* m_ptr_record_data = &m_record_data;
	Ref_r pointer_record_ref{&m_ptr_record_data, [](void*) {}, ptr_record_type, true, true};
	Ref_r dereferenced_record_ref = pointer_record_ref.dereference();

	// Check that the reference is valid.
	EXPECT_FALSE(!dereferenced_record_ref);
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_record_ref));
	EXPECT_FALSE(!Reference_base::get_content(dereferenced_record_ref)->m_buffer);
	EXPECT_EQ(0, Reference_base::get_content(dereferenced_record_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(dereferenced_record_ref)->m_buffer->m_write_locks);

	// Check that the reference has been dereferenced.
	EXPECT_EQ(base_record_ref.type(), dereferenced_record_ref.type())
		<< "Scalar datatype:\n"
		<< base_record_ref.type()->debug_string() << "\n\nPointer datatype:\n"
		<< dereferenced_record_ref.type()->debug_string();

	const void* base_record_ptr = base_array_ref.get();
	const void* dereferenced_record_ptr = dereferenced_array_ref.get();
	EXPECT_EQ(reinterpret_cast<int*>(const_cast<void*>(base_record_ptr)), reinterpret_cast<int*>(const_cast<void*>(dereferenced_record_ptr)));
}

/*
 * Name:                DataRefAnyTest.dereference_pointer
 *
 * Tested functions:    PDI::Ref_any::dereference()
 */
TEST_F(DataRefAnyTest, invalid_dereference_pointers)
{
	/* Scalar */
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	int m_scalar = 12;
	Ref base_scalar_ref{&m_scalar, [](void*) {}, scalar_type, true, true};

	try {
		Ref dereferenced_scalar_ref = base_scalar_ref.dereference();
		ADD_FAILURE();
	} catch (const Type_error& e) {
	}

	/* Char */
	auto&& char_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(char));
	char m_char = 'a';
	Ref base_char_ref{&m_char, [](void*) {}, char_type, true, true};

	try {
		Ref dereferenced_char_ref = base_char_ref.dereference();
		ADD_FAILURE();
	} catch (const Type_error& e) {
	}


	/* Array */
	auto&& array_type = Array_datatype::make(scalar_type, 32);
	int m_scalar_array[32];
	for (size_t i = 0; i < 32; ++i)
		m_scalar_array[i] = i;
	Ref base_array_ref{m_scalar_array, [](void*) {}, array_type, true, true};

	try {
		Ref dereferenced_array_ref = base_array_ref.dereference();
		ADD_FAILURE();
	} catch (const Type_error& e) {
	}

	/* Record */
	struct Record_t {
		char x;
		int y[32];
	};

	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Record_t, x), char_type, "x");
	members.emplace_back(offsetof(Record_t, y), array_type, "y");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Record_t));

	Record_t m_record_data;
	m_record_data.x = 42;
	for (int i = 0; i < 32; i++) {
		m_record_data.y[i] = i;
	}
	Ref base_record_ref{&m_record_data, [](void*) {}, record_type, true, true};

	try {
		Ref dereferenced_record_ref = base_record_ref.dereference();
		ADD_FAILURE();
	} catch (const Type_error& e) {
	}
}

/*
 * Struct prepared for DataRefAnyTypedTest.
 */
template <typename T>
struct DataRefAnyTypedTest
	: public DataRefAnyTest
	, T {};

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
	EXPECT_FALSE(ref);
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
	EXPECT_FALSE(ref_copy);
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
	EXPECT_FALSE(ref_moved);
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

	EXPECT_TRUE(*this->m_tested_ref);
	EXPECT_EQ(*this->m_tested_ref, chref);
	EXPECT_FALSE(!Reference_base::get_content(chref));
	EXPECT_FALSE(!Reference_base::get_content(chref)->m_buffer);

	size_t expected_locks;
	if constexpr (is_same<TypeParam, Ref>::value) {
		expected_locks = 0;
	} else if constexpr (is_same<TypeParam, Ref_r>::value) {
		expected_locks = Reference_base::Referenced_buffer::S_WLCK;
	} else if constexpr (is_same<TypeParam, Ref_w>::value) {
		expected_locks = Reference_base::Referenced_buffer::S_RLCK + Reference_base::Referenced_buffer::S_WLCK;
	} else { //is_same<TypeParam, Ref_rw>::value
		expected_locks = Reference_base::Referenced_buffer::S_RLCK + Reference_base::Referenced_buffer::S_WLCK;
	}
	EXPECT_EQ(expected_locks, Reference_base::get_content(chref)->m_buffer->m_locks);
}

/*
 * Struct prepared for DenseArrayRefAnyTest.
 */
struct DenseArrayRefAnyTest: public DataRefAnyTest {
	DenseArrayRefAnyTest()
		: DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();

		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}

		DataRefAnyTest::m_tested_ref = unique_ptr<Ref>{new Ref{array_to_share, [](void*) {}, datatype, true, true}};
	}

	Datatype_sptr datatype{Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10), 10)};

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
	EXPECT_TRUE(changed_ref);

	Ref_rw cloned_ref(changed_ref.copy());
	int* cloned_array = static_cast<int*>(cloned_ref.get());
	for (int i = 0; i < 100; i++) {
		EXPECT_EQ(this->array_to_share[i], cloned_array[i]);
	}
}

/*
 * Struct prepared for SparseArrayRefAnyTest.
 *
 */
struct SparseArrayRefAnyTest: public DataRefAnyTest {
	int* array_to_share{new int[100]}; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)

	SparseArrayRefAnyTest()
		: DataRefAnyTest()
	{
		m_tested_ref->reset();

		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}

		m_tested_ref = unique_ptr<Ref>{new Ref{array_to_share, [](void* ptr) { operator delete[] (ptr); }, datatype, true, true}};
	}

	Datatype_sptr datatype{Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10, 3, 4), 10, 3, 4)};
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
	EXPECT_TRUE(changed_ref);

	Ref_r cloned_ref(changed_ref.copy());
	const int* cloned_array = static_cast<const int*>(cloned_ref.get());
	for (int i = 0; i < 16; i++) {
		std::cerr << i << ": " << cloned_array[i] << std::endl;
		EXPECT_EQ(this->array_to_share[(i / 4 + 3) * 10 + (i % 4) + 3], cloned_array[i]);
	}
}

/*
 * Struct prepared for DenseRecordRefAnyTest.
 */
struct DenseRecordRefAnyTest: public DataRefAnyTest {
	struct Struct_def {
		char char_array[25]; //disp = 0
		long long_scalar; //disp = 32
		int int_scalar; //disp = 40
	}; //sizeof = 48

	Struct_def* record_to_share = new Struct_def;

	DenseRecordRefAnyTest()
		: DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();

		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;

		DataRefAnyTest::m_tested_ref = unique_ptr<Ref>{new Ref{record_to_share, [](void* ptr) { operator delete (ptr); }, datatype, true, true}};
	}

	vector<Record_datatype::Member> get_members()
	{
		return {
			Record_datatype::Member{
				offsetof(Struct_def, char_array),
				Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 25),
				"char_array"
			},
			Record_datatype::Member{offsetof(Struct_def, long_scalar), Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), "long_scalar"},
			Record_datatype::Member{offsetof(Struct_def, int_scalar), Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "int_scalar"}
		};
	}

	Datatype_sptr datatype{Record_datatype::make(get_members(), sizeof(Struct_def))};
};

TEST_F(DenseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	EXPECT_TRUE(changed_ref);

	Ref_r cloned_ref(changed_ref.copy());
	const Struct_def* cloned_array = static_cast<const Struct_def*>(cloned_ref.get());
	for (int i = 0; i < 25; i++) {
		EXPECT_EQ(cloned_array->char_array[i], i);
	}
	EXPECT_EQ(cloned_array->long_scalar, 10);
	EXPECT_EQ(cloned_array->int_scalar, -10);
}

/*
 * Struct prepared for SparseRecordRefAnyTest.
 */
struct SparseRecordRefAnyTest: public DataRefAnyTest {
	struct Sparse_struct_def {
		char char_array[25]; //disp = 0 //buffer: 5 x 5; data: 3 x 3; start: (1, 1)
		long long_scalar; //disp = 32
		int int_scalar; //disp = 40
	}; //sizeof = 48

	struct Dense_struct_def {
		char char_array[9]; //disp = 0 //buffer: 3 x 3; data: 3 x 3;
		long long_scalar; //disp = 24
		int int_scalar; //disp = 28
	}; //sizeof = 32

	Sparse_struct_def* record_to_share = new Sparse_struct_def;

	SparseRecordRefAnyTest()
		: DataRefAnyTest()
	{
		DataRefAnyTest::m_tested_ref->reset();

		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;

		DataRefAnyTest::m_tested_ref = unique_ptr<Ref>{new Ref{record_to_share, [](void* ptr) { operator delete (ptr); }, datatype, true, true}};
	}

	vector<Record_datatype::Member> get_members()
	{
		return {
			Record_datatype::Member{
				offsetof(Sparse_struct_def, char_array),
				Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)), 5, 1, 3), 5, 1, 3),
				"char_array"
			},
			Record_datatype::Member{
				offsetof(Sparse_struct_def, long_scalar),
				Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
				"long_scalar"
			},
			Record_datatype::Member{offsetof(Sparse_struct_def, int_scalar), Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), "int_scalar"}
		};
	}

	Datatype_sptr datatype{Record_datatype::make(get_members(), sizeof(Sparse_struct_def))};
};

TEST_F(SparseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(*this->m_tested_ref);
	EXPECT_TRUE(changed_ref);

	Ref_r cloned_ref(changed_ref.copy());
	const Dense_struct_def* cloned_array = static_cast<const Dense_struct_def*>(cloned_ref.get());
	for (int i = 0; i < 9; i++) {
		EXPECT_EQ(cloned_array->char_array[i], (i / 3 + 1) * 5 + i % 3 + 1);
	}
	EXPECT_EQ(cloned_array->long_scalar, 10);
	EXPECT_EQ(cloned_array->int_scalar, -10);
}
