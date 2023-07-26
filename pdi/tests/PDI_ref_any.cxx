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

#include <pdi/array_datatype.h>
#include <pdi/pdi_fwd.h>
#include <pdi/record_datatype.h>
#include <pdi/pointer_datatype.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>

#include "mocks/datatype_mock.h"

using namespace PDI;
using namespace std;

using ::testing::_;
using ::testing::Return;

/* Struct prepared for DataRefAnyTest.
 */
struct DataRefAnyTest
	: public ::testing::Test
	, Reference_base
{
	bool destructor_called = false;

	std::shared_ptr<MockDatatype const> ref_type = make_unique<MockDatatype>();

	int data = [this] () {
		EXPECT_CALL(*ref_type, datasize).WillOnce(Return(1));
		return 1;
	}();

	PDI::Ref tested_ref {&data, [this] (void*) { destructor_called = true; }, ref_type, true, true};

	~DataRefAnyTest()
	{
		if (!destructor_called) {
			EXPECT_CALL(*ref_type, destroy_data(&data));
		}
	}
};

/* Tested functions:
 * - PDI::Ref_any::Ref_any()
 *
 * Description:
 * Check if the reference is correctly created.
 */
TEST_F(DataRefAnyTest, defaultConstructor)
{
	Ref null_ref;
	EXPECT_FALSE(Reference_base::get_content(null_ref));
}

/* Tested functions:
 * - PDI::Ref_any::Ref_any(Ref& )
 *
 * Description:
 * Check if reference copy is correctly created and ref properties updated.
 */
TEST_F(DataRefAnyTest, copyConstructor)
{
	Ref copied_ref(tested_ref);

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(tested_ref));
}

/* Tested functions:
 * - PDI::Ref_any::Ref_any(Ref&& )
 */
TEST_F(DataRefAnyTest, moveConstructor)
{
	Ref moved_ref(move(tested_ref));

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_TRUE(Reference_base::get_content(moved_ref));
	EXPECT_TRUE(Reference_base::get_content(moved_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(moved_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(moved_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(nullptr, Reference_base::get_content(tested_ref));
	EXPECT_CALL(*ref_type, destroy_data(&data));
}

/* Tested functions:
 * - PDI::Ref_any::Ref_any(void*, std::function<void(void*)>, Datatype_sptr, bool, bool)
 *
 * Description:
 * Check if the reference is correctly created.
 */
TEST_F(DataRefAnyTest, newReferenceConstructor)
{
	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);
}

/* Tested functions:
 * - Datatype_sptr type() const noexcept
 */
TEST_F(DataRefAnyTest, type)
{
	Ref null_ref;
	Ref copied_ref = tested_ref;
	int data2;
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref sub_ref = tested_ref[0];

	EXPECT_EQ(tested_ref.type(), ref_type);
	EXPECT_EQ(*null_ref.type(), *UNDEF_TYPE);
	EXPECT_EQ(copied_ref.type(), ref_type);
	EXPECT_EQ(sub_ref.type(), ref_type);
}

/* Tested functions:
 * - size_t hash() const noexcept
 * - bool operator== (const Reference_base& o) const noexcept
 * - bool operator!= (const Reference_base& o) const noexcept
 */
TEST_F(DataRefAnyTest, equality)
{
	Ref null_ref1;
	Ref null_ref2;
	Ref copied_ref = tested_ref;
	int data2;
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref subref1 = tested_ref[0];
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref subref2 = tested_ref[0];

	EXPECT_FALSE(tested_ref == null_ref1);
	EXPECT_FALSE(tested_ref == null_ref2);
	EXPECT_TRUE(tested_ref == copied_ref);
	EXPECT_FALSE(tested_ref == subref1);
	EXPECT_FALSE(tested_ref == subref2);
	EXPECT_TRUE(null_ref1 == null_ref2);
	EXPECT_FALSE(null_ref1 == copied_ref);
	EXPECT_FALSE(null_ref1 == subref1);
	EXPECT_FALSE(null_ref1 == subref2);
	EXPECT_FALSE(null_ref2 == copied_ref);
	EXPECT_FALSE(null_ref2 == subref1);
	EXPECT_FALSE(null_ref2 == subref2);
	EXPECT_FALSE(copied_ref == subref1);
	EXPECT_FALSE(copied_ref == subref2);
	EXPECT_TRUE(subref1 == subref2);

	EXPECT_NE(tested_ref.hash(), null_ref1.hash());
	EXPECT_NE(tested_ref.hash(), null_ref2.hash());
	EXPECT_EQ(tested_ref.hash(), copied_ref.hash());
	EXPECT_NE(tested_ref.hash(), subref1.hash());
	EXPECT_NE(tested_ref.hash(), subref2.hash());
	EXPECT_EQ(null_ref1.hash(), null_ref2.hash());
	EXPECT_NE(null_ref1.hash(), copied_ref.hash());
	EXPECT_NE(null_ref1.hash(), subref1.hash());
	EXPECT_NE(null_ref1.hash(), subref2.hash());
	EXPECT_NE(null_ref2.hash(), copied_ref.hash());
	EXPECT_NE(null_ref2.hash(), subref1.hash());
	EXPECT_NE(null_ref2.hash(), subref2.hash());
	EXPECT_NE(copied_ref.hash(), subref1.hash());
	EXPECT_NE(copied_ref.hash(), subref2.hash());
	EXPECT_EQ(subref1.hash(), subref2.hash());

	EXPECT_TRUE(tested_ref != null_ref1);
	EXPECT_TRUE(tested_ref != null_ref2);
	EXPECT_FALSE(tested_ref != copied_ref);
	EXPECT_TRUE(tested_ref != subref1);
	EXPECT_TRUE(tested_ref != subref2);
	EXPECT_FALSE(null_ref1 != null_ref2);
	EXPECT_TRUE(null_ref1 != copied_ref);
	EXPECT_TRUE(null_ref1 != subref1);
	EXPECT_TRUE(null_ref1 != subref2);
	EXPECT_TRUE(null_ref2 != copied_ref);
	EXPECT_TRUE(null_ref2 != subref1);
	EXPECT_TRUE(null_ref2 != subref2);
	EXPECT_TRUE(copied_ref != subref1);
	EXPECT_TRUE(copied_ref != subref2);
	EXPECT_FALSE(subref1 != subref2);
}

/* Tested functions:
 * - PDI::Ref_any::Ref_any& operator= (const Ref_any&) noexcept
 *
 * Description:
 * Check if reference copy is correctly created and ref properties updated.
 */
TEST_F(DataRefAnyTest, copyAssignment)
{
	Ref copied_ref;
	copied_ref = tested_ref;

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(tested_ref));
}

/* Tested functions:
 * - Ref_any& operator= (Ref_any&& other) noexcept
 */
TEST_F(DataRefAnyTest, moveAssignment)
{
	Ref moved_ref;
	moved_ref = move(tested_ref);

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_TRUE(Reference_base::get_content(moved_ref));
	EXPECT_TRUE(Reference_base::get_content(moved_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(moved_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(moved_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(moved_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(nullptr, Reference_base::get_content(tested_ref));
	EXPECT_CALL(*ref_type, destroy_data(&data));
}

/* Tested functions:
 * - bool operator< (const Reference_base& o) const noexcept
 * - bool operator> (const Reference_base& o) const noexcept
 * - bool operator<= (const Reference_base& o) const noexcept
 * - bool operator>= (const Reference_base& o) const noexcept
 */
TEST_F(DataRefAnyTest, order)
{
	Ref null_ref1;
	Ref null_ref2;
	Ref copied_ref = tested_ref;
	int data2;
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref subref1 = tested_ref[0];
	int data3;
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data3, ref_type)));
	Ref subref2 = tested_ref[0];

	//TODO
}

/* Tested functions:
 * - PDI::Ref_any::operator[](char const*)
 */
TEST_F(DataRefAnyTest, member)
{
	int data2;
	EXPECT_CALL(*ref_type, member("x", _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref sub_ref = tested_ref["x"];

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);
	
	Ref null_ref;
	EXPECT_THROW(null_ref["x"], Type_error);
}

/* Tested functions:
 * - PDI::Ref_any::operator[](size_t)
 */
TEST_F(DataRefAnyTest, index)
{
	int data2;
	EXPECT_CALL(*ref_type, index(4, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref sub_ref = tested_ref[4];

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);
	
	Ref null_ref;
	EXPECT_THROW(null_ref[4], Type_error);
}

/* Tested functions:
 * - PDI::Ref_any::operator[](std::pair<size_t, size_t>)
 */
TEST_F(DataRefAnyTest, slice)
{
	int data2;
	EXPECT_CALL(*ref_type, slice(4,8, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref sub_ref = tested_ref[std::make_pair(4,8)];

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);
	
	Ref null_ref;
	EXPECT_THROW(null_ref[std::make_pair(4,8)], Type_error);
}

/* Tested functions:
 * - ref_access_t<R, W> PDI::Ref_any::get () const
 * - PDI::Ref_any::operator ref_access_t<R, W> () const
 * - ref_access_t<R, W> PDI::Ref_any::get (std::nothrow_t) const noexcept
 */
TEST_F(DataRefAnyTest, get)
{
	Ref_r copied_ref = tested_ref;
	EXPECT_EQ(&data, copied_ref.get());
	EXPECT_EQ(&data, copied_ref.get(std::nothrow));
	EXPECT_EQ(&data, static_cast<void const*>(copied_ref));
	
	Ref_r null_ref;
	ASSERT_THROW(null_ref.get(), Right_error);
	EXPECT_EQ(nullptr, null_ref.get(std::nothrow));
	ASSERT_THROW(static_cast<void const*>(null_ref), Right_error);
}

/* Tested functions:
 * - T scalar_value () const
 */
TEST_F(DataRefAnyTest, scalarValue)
{
	//TODO
}

/* Tested functions:
 * - explicit operator bool () const noexcept
 */
TEST_F(DataRefAnyTest, operatorBool)
{
	Ref null_ref;
	Ref copied_null_ref = null_ref;
	Ref copied_ref = tested_ref;
	int data2;
	EXPECT_CALL(*ref_type, index(0, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref sub_ref = tested_ref[0];
	
	EXPECT_TRUE(tested_ref);
	EXPECT_FALSE(null_ref);
	EXPECT_FALSE(copied_null_ref);
	EXPECT_TRUE(copied_ref);
	EXPECT_TRUE(sub_ref);
}

/* Tested functions:
 * - PDI::Ref_any::reset()
 *
 * Description:
 * Checks if destructor is called on reset().
 */
TEST_F(DataRefAnyTest, resetToDestroy)
{
	EXPECT_CALL(*ref_type, destroy_data(&data));
	tested_ref.reset();
	EXPECT_TRUE(destructor_called);
}

/* Tested functions: PDI::Ref_any::reset()
 *
 * Description:
 * Checks that the destructor isn't called on reset() when more than 1 owner.
 */
TEST_F(DataRefAnyTest, reset)
{
	Ref_r copied_ref = tested_ref;

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(copied_ref)->m_buffer
	);
	
	int data2;
	EXPECT_CALL(*ref_type, index(4, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref_r sub_ref = tested_ref[4];

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(copied_ref)->m_buffer
	);
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);

	EXPECT_FALSE(destructor_called);

	tested_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_NE(Reference_base::get_content(copied_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(copied_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);

	EXPECT_FALSE(destructor_called);

	tested_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_TRUE(Reference_base::get_content(copied_ref));
	EXPECT_TRUE(Reference_base::get_content(copied_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(copied_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(copied_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(2, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_NE(Reference_base::get_content(copied_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(copied_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);

	EXPECT_FALSE(destructor_called);

	copied_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_FALSE(Reference_base::get_content(copied_ref));

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));

	EXPECT_FALSE(destructor_called);

	EXPECT_CALL(*ref_type, destroy_data(_));
	sub_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_FALSE(Reference_base::get_content(copied_ref));

	EXPECT_FALSE(Reference_base::get_content(sub_ref));

	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(copied_ref));
	EXPECT_EQ(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));

	EXPECT_TRUE(destructor_called);
}

/*
 * Name:                DataRefAnyTest.content_deep_copy
 *
 * Tested functions:    PDI::Ref_any::do_copy()
 */
TEST_F(DataRefAnyTest, copy)
{
	int data2;
	EXPECT_CALL(*ref_type, index(4, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	Ref_r sub_ref = tested_ref[4];

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);

	int data3;
	EXPECT_CALL(*ref_type, densify).WillOnce(Return(ref_type));
	EXPECT_CALL(*ref_type, buffersize).WillOnce(Return(1)).WillOnce(Return(1)).WillOnce(Return(1));
	EXPECT_CALL(*ref_type, datasize).WillOnce(Return(1));
	EXPECT_CALL(*ref_type, alignment).WillOnce(Return(1)).WillOnce(Return(1));
	EXPECT_CALL(*ref_type, data_to_dense_copy).WillOnce(Return(&data3));
	Ref_r copied = sub_ref.copy();

	EXPECT_TRUE(Reference_base::get_content(tested_ref));
	EXPECT_TRUE(Reference_base::get_content(tested_ref)->m_buffer);
	EXPECT_EQ(&data, Reference_base::get_content(tested_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(tested_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(tested_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(tested_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied));
	EXPECT_TRUE(Reference_base::get_content(copied)->m_buffer);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(copied)->m_buffer->m_write_locks);

	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(sub_ref));
	EXPECT_NE(Reference_base::get_content(tested_ref), Reference_base::get_content(copied));
	EXPECT_NE(Reference_base::get_content(sub_ref), Reference_base::get_content(copied));
	EXPECT_EQ(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(sub_ref)->m_buffer
	);
	EXPECT_NE(
			Reference_base::get_content(tested_ref)->m_buffer,
			Reference_base::get_content(copied)->m_buffer
	);
	EXPECT_NE(
			Reference_base::get_content(sub_ref)->m_buffer,
			Reference_base::get_content(copied)->m_buffer
	);

	tested_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_TRUE(Reference_base::get_content(sub_ref));
	EXPECT_TRUE(Reference_base::get_content(sub_ref)->m_buffer);
	EXPECT_EQ(&data2, Reference_base::get_content(sub_ref)->m_data);
	EXPECT_EQ(ref_type, Reference_base::get_content(sub_ref)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(sub_ref)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(sub_ref)->m_buffer->m_write_locks);

	EXPECT_TRUE(Reference_base::get_content(copied));
	EXPECT_TRUE(Reference_base::get_content(copied)->m_buffer);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(copied)->m_buffer->m_write_locks);

	EXPECT_CALL(*ref_type, destroy_data(&data));
	sub_ref.reset();

	EXPECT_FALSE(Reference_base::get_content(tested_ref));

	EXPECT_FALSE(Reference_base::get_content(sub_ref));

	EXPECT_TRUE(Reference_base::get_content(copied));
	EXPECT_TRUE(Reference_base::get_content(copied)->m_buffer);
	EXPECT_EQ(ref_type, Reference_base::get_content(copied)->m_type);
	EXPECT_EQ(0, Reference_base::get_content(copied)->m_buffer->m_read_locks);
	EXPECT_EQ(1, Reference_base::get_content(copied)->m_buffer->m_write_locks);

	EXPECT_TRUE(destructor_called);

	EXPECT_CALL(*ref_type, destroy_data(_));
}

/* Tested functions:
 * - PDI::Ref_any::release()
 *
 * Description:
 * Checks if release returns the correct address, and does not call the destructor.
 */
TEST_F(DataRefAnyTest, release)
{
	auto dupref = tested_ref;

	int data2;
	EXPECT_CALL(*ref_type, index(7, _)).WillOnce(Return(make_pair(&data2, ref_type)));
	auto sub_ref = dupref[7];

	EXPECT_EQ(&data, tested_ref.release());
	EXPECT_FALSE(tested_ref);
	EXPECT_FALSE(Reference_base::get_content(tested_ref));
	EXPECT_FALSE(dupref);
	EXPECT_FALSE(sub_ref);
	EXPECT_FALSE(destructor_called);
	destructor_called = true;
}

/* Tested functions:
 * - PDI::Ref_any::on_nullify()
 *
 * Description:
 * Check if the function passed in on_nullify is called on release by different owner.
 */
TEST_F(DataRefAnyTest, onNullify)
{
	//the address to replace
	bool callback_called = false;
	tested_ref.on_nullify([&] (Ref) { callback_called = true; });
	Ref otherRef(tested_ref);
	otherRef.release();
	EXPECT_TRUE(callback_called);
	destructor_called = true;
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
		Ref base_ref {&data, [](void* p){static_cast<Record*>(p)->x = -1;}, record_type, true, true};
		base_ref[4];
		FAIL();
	} catch (const Type_error& e) {}
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
		<< "Pointer datatype:\n" << dereferenced_scalar_ref.type()->debug_string()
		<< "\n\nScalar datatype:\n" << base_scalar_ref.type()->debug_string();

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
        << "Pointer datatype:\n" << dereferenced_array_ref.type()->debug_string()
        << "\n\nArray datatype:\n" << base_array_ref.type()->debug_string();


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
        << "Scalar datatype:\n" << base_record_ref.type()->debug_string()
		<< "\n\nPointer datatype:\n" << dereferenced_record_ref.type()->debug_string();

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
struct DataRefAnyTypedTest : public DataRefAnyTest
{
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
	TypeParam chref(this->tested_ref);

	EXPECT_TRUE(this->tested_ref);
	EXPECT_EQ(this->tested_ref, chref);
	EXPECT_TRUE(Reference_base::get_content(chref));
	EXPECT_TRUE(Reference_base::get_content(chref)->m_buffer);

	if (this->locks & 2) {
		//if granted with write access
		EXPECT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_read_locks);
		EXPECT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_write_locks);
	} else {
		//if not granted with write access
		if (this->locks & 1) {
			//if granted with read access
			EXPECT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_read_locks);
			EXPECT_EQ(1, Reference_base::get_content(chref)->m_buffer->m_write_locks);
		} else {
			//if not granted with read access
			EXPECT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_read_locks);
			EXPECT_EQ(0, Reference_base::get_content(chref)->m_buffer->m_write_locks);
		}
	}
}

/*
 * Struct prepared for DenseArrayRefAnyTest.
 */
struct DenseArrayRefAnyTest : public DataRefAnyTest
{
	DenseArrayRefAnyTest() : DataRefAnyTest()
	{
		tested_ref.reset();

		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}

		tested_ref
				= Ref {array_to_share,
		               [] (void*) {},
		               Array_datatype::
		                       make(Array_datatype::
		                                    make(Scalar_datatype::
		                                                 make(Scalar_kind::SIGNED, sizeof(int)),
		                                         10),
		                            10),
		               true,
		               true};
	}

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
	Ref_r changed_ref(tested_ref);
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
struct SparseArrayRefAnyTest : public DataRefAnyTest
{
	int* array_to_share {new int[100]}; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)

	SparseArrayRefAnyTest() : DataRefAnyTest()
	{
		tested_ref.reset();

		for (int i = 0; i < 100; i++) {
			array_to_share[i] = i;
		}

		tested_ref = Ref(
				array_to_share,
				[] (void* ptr) { operator delete[] (ptr); },
				Array_datatype::
						make(Array_datatype::
		                             make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
		                                  10,
		                                  3,
		                                  4),
		                     10,
		                     3,
		                     4),
				true,
				true
		);
	}
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
	Ref_r changed_ref(tested_ref);
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
struct DenseRecordRefAnyTest : public DataRefAnyTest
{
	struct Struct_def
	{
		char char_array[25]; //disp = 0
		long long_scalar; //disp = 32
		int int_scalar; //disp = 40
	}; //sizeof = 48

	Struct_def* record_to_share = new Struct_def;

	DenseRecordRefAnyTest() : DataRefAnyTest()
	{
		DataRefAnyTest::tested_ref.reset();

		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;

		tested_ref = Ref(
				record_to_share,
				[this] (void*) { destructor_called = true; },
				Record_datatype::
						make({{offsetof(Struct_def, char_array),
		                       Array_datatype::
		                               make(Scalar_datatype::
		                                            make(Scalar_kind::UNSIGNED, sizeof(char)),
		                                    25),
		                       "char_array"},
		                      {offsetof(Struct_def, long_scalar),
		                       Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
		                       "long_scalar"},
		                      {offsetof(Struct_def, int_scalar),
		                       Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
		                       "int_scalar"}},
		                     sizeof(Struct_def)),
				true,
				true
		);
	}
};

TEST_F(DenseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(tested_ref);
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
struct SparseRecordRefAnyTest : public DataRefAnyTest
{
	struct Sparse_struct_def
	{
		char char_array[25]; //disp = 0 //buffer: 5 x 5; data: 3 x 3; start: (1, 1)
		long long_scalar; //disp = 32
		int int_scalar; //disp = 40
	}; //sizeof = 48

	struct Dense_struct_def
	{
		char char_array[9]; //disp = 0 //buffer: 3 x 3; data: 3 x 3;
		long long_scalar; //disp = 24
		int int_scalar; //disp = 28
	}; //sizeof = 32

	Sparse_struct_def* record_to_share = new Sparse_struct_def;

	SparseRecordRefAnyTest() : DataRefAnyTest()
	{
		tested_ref.reset();

		for (int i = 0; i < 25; i++) {
			record_to_share->char_array[i] = i;
		}
		record_to_share->long_scalar = 10;
		record_to_share->int_scalar = -10;

		tested_ref = Ref(
				record_to_share,
				[] (void* ptr) { operator delete (ptr); },
				datatype,
				true,
				true
		);
	}

	vector<Record_datatype::Member> get_members ()
	{
		return {Record_datatype::
		                Member {offsetof(Sparse_struct_def, char_array),
		                        Array_datatype::
		                                make(Array_datatype::
		                                             make(Scalar_datatype::
		                                                          make(Scalar_kind::UNSIGNED,
		                                                               sizeof(char)),
		                                                  5,
		                                                  1,
		                                                  3),
		                                     5,
		                                     1,
		                                     3),
		                        "char_array"},
		        Record_datatype::
		                Member {offsetof(Sparse_struct_def, long_scalar),
		                        Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
		                        "long_scalar"},
		        Record_datatype::
		                Member {offsetof(Sparse_struct_def, int_scalar),
		                        Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
		                        "int_scalar"}};
	}

	Datatype_sptr datatype {Record_datatype::make(get_members(), sizeof(Sparse_struct_def))};
};

TEST_F(SparseRecordRefAnyTest, checkDeepCopy)
{
	Ref_r changed_ref(tested_ref);
	EXPECT_TRUE(changed_ref);

	Ref_r cloned_ref(changed_ref.copy());
	const Dense_struct_def* cloned_array = static_cast<const Dense_struct_def*>(cloned_ref.get());
	for (int i = 0; i < 9; i++) {
		EXPECT_EQ(cloned_array->char_array[i], (i / 3 + 1) * 5 + i % 3 + 1);
	}
	EXPECT_EQ(cloned_array->long_scalar, 10);
	EXPECT_EQ(cloned_array->int_scalar, -10);
}
