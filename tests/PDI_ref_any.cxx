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
#include <gmock/gmock.h>

#include <pdi/ref_any.h>
#include <pdi/pdi_fwd.h>

#include "mocks/datatype_mock.h"

#define DATA_SIZE 1024

using namespace PDI;
using namespace std;

using ::testing::Return;


struct DataRefAnyTest : public ::testing::Test, Ref_base {
	DataRefAnyTest() {
		EXPECT_CALL(*this->mocked_datatype, datasize())
			.Times(1)
			.WillOnce(Return(DATA_SIZE));

		m_tested_ref = unique_ptr<Ref> {new Ref{&m_data, [](void* d){ *((bool*) d) = false;  },
											Datatype_uptr{mocked_datatype}, true, true}};
	}

	MockDatatype* mocked_datatype {new MockDatatype()};
	bool m_data {true};

	unique_ptr<Ref> m_tested_ref;
};

template<typename T>
struct DataRefAnyTypedTest : public DataRefAnyTest, T {
	DataRefAnyTypedTest() : DataRefAnyTest() {
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


TYPED_TEST(DataRefAnyTypedTest, NullRef) {
	TypeParam ref;
	ASSERT_FALSE(ref);
}

TYPED_TEST(DataRefAnyTypedTest, PossibleNullRefCopy) {
	TypeParam ref;
	TypeParam ref_copy(ref);
	ASSERT_FALSE(ref_copy);
}

TYPED_TEST(DataRefAnyTypedTest, PossibleNullRefMove) {
	TypeParam ref;
	TypeParam ref_moved(std::move(ref));
	ASSERT_FALSE(ref_moved);
}

TYPED_TEST(DataRefAnyTypedTest, chmodConstructor) {
	//copies a reference with different privileges
	TypeParam chref(*this->m_tested_ref);

	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, chref);
	ASSERT_EQ(2, Ref_base::get_content(chref)->m_owners);

	if (this->locks & 2) {
		//if granted with write access
		ASSERT_EQ(1, Ref_base::get_content(chref)->m_read_locks);
		ASSERT_EQ(1, Ref_base::get_content(chref)->m_write_locks);
	} else {
		//if not granted with write access
		if (this->locks & 1) {
			//if granted with read access
			ASSERT_EQ(0, Ref_base::get_content(chref)->m_read_locks);
			ASSERT_EQ(1, Ref_base::get_content(chref)->m_write_locks);
		} else {
			//if not granted with read access
			ASSERT_EQ(0, Ref_base::get_content(chref)->m_read_locks);
			ASSERT_EQ(0, Ref_base::get_content(chref)->m_write_locks);
		}
	}
}

// TYPED_TEST(DataRefAnyTypedTest, deepCopy) {
// 	Ref_r ref_to_clone(*this->m_tested_ref);
// 	Ref cloned_ref = Ref_base::do_copy(ref_to_clone);
// 	//Ref cloned_ref(*this->m_tested_ref);
// 	//cloned_ref.copy();

// }

TEST_F(DataRefAnyTest, newReferenceConstructor) {
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(1, Ref_base::get_content(*this->m_tested_ref)->m_owners);
	ASSERT_EQ(0, Ref_base::get_content(*this->m_tested_ref)->m_read_locks);
	ASSERT_EQ(0, Ref_base::get_content(*this->m_tested_ref)->m_write_locks);
}

TEST_F(DataRefAnyTest, copyConstructor) {
	Ref copied_ref(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref);
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, copied_ref);
	ASSERT_EQ(2, Ref_base::get_content(copied_ref)->m_owners);
	ASSERT_EQ(0, Ref_base::get_content(copied_ref)->m_read_locks);
	ASSERT_EQ(0, Ref_base::get_content(copied_ref)->m_write_locks);
}

TEST_F(DataRefAnyTest, moveConstructor) {
	Ref moved_ref(move(*this->m_tested_ref));
	ASSERT_TRUE(moved_ref);
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_EQ(nullptr, Ref_base::get_content(*this->m_tested_ref));
	ASSERT_EQ(1, Ref_base::get_content(moved_ref)->m_owners);
	ASSERT_EQ(0, Ref_base::get_content(moved_ref)->m_read_locks);
	ASSERT_EQ(0, Ref_base::get_content(moved_ref)->m_write_locks);
}

TEST_F(DataRefAnyTest, unlinkToDestroy) {
	this->m_tested_ref->reset();
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_FALSE(this->m_data);
}

TEST_F(DataRefAnyTest, unlinkButNotDestroy) {
	Ref copied_ref(*this->m_tested_ref);
	ASSERT_TRUE(*this->m_tested_ref);
	ASSERT_EQ(*this->m_tested_ref, copied_ref);
	ASSERT_EQ(2, Ref_base::get_content(copied_ref)->m_owners);

	this->m_tested_ref->reset();

	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref);
	ASSERT_TRUE(this->m_data);
	ASSERT_EQ(1, Ref_base::get_content(copied_ref)->m_owners);
	ASSERT_EQ(0, Ref_base::get_content(copied_ref)->m_read_locks);
	ASSERT_EQ(0, Ref_base::get_content(copied_ref)->m_write_locks);

	copied_ref.reset();

	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_FALSE(copied_ref);
	ASSERT_FALSE(this->m_data);
}

TEST_F(DataRefAnyTest, getAccess) {
	Ref_r copied_ref_r(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref_r);
	const void* ptr_r = copied_ref_r.get();
	ASSERT_EQ(&this->m_data, ptr_r);
	
	copied_ref_r.reset();

	Ref_rw copied_ref_rw(*this->m_tested_ref);
	ASSERT_TRUE(copied_ref_rw);
	void* ptr_rw = copied_ref_rw.get();
	ASSERT_EQ(&this->m_data, ptr_rw);
}

TEST_F(DataRefAnyTest, releaseTest) {
	ASSERT_EQ(&this->m_data, this->m_tested_ref->release());
	ASSERT_FALSE(*this->m_tested_ref);
	ASSERT_TRUE(this->m_data);
}

TEST_F(DataRefAnyTest, nullifyTest) {
	//the address to replace
	char c;
	void* address = &c;
	this->m_tested_ref->on_nullify([address](Ref whoCalled){ Ref_base::get_content(whoCalled)->m_buffer = address; });
	Ref otherRef(*this->m_tested_ref);
	void* recvAddress = otherRef.release();
	ASSERT_EQ(address, recvAddress);
	ASSERT_TRUE(this->m_data);
}
