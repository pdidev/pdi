/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

using namespace PDI;
using namespace std;

/*
 * Struct prepared for PointerDatatypeTest.
 */
struct PointerDatatypeTest: public ::testing::Test {
	PointerDatatypeTest()
	{
		m_scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
		m_ptr_scalar_type = Pointer_datatype::make(m_scalar_type);

		m_array_type = PDI::Array_datatype::make(m_scalar_type, 32);
		m_ptr_array_type = Pointer_datatype::make(m_array_type);

		vector<Record_datatype::Member> members;
		members.emplace_back(0, m_scalar_type, "member_0");
		members.emplace_back(sizeof(int), m_array_type, "member_1");

		m_record_type = Record_datatype::make(std::move(members), 33 * sizeof(int));
		m_ptr_record_type = Pointer_datatype::make(m_record_type);
	}

	shared_ptr<Datatype> m_scalar_type;
	shared_ptr<Pointer_datatype> m_ptr_scalar_type;

	shared_ptr<Datatype> m_array_type;
	shared_ptr<Pointer_datatype> m_ptr_array_type;

	shared_ptr<Datatype> m_record_type;
	shared_ptr<Pointer_datatype> m_ptr_record_type;
};

/*
 * Name:                ScalarPointerDatatypeTest.scalar_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct scalar is returned.
 *
 */
TEST_F(PointerDatatypeTest, scalar_equality)
{
	if (*this->m_ptr_scalar_type->dereference() != *this->m_scalar_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_scalar_type->debug_string() << endl;
		cerr << "Scalar datatype:\n" << this->m_scalar_type->debug_string() << endl;
		FAIL();
	}
}

/*
 * Name:                PointerDatatypeTest.array_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct array is returned.
 *
 */
TEST_F(PointerDatatypeTest, array_equality)
{
	if (*this->m_ptr_array_type->dereference() != *this->m_array_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_array_type->debug_string() << endl;
		cerr << "Array datatype:\n" << this->m_array_type->debug_string() << endl;
		FAIL();
	}
}

/*
 * Name:                PointerDatatypeTest.record_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct record is returned.
 *
 */
TEST_F(PointerDatatypeTest, record_equality)
{
	if (*this->m_ptr_record_type->dereference() != *this->m_record_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_record_type->dereference()->debug_string() << endl;
		cerr << "Record datatype:\n" << this->m_record_type->debug_string() << endl;
		FAIL();
	}
}

/*
 * Name:                PointerAccessSequenceTest.pointer_to_record_with_pointer_to_array_of_scalars_subtype_check
 *
 * Tested functions:    PDI::pointer_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(PointerAccessSequenceTest, pointer_to_record_with_pointer_to_array_of_scalars_subtype_check)
{
	struct Simple_record_t {
		char m_char;
		int* m_pointer;
	};

	Simple_record_t simple_record;
	simple_record.m_pointer = new int[10];
	for (int i = 0; i < 10; i++) {
		simple_record.m_pointer[i] = i;
	}
	auto&& char_type = Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char));
	auto&& int_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	auto&& array_type = Array_datatype::make(int_type, 10);
	auto&& ptr_array_type = Pointer_datatype::make(array_type);

	std::vector<Record_datatype::Member> members;
	members.emplace_back(offsetof(Simple_record_t, m_char), char_type, "m_char");
	members.emplace_back(offsetof(Simple_record_t, m_pointer), ptr_array_type, "m_pointer");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Simple_record_t));
	auto&& ptr_record_type = Pointer_datatype::make(record_type);
	Simple_record_t* value_ptr = &simple_record;


	std::pair<void*, Datatype_sptr> data = ptr_record_type->dereference(&value_ptr);
	ASSERT_EQ(value_ptr, data.first);
	ASSERT_EQ(*record_type, *data.second);

	std::pair<void*, Datatype_sptr> char_data = ptr_record_type->member("m_char", &value_ptr);
	ASSERT_EQ(&simple_record.m_char, char_data.first);
	ASSERT_EQ(*char_type, *char_data.second);

	std::pair<void*, Datatype_sptr> ptr_data = data.second->member("m_pointer", data.first);
	ASSERT_EQ(&simple_record.m_pointer, ptr_data.first);
	ASSERT_EQ(*ptr_array_type, *ptr_data.second);

	std::pair<void*, Datatype_sptr> ptr_ptr_data = ptr_data.second->dereference(ptr_data.first);
	ASSERT_EQ(simple_record.m_pointer, ptr_ptr_data.first);
	ASSERT_EQ(*array_type, *ptr_ptr_data.second);

	std::pair<void*, Datatype_sptr> array_ptr_ptr_data = ptr_ptr_data.second->index(2, ptr_ptr_data.first);
	ASSERT_EQ(*int_type, *array_ptr_ptr_data.second);
	ASSERT_EQ(&(simple_record.m_pointer[2]), array_ptr_ptr_data.first);

	delete[] simple_record.m_pointer;
}
