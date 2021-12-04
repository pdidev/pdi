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

#include <pdi/pdi_fwd.h>
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>

#include "PDI_record_datatype_cases.h"

using namespace PDI;
using namespace std;

template <class T>
struct RecordDatatypeTest : public ::testing::Test {
	RecordDatatypeTest() : test_structure{new T} {}
	virtual ~RecordDatatypeTest() = default;
	unique_ptr<T> test_structure;
};

typedef ::testing::Types<AlignedScalarsTest,
        NotAlignedScalarsTest,
        DenseArrayScalarsTest,
        SparseArrayScalarsTest,
        DenseRecordsInRecordTest,
        SparseRecordsInRecordTest> TypesForRecord;
TYPED_TEST_CASE(RecordDatatypeTest, TypesForRecord);

/*
 * Name:                RecordDatatypeTest/<structname>.check_dense
 *
 * Tested functions:    PDI::Record_datatype::dense()
 *
 * Description:         Test checks if record has correct dense value.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_dense)
{
	ASSERT_EQ(this->test_structure->dense(), this->test_structure->test_record()->dense());
}


/*
 * Name:                RecordDatatypeTest/<structname>.check_buffersize
 *
 * Tested functions:    PDI::Record_datatype::buffersize()
 *
 * Description:         Test checks if record has correct buffersize.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_buffersize)
{
	ASSERT_EQ(this->test_structure->buffersize(), this->test_structure->test_record()->buffersize());
}


/*
 * Name:                RecordDatatypeTest/<structname>.check_datasize
 *
 * Tested functions:    PDI::Record_datatype::datasize()
 *
 * Description:         Test checks if record has correct datasize.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_datasize)
{
	ASSERT_EQ(this->test_structure->datasize(), this->test_structure->test_record()->datasize());
}

/*
 * Name:                RecordDatatypeTest/<structname>.check_alignment
 *
 * Tested functions:    PDI::Record_datatype::alignment()
 *
 * Description:         Test checks if record has correct alignment.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_alignment)
{
	ASSERT_EQ(this->test_structure->alignment(), this->test_structure->test_record()->alignment());
}

/*
 * Name:                RecordDatatypeTest/<structname>.check_densify
 *
 * Tested functions:    PDI::Record_datatype::densify()
 *
 * Description:         Test checks if dense record is created.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_densify)
{
	Datatype_sptr newRecord {this->test_structure->test_record()->densify()};
	ASSERT_EQ(this->test_structure->datasize(), newRecord->datasize());
	ASSERT_EQ(this->test_structure->buffersize_after_densify(), newRecord->buffersize());
}

/*
 * Struct prepared for RecordDeepCopyTest.
 *
 */
struct RecordDeepCopyTest : public ::testing::Test {

	struct Dense_record_t {
		int my_int_array[9];
		char my_char;
		long my_long_array[10];
	};
	struct Sparse_record_t {
		int my_int_array[25];
		char my_char;
		long my_long_array[100];
	};
	int copied_scalar;
	
	RecordDeepCopyTest()
	{
		for (int i = 0; i < 9; i++) {
			dense_record.my_int_array[i] = i;
		}
		dense_record.my_char = 9;
		for (int i = 0; i < 10; i++) {
			dense_record.my_long_array[i] = i + 10;
		}
		
		for (int i = 0; i < 25; i++) {
			sparse_record.my_int_array[i] = 0;
		}
		sparse_record.my_char = 0;
		for (int i = 0; i < 100; i++) {
			sparse_record.my_long_array[i] = 0;
		}
	}
	
	Dense_record_t dense_record;
	Sparse_record_t sparse_record;
	
	Datatype_sptr datatype {
		Record_datatype::make(
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)),
							5,
							1,
							3
						),
						5,
						1,
						3
					),
					"my_int_array"
				},
				Record_datatype::Member{
					100,
					Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char)),
					"my_char"
				},
				Record_datatype::Member{
					104,
					Array_datatype::make(
						Array_datatype::make(
							Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)),
							10,
							2,
							5
						),
						10,
						5,
						2
					),
					"my_long_array"
				}
			},
			908
		)
	};
	
};

/*
 * Name:                RecordDeepCopyTest.dense_to_sparse
 *
 * Tested functions:    PDI::ScalarDatatype::data_sparse_copy()
 *
 * Description:         Test checks if correct copy is returned
 *
 */
TEST_F(RecordDeepCopyTest, dense_to_sparse)
{
	this->datatype->data_from_dense_copy(&this->sparse_record, &this->dense_record);
	for (int i = 1; i < 4; i++) {
		for (int j = 1; j < 4; j++) {
			ASSERT_EQ(this->dense_record.my_int_array[3*(i-1) + j-1], this->sparse_record.my_int_array[i*5 + j]);
		}
	}
	ASSERT_EQ(this->dense_record.my_char, this->sparse_record.my_char);
	for (int i = 5; i < 7; i++) {
		for (int j = 2; j < 7; j++) {
			ASSERT_EQ(this->dense_record.my_long_array[5*(i-5) + j-2], this->sparse_record.my_long_array[i*10 + j]);
		}
	}
}

/*
 * Name:                RecordAccessSequenceTest.invalid_subtype_access_sequence_check
 *
 * Tested functions:    PDI::Record_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(RecordAccessSequenceTest, invalid_subtype_access_sequence_check)
{

	struct Simple_record_t {
		int m_array[5];
		char m_char;
		long m_long;
	};
	
	Simple_record_t simple_record;
	
	for (int i = 0; i < 5; i++) {
		simple_record.m_array[i] = i;
	}
	
	simple_record.m_char = 5;
	simple_record.m_long = 987654;
	std::vector<Record_datatype::Member> members;
	auto&& array_data = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 5);
	auto&& char_data = Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char));
	auto&& long_data = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long));
	
	members.emplace_back(offsetof(Simple_record_t, m_array), array_data, "array");
	members.emplace_back(offsetof(Simple_record_t, m_char), char_data, "char");
	members.emplace_back(offsetof(Simple_record_t, m_long), long_data, "long");
	
	auto&& record_type = Record_datatype::make(std::move(members),sizeof(Simple_record_t));
	
	std::pair<void*, Datatype_sptr> data = record_type->member("array", &simple_record);
	ASSERT_EQ(*array_data, *data.second);
	for (int i = 0; i < 5 ; i++) {
		ASSERT_EQ(simple_record.m_array[i], static_cast<int*>(data.first)[i]);
	}
	
	data = record_type->member("char", &simple_record);
	ASSERT_EQ(*char_data, *data.second);
	ASSERT_EQ(&simple_record.m_char, data.first);
	
	data = record_type->member("long", &simple_record);
	ASSERT_EQ(*long_data, *data.second);
	ASSERT_EQ(&simple_record.m_long, data.first);
}

/*
 * Name:                RecordAccessSequenceTest.subtype_and_value_check_for_record_of_arrays_with_pointers
 *
 * Tested functions:    PDI::Record_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(RecordAccessSequenceTest, subtype_and_value_check_for_record_of_arrays_with_pointers)
{
	struct Simple_record_t {
		int* m_array[10];
	};
	
	Simple_record_t simple_record;
	
	for (int i = 0; i < 10; i++) {
		simple_record.m_array[i] = new int;
		*simple_record.m_array[i] = i;
	}
	
	std::vector<Record_datatype::Member> members;
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	auto&& pointer_type = Pointer_datatype::make(scalar_type);
	auto&& array_type = Array_datatype::make(pointer_type, 10);
	members.emplace_back(offsetof(Simple_record_t, m_array), array_type, "array");
	auto&& record_type = Record_datatype::make(std::move(members), sizeof(Simple_record_t));
		
	std::pair<void*, Datatype_sptr> data = record_type->member("array", &simple_record);
	ASSERT_EQ(simple_record.m_array, data.first);
	ASSERT_EQ(*array_type, *data.second);
	for (int i = 0; i < 10 ; i++) {
		ASSERT_EQ(*simple_record.m_array[i], *(static_cast<int**>(data.first)[i]));
	}
	
	data = data.second->index(3, data.first);
	data = data.second->dereference(data.first);
	ASSERT_EQ(3, *static_cast<int*>(data.first));
	ASSERT_EQ(*scalar_type, *data.second);
	
	for (int i = 0; i < 10; i++) {
		delete simple_record.m_array[i];
	}
}
