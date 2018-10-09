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

#include <pdi/pdi_fwd.h>
#include <pdi/record_datatype.h>

#include "PDI_record_datatype_cases.h"

using namespace PDI;
using namespace std;

template <class T>
struct RecordDatatypeTest : public ::testing::Test {
	RecordDatatypeTest() : test_structure{new T} {}
	unique_ptr<Record_interface> test_structure;
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
	Datatype_uptr newRecord {this->test_structure->test_record()->densify()};
	ASSERT_EQ(this->test_structure->datasize(), newRecord->datasize());
	ASSERT_EQ(this->test_structure->buffersize_after_densify(), newRecord->buffersize());
}

/*
 * Name:                RecordDatatypeTest/<structname>.check_clone_type
 *
 * Tested functions:    PDI::Record_datatype::clone_type()
 *
 * Description:         Test checks if correct clone_typeis created.
 *
 */
TYPED_TEST(RecordDatatypeTest, check_clone_type)
{
	Datatype_uptr cloned_record {this->test_structure->test_record()->clone_type()};
	unique_ptr<Record_datatype> newRecord {static_cast<Record_datatype*>(cloned_record.release())};
	
	ASSERT_EQ(this->test_structure->test_record()->buffersize(), newRecord->buffersize());
	ASSERT_EQ(this->test_structure->test_record()->datasize(), newRecord->datasize());
	ASSERT_EQ(this->test_structure->test_record()->alignment(), newRecord->alignment());
	ASSERT_EQ(this->test_structure->dense(), newRecord->dense());
	ASSERT_EQ(this->test_structure->test_record()->members().size(), newRecord->members().size());
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
	
	Datatype_uptr datatype {
		new Record_datatype {
			vector<Record_datatype::Member> {
				Record_datatype::Member{
					0,
					unique_ptr<Datatype> {
						new Array_datatype
						{
							unique_ptr<Datatype> {
								new Array_datatype
								{
									unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
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
					"my_int_array"
				},
				Record_datatype::Member{
					100,
					unique_ptr<Datatype> { new Scalar_datatype{Scalar_kind::UNSIGNED, sizeof(char)} },
					"my_char"
				},
				Record_datatype::Member{
					104,
					unique_ptr<Datatype> {
						new Array_datatype
						{
							unique_ptr<Datatype> {
								new Array_datatype
								{
									unique_ptr<Datatype>{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}},
									10,
									2,
									5
								}
							},
							10,
							5,
							2,
						}
					},
					"my_long_array"
				}
			},
			908
		}
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