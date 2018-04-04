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
#include <pdi/scalar_datatype.h>
#include <pdi/record_datatype.h>

#include <vector>

using namespace PDI;
using namespace std;

struct Record_interface {
	virtual const bool dense() = 0;
	virtual const size_t datasize() = 0;
	virtual const size_t buffersize() = 0;
	virtual const size_t alignment() = 0;
	virtual const Record_datatype* test_record() = 0;
};

/*
 * Struct prepared for SparseScalarsTest.
 * Order of the fields is important to test padding sizes.
 */
struct SparseScalarsTest : Record_interface {
	struct Sparse_structure {
		char c1;
		int i;
		char c2;
		unsigned int u;
		char c3;
		long l;
		char c4;
		unsigned long ul;
		char c5;
		float f;
		char c6;
		double d;
	};

	const bool dense() override
	{
		return false;
	}

	const size_t datasize() override
	{
		return 6 * sizeof(char) +
					sizeof(int) +
					sizeof(unsigned int) +
					sizeof(long) +
					sizeof(unsigned long) +
					sizeof(float) +
					sizeof(double);
	}

	const size_t buffersize() override
	{
		return sizeof(Sparse_structure);
	}

	const size_t alignment() override
	{
		return std::max({sizeof(char),
						sizeof(int),
						sizeof(unsigned int),
						sizeof(long),
						sizeof(unsigned long),
						sizeof(float),
						sizeof(double)});
	}
	const Record_datatype* test_record() override {
		return &m_test_record;
	}

	Record_datatype m_test_record {
		vector<Record_datatype::Member> {
			{offsetof(Sparse_structure, c1), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c1"},
			{offsetof(Sparse_structure, i), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}}, "i"},
			{offsetof(Sparse_structure, c2), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c2"},
			{offsetof(Sparse_structure, u), Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(unsigned int)}}, "u"},
			{offsetof(Sparse_structure, c3), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c3"},
			{offsetof(Sparse_structure, l), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}}, "l"},
			{offsetof(Sparse_structure, c4), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c4"},
			{offsetof(Sparse_structure, ul), Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(unsigned long)}}, "ul"},
			{offsetof(Sparse_structure, c5), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c5"},
			{offsetof(Sparse_structure, f), Datatype_uptr{new Scalar_datatype {Scalar_kind::FLOAT, sizeof(float)}}, "f"},
			{offsetof(Sparse_structure, c6), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(char)}}, "c6"},
			{offsetof(Sparse_structure, d), Datatype_uptr{new Scalar_datatype {Scalar_kind::FLOAT, sizeof(double)}}, "f"},
			
		},
		sizeof(Sparse_structure)
	};
};

struct DenseScalarsTest : Record_interface {
	struct Dense_structure {
		int i;
		unsigned int u;
		long l;
		unsigned long ul;
	};
	
	const bool dense() override
	{
		return true;
	}

	const size_t datasize() override
	{
		return sizeof(int) +
		    sizeof(unsigned int) +
		    sizeof(long) +
		    sizeof(unsigned long);
	}

	const size_t buffersize() override
	{
		return sizeof(Dense_structure);
	}

	const size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long)});
	}

	const Record_datatype* test_record() override {
		return &m_test_record;
	}

	Record_datatype m_test_record {
		vector<Record_datatype::Member> {
			{offsetof(Dense_structure, i), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}}, "i"},
			{offsetof(Dense_structure, u), Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(unsigned int)}}, "u"},
			{offsetof(Dense_structure, l), Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(long)}}, "l"},
			{offsetof(Dense_structure, ul), Datatype_uptr{new Scalar_datatype {Scalar_kind::UNSIGNED, sizeof(unsigned long)}}, "ul"}													
		},
		sizeof(Dense_structure)
	};
};

template <class T>
struct RecordDatatypeTest : public ::testing::Test {
	RecordDatatypeTest() : test_structure{new T} {}
	unique_ptr<Record_interface> test_structure;
};

typedef ::testing::Types<DenseScalarsTest, SparseScalarsTest> TypesForRecord;
TYPED_TEST_CASE(RecordDatatypeTest, TypesForRecord);

/*
 * Name:                RecordDatatypeTest/<structname>.assert_dense
 *
 * Tested functions:    none
 *
 * Description:         Test checks if record is dense.
 *
 */
TYPED_TEST(RecordDatatypeTest, assert_dense)
{
	if (is_same<TypeParam,DenseScalarsTest>::value) {
		ASSERT_EQ(this->test_structure->datasize(), this->test_structure->buffersize()) << "Struct of ints and longs is not dense. Can't test dense record";
	}
	if (is_same<TypeParam,SparseScalarsTest>::value) {
		ASSERT_LT(this->test_structure->datasize(), this->test_structure->buffersize()) << "Struct of chars, ints and floats is dense. Can't test sparse record";
	}
}

/*
 * Name:                RecordDatatypeTest/<structname>.check_dense
 *
 * Tested functions:    PDI::Record_datatype::dense()
 *
 * Description:         Test checks if record has correct dense value.
 *
 */
// ************		NOT SUPPORTED YET	  *******************
// TYPED_TEST(RecordDatatypeTest, check_dense)
// {
// 	if (is_same<TypeParam,DenseScalarsTest>::value) {
// 		ASSERT_TRUE(this->test_structure->test_record()->dense());
// 	}
// 	if (is_same<TypeParam,SparseScalarsTest>::value) {
// 		ASSERT_FALSE(this->test_structure->test_record()->dense());
// 	}
// }


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
    if (is_same<TypeParam,DenseScalarsTest>::value) {
		ASSERT_EQ(newRecord->datasize(), newRecord->buffersize());
		ASSERT_EQ(this->test_structure->datasize(), newRecord->datasize());
	}
	if (is_same<TypeParam,SparseScalarsTest>::value) {
		//TODO ASSERT_EQ(newRecord->datasize(), newRecord->buffersize());
		//TODO ASSERT_EQ(this->test_structure->datasize(), newRecord->buffersize());
	}
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
	//TODO ASSERT_EQ(this->test_structure->dense(), newRecord->dense());
	ASSERT_EQ(this->test_structure->test_record()->members().size(), newRecord->members().size());
}

