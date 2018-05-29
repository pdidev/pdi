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

#include <PDI_record_datatype_cases.h>

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

