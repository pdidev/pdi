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

#include <pdi/record_datatype.h>

using namespace PDI;
using namespace std;

/*
 * Struct prepared for RecordDatatypeEmptyTest.
 */
struct EmptyStructure {};

struct RecordDatatypeEmptyTest : public ::testing::Test {
	RecordDatatypeEmptyTest()
	{
		test_size = sizeof(EmptyStructure);
		
		vector<Record_datatype::Member> test_members;
		test_record = unique_ptr<Record_datatype> {new Record_datatype(move(test_members), test_size)};
	}
	
	//size used to create Record_datatype
	size_t test_size;
	
	unique_ptr<Record_datatype> test_record;
};

/*
 * Name:                RecordDatatypeEmptyTest.check_fields
 *
 * Tested functions:    PDI::Record_datatype::buffersize()
 *                      PDI::Record_datatype::datasize()
 *                      PDI::Record_datatype::alignment()
 *
 * Description:         Test checks if correct buffersize is returned.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_fields)
{
	//sizeof empty struct is 1 byte
	ASSERT_EQ(1, this->test_record->buffersize());
	ASSERT_EQ(0, this->test_record->datasize());
	ASSERT_EQ(0, this->test_record->alignment());
}

/*
 * Name:                RecordDatatypeEmptyTest.check_if_empty_members
 *
 * Tested functions:    PDI::Record_datatype::members()
 *
 * Description:         Test checks if members() return empty vector.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_if_empty_members)
{
	ASSERT_EQ(true, this->test_record->members().empty());
}

/*
 * Name:                RecordDatatypeEmptyTest.check_clone_type
 *
 * Tested functions:    PDI::Record_datatype::clone_type()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_clone_type)
{
	Datatype_uptr cloned_datatype {this->test_record->clone_type()};
	
	//need to cast to unique_ptr<Record_datatype> to get the members()
	Record_datatype* ptr {static_cast<Record_datatype*>(cloned_datatype.release())};
	unique_ptr<Record_datatype> cloned_record {ptr};
	
	ASSERT_EQ(true, this->test_record->members().empty());
	ASSERT_EQ(true, cloned_record->members().empty());
	ASSERT_EQ(this->test_record->datasize(), cloned_record->datasize());
	ASSERT_EQ(this->test_record->buffersize(), cloned_record->buffersize());
	ASSERT_EQ(this->test_record->alignment(), cloned_record->alignment());
}

/*
 * Name:                RecordDatatypeEmptyTest.check_clone
 *
 * Tested functions:    PDI::Record_datatype::clone()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_clone)
{
	Datatype_template_uptr cloned_datatype {this->test_record->clone()};
	
	//need to cast to unique_ptr<Record_datatype> to get the members()
	Record_datatype* ptr {static_cast<Record_datatype*>(cloned_datatype.release())};
	unique_ptr<Record_datatype> cloned_record {ptr};
	
	ASSERT_EQ(this->test_record->datasize(), cloned_record->datasize());
	ASSERT_EQ(this->test_record->buffersize(), cloned_record->buffersize());
	ASSERT_EQ(this->test_record->alignment(), cloned_record->alignment());
}

/*
 * Name:                RecordDatatypeEmptyTest.check_densify
 *
 * Tested functions:    PDI::Record_datatype::densify()
 *
 * Description:         Test checks if densified empty record is also empty.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_densify)
{
	Datatype_uptr densified_uptr = this->test_record->densify();
	
	//need to cast to unique_ptr<Record_datatype> to get the members()
	Record_datatype* ptr {static_cast<Record_datatype*>(densified_uptr.release())};
	unique_ptr<Record_datatype> densified_record {ptr};
	
	ASSERT_EQ(true, this->test_record->members().empty());
	ASSERT_EQ(true, densified_record->members().empty());
	ASSERT_EQ(this->test_record->datasize(), densified_record->datasize());
	ASSERT_EQ(this->test_record->buffersize(), densified_record->buffersize());
	ASSERT_EQ(this->test_record->alignment(), densified_record->alignment());
}

/*
 * Name:                RecordDatatypeEmptyTest.check_evaluate
 *
 * Tested functions:    PDI::Record_datatype::evaluate()
 *
 * Description:         Test checks if correct clone is created on evaluation.
 *
 */
TEST_F(RecordDatatypeEmptyTest, check_evaluate)
{
	//just need something for evalute function (not used)
	Context* context;
	Datatype_uptr cloned_datatype {this->test_record->evaluate(*context)};
	
	//need to cast to unique_ptr<Record_datatype> to get the members()
	Record_datatype* ptr {static_cast<Record_datatype*>(cloned_datatype.release())};
	unique_ptr<Record_datatype> densified_record {ptr};
	
	ASSERT_EQ(true, this->test_record->members().empty());
	ASSERT_EQ(true, densified_record->members().empty());
	ASSERT_EQ(this->test_record->datasize(), densified_record->datasize());
	ASSERT_EQ(this->test_record->buffersize(), densified_record->buffersize());
	ASSERT_EQ(this->test_record->alignment(), densified_record->alignment());
}
