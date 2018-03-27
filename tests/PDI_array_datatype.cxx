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

#include <datatype_mock.h>

#include <pdi/array_datatype.h>

using namespace PDI;
using ::testing::Return;

/*
 * Struct prepared for ArrayDatatypeTest.
 */
struct ArrayDatatypeTest : public ::testing::Test {
	ArrayDatatypeTest():
		test_size{10},
		test_start{0},
		test_subsize{5},
		mockDatatype {new MockDatatype()},
	             test_array{Datatype_uptr(mockDatatype), test_size, test_start, test_subsize}
	{}
	MockDatatype* mockDatatype;
	size_t test_size;
	size_t test_start;
	size_t test_subsize;
	Array_datatype test_array;
};

/*
 * Name:                ArrayDatatypeTest.check_fields
 *
 * Tested functions:    PDI::ArrayDatatypeTest::size()
 *                      PDI::ArrayDatatypeTest::start()
 *                      PDI::ArrayDatatypeTest::subsize()
 *                      PDI::ArrayDatatypeTest::subtype()
 *
 * Description:         Test checks if these functions return correct values.
 *
 */
TEST_F(ArrayDatatypeTest, check_fields)
{
	ASSERT_EQ(this->test_size, this->test_array.size());
	ASSERT_EQ(this->test_start, this->test_array.start());
	ASSERT_EQ(this->test_subsize, this->test_array.subsize());
	ASSERT_EQ(static_cast<Datatype*>(this->mockDatatype), &this->test_array.subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_less_args
 *
 * Tested functions:    PDI::ArrayDatatypeTest::Array_datatype(Datatype_uptr, size_t)
 *
 * Description:         Test checks if constructor with less arguments correctly
 *                      assign values to other fields.
 *
 */
TEST_F(ArrayDatatypeTest, check_less_args)
{
	size_t size = 10;
	Array_datatype array{Datatype_uptr{new MockDatatype()}, size};
	ASSERT_EQ(size, array.size());
	ASSERT_EQ(0, array.start());
	ASSERT_EQ(size, array.subsize());
}

/*
 * Name:                ArrayDatatypeTest.check_clone
 *
 * Tested functions:    PDI::ArrayDatatypeTest::clone()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TEST_F(ArrayDatatypeTest, check_clone)
{
	MockDatatype* new_datatype = new MockDatatype();
	EXPECT_CALL(*this->mockDatatype, clone_type_proxy()).WillOnce(Return(new_datatype));
	
	Datatype_template_uptr cloned_datatype {this->test_array.clone()};
	std::unique_ptr<Array_datatype> cloned_array {static_cast<Array_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_array.size(), cloned_array->size());
	ASSERT_EQ(this->test_array.start(), cloned_array->start());
	ASSERT_EQ(this->test_array.subsize(), cloned_array->subsize());
	ASSERT_EQ(new_datatype, &cloned_array->subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_clone_type
 *
 * Tested functions:    PDI::ArrayDatatypeTest::clone_type()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TEST_F(ArrayDatatypeTest, check_clone_type)
{
	MockDatatype* new_datatype = new MockDatatype();
	EXPECT_CALL(*this->mockDatatype, clone_type_proxy()).WillOnce(Return(new_datatype));
	
	Datatype_uptr cloned_datatype {this->test_array.clone_type()};
	std::unique_ptr<Array_datatype> cloned_array {static_cast<Array_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_array.size(), cloned_array->size());
	ASSERT_EQ(this->test_array.start(), cloned_array->start());
	ASSERT_EQ(this->test_array.subsize(), cloned_array->subsize());
	ASSERT_EQ(new_datatype, &cloned_array->subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_densify
 *
 * Tested functions:    PDI::ArrayDatatypeTest::densify()
 *
 * Description:         Test checks if correct densified clone is created.
 *
 */
TEST_F(ArrayDatatypeTest, check_densify)
{
	MockDatatype* new_datatype = new MockDatatype();
	EXPECT_CALL(*this->mockDatatype, densify_proxy()).WillOnce(Return(new_datatype));
	
	Datatype_uptr cloned_datatype {this->test_array.densify()};
	std::unique_ptr<Array_datatype> cloned_array {static_cast<Array_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_array.subsize(), cloned_array->size());
	ASSERT_EQ(0, cloned_array->start());
	ASSERT_EQ(this->test_array.subsize(), cloned_array->subsize());
	ASSERT_EQ(new_datatype, &cloned_array->subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_evaluate
 *
 * Tested functions:    PDI::ArrayDatatypeTest::evaluate()
 *
 * Description:         Test checks if correct clone is created on evaluation.
 *
 */
TEST_F(ArrayDatatypeTest, check_evaluate)
{
	//just need something for evalute function (not used)
	PDI::Context* context;
	MockDatatype* new_datatype = new MockDatatype();
	EXPECT_CALL(*this->mockDatatype, clone_type_proxy()).WillOnce(Return(new_datatype));
	
	Datatype_uptr cloned_datatype {this->test_array.evaluate(*context)};
	std::unique_ptr<Array_datatype> cloned_array {static_cast<Array_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_array.size(), cloned_array->size());
	ASSERT_EQ(this->test_array.start(), cloned_array->start());
	ASSERT_EQ(this->test_array.subsize(), cloned_array->subsize());
	ASSERT_EQ(new_datatype, &cloned_array->subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_dense_false_array
 *
 * Tested functions:    PDI::ArrayDatatypeTest::dense()
 *
 * Description:         Test checks if correct density is returned,
 *                      when array is not dense.
 *
 */
TEST_F(ArrayDatatypeTest, check_dense_false_array)
{
	EXPECT_CALL(*this->mockDatatype, dense()).Times(0);
	ASSERT_EQ(false, this->test_array.dense());
}

/*
 * Name:                ArrayDatatypeTest.check_dense_false_subtype
 *
 * Tested functions:    PDI::ArrayDatatypeTest::dense()
 *
 * Description:         Test checks if correct density is returned,
 *                      when array is dense but subtype isn't.
 *
 */
TEST_F(ArrayDatatypeTest, check_dense_false_subtype)
{
	MockDatatype* mocksparse = new MockDatatype();
	EXPECT_CALL(*mocksparse, dense()).WillOnce(Return(false));
	Array_datatype dense_array {Datatype_uptr(mocksparse), 10};
	ASSERT_EQ(false, dense_array.dense());
}

/*
 * Name:                ArrayDatatypeTest.check_dense_true
 *
 * Tested functions:    PDI::ArrayDatatypeTest::dense()
 *
 * Description:         Test checks if correct density is returned,
 *                      when array and subtype are dense.
 *
 */
TEST_F(ArrayDatatypeTest, check_dense_true)
{
	MockDatatype* mocksparse = new MockDatatype();
	EXPECT_CALL(*mocksparse, dense()).WillOnce(Return(true));
	Array_datatype dense_array{Datatype_uptr(mocksparse), 10};
	ASSERT_EQ(true, dense_array.dense());
}

/*
 * Name:                ArrayDatatypeTest.datasize
 *
 * Tested functions:    PDI::ArrayDatatypeTest::datasize()
 *
 * Description:         Test checks if correct datasize is returned.
 *
 */
TEST_F(ArrayDatatypeTest, check_datasize)
{
	size_t subtype_datasize = 5;
	EXPECT_CALL(*this->mockDatatype, datasize()).WillOnce(Return((subtype_datasize)));
	ASSERT_EQ(subtype_datasize * this->test_subsize, this->test_array.datasize());
}

/*
 * Name:                ArrayDatatypeTest.buffersize
 *
 * Tested functions:    PDI::ArrayDatatypeTest::buffersize()
 *
 * Description:         Test checks if correct buffersize is returned.
 *
 */
TEST_F(ArrayDatatypeTest, check_buffersize)
{
	size_t subtype_buffersize = 10;
	EXPECT_CALL(*this->mockDatatype, datasize()).WillOnce(Return(subtype_buffersize));
	ASSERT_EQ(subtype_buffersize * this->test_size, this->test_array.buffersize());
}

/*
 * Name:                ArrayDatatypeTest.check_alignment
 *
 * Tested functions:    PDI::ArrayDatatypeTest::alignment()
 *
 * Description:         Test checks if correct alignment size is returned.
 *
 */
TEST_F(ArrayDatatypeTest, check_alignment)
{
	size_t subtype_alignment = 1;
	EXPECT_CALL(*this->mockDatatype, alignment()).WillOnce(Return(subtype_alignment));
	ASSERT_EQ(subtype_alignment, this->test_array.alignment());
}