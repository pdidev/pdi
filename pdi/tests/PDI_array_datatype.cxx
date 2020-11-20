/*******************************************************************************
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

#include <gtest/gtest.h>

#include <pdi/array_datatype.h>
#include <pdi/scalar_datatype.h>

#include "mocks/context_mock.h"
#include "mocks/datatype_mock.h"

using namespace PDI;
using ::testing::Return;

/*
 * Struct prepared for ArrayDatatypeTest.
 */
struct ArrayDatatypeTest : public ::testing::Test {
	ArrayDatatypeTest():
		mockDatatype {new MockDatatype()},
	             test_size{10},
	             test_start{0},
	             test_subsize{5},
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
	MockContext mockCtx;
	std::unique_ptr<Array_datatype> cloned_array {static_cast<Array_datatype*>(cloned_datatype->evaluate(mockCtx).release())};
	ASSERT_EQ(this->test_array.size(), cloned_array->size());
	ASSERT_EQ(this->test_array.start(), cloned_array->start());
	ASSERT_EQ(this->test_array.subsize(), cloned_array->subsize());
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
	MockDatatype* new_datatype = new MockDatatype();
	EXPECT_CALL(*this->mockDatatype, clone_type_proxy()).WillOnce(Return(new_datatype));
	
	MockContext mockCtx;
	Datatype_uptr cloned_datatype {this->test_array.evaluate(mockCtx)};
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
	EXPECT_CALL(*this->mockDatatype, buffersize()).WillOnce(Return(subtype_buffersize));
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


/*
 * Struct prepared for SparseArrayDeepCopyTest.
 *
 */
struct SparseArrayDeepCopyTest : public ::testing::Test {

	int* sparse_array {new int[100]}; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)
	int* dense_array {new int[16]};
	
	SparseArrayDeepCopyTest()
	{
		for (int i = 0; i < 100; i++) {
			sparse_array[i] = 0;
		}
		for (int i = 0; i < 16; i++) {
			dense_array[i] = i;
		}
	}
	
	Datatype_uptr datatype {
		new Array_datatype
		{
			Datatype_uptr {
				new Array_datatype
				{
					Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}},
					10,
					3,
					4
				}
			},
			10,
			3,
			4
		}
	};
};

/*
 * Name:                SparseArrayDeepCopyTest.dense_to_sparse
 *
 * Tested functions:    PDI::ArrayDatatypeTest::data_sparse_copy()
 *
 * Description:         Test checks if correct copy is returned
 *
 */
TEST_F(SparseArrayDeepCopyTest, dense_to_sparse)
{
	this->datatype->data_from_dense_copy(sparse_array, dense_array);
	for (int i = 3; i < 7; i++) {
		for (int j = 3; j < 7; j++) {
			ASSERT_EQ(dense_array[(i-3)*4 + j-3], sparse_array[10*i + j]);
		}
	}
}

/*
 * Struct prepared for ArrayAccessSequenceTest.
 *
 */
struct ArrayAccessSequenceTest : public ::testing::Test {

	Array_datatype m_array_type;
	std::unique_ptr<int[]> m_array;
	
	ArrayAccessSequenceTest():
		m_array_type{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 16 },
	m_array{new int[16]}
	{
		for (int i = 0; i < 16; i++) {
			m_array[i] = i;
		}
	}
};

/*
 * Name:                ArrayAccessSequenceTest.access_single_element
 *
 * Tested functions:    PDI::Array_datatype::subaccess
 *
 * Description:         Test checks if passing array return correct subtype and value.
 */
TEST_F(ArrayAccessSequenceTest, access_single_element)
{
	std::pair<void*, Datatype_uptr> data = m_array_type.subaccess(m_array.get(), Array_datatype::Index_accessor{5});
	ASSERT_EQ(m_array[5], *static_cast<int*>(data.first));
	
	ASSERT_NE(m_array_type, *data.second);
	Scalar_datatype scalar_type {Scalar_kind::SIGNED,sizeof(int)};
	ASSERT_EQ(scalar_type, *data.second);
}

/*
 * Name:                ArrayAccessSequenceTest.access_sequence_subtype_begin
 *
 * Tested functions:    PDI::Array_datatype::subaccess
 *
 * Description:         Test checks if passing array return correct subtype and values.
 */
TEST_F(ArrayAccessSequenceTest, access_sequence_subtype_begin)
{
	std::pair<void*, Datatype_uptr> data = m_array_type.subaccess(m_array.get(), Array_datatype::Slice_accessor{0, 3}); //"[0:3]"
	for (int i = 0; i < 3; i++) {
		ASSERT_EQ(m_array[i], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_array_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 3 };
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessSequenceTest.access_sequence_subtype_middle
 *
 * Tested functions:    PDI::Array_datatype::subaccess
 *
 * Description:         Test checks if passing array return correct subtype and values.
 */
TEST_F(ArrayAccessSequenceTest, access_sequence_subtype_middle)
{
	std::pair<void*, Datatype_uptr> data = m_array_type.subaccess(m_array.get(), Array_datatype::Slice_accessor{5, 7});
	for (int i = 0; i < 2; i++) {
		ASSERT_EQ(m_array[i+5], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_array_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 2 };
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessSequenceTest.access_sequence_subtype_end
 *
 * Tested functions:    PDI::Array_datatype::subaccess
 *
 * Description:         Test checks if passing array return correct subtype and values.
 */
TEST_F(ArrayAccessSequenceTest, access_sequence_subtype_end)
{
	std::pair<void*, Datatype_uptr> data = m_array_type.subaccess(m_array.get(), Array_datatype::Slice_accessor{12, 16});
	for (int i = 0; i < 4; i++) {
		ASSERT_EQ(m_array[i+12], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_array_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 4 };
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessSimpleSequenceTest.access_sequence_subtype_sparse_data
 *
 * Tested functions:    PDI::Array_datatype::subaccess
 *
 * Description:         Test checks if passing sparse array return correct subtype and values.
 */
TEST(ArrayAccessSimpleSequenceTest, access_sequence_subtype_sparse_data)
{
	int sparse_array[16];
	{
		for (int i = 0; i < 16; i++) {
			sparse_array[i] = i;
		}
	}
	
	Array_datatype sparse_array_type{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 16, 2, 4};
	
	std::pair<void*, Datatype_uptr> data = sparse_array_type.subaccess(sparse_array, Array_datatype::Slice_accessor{0, 2});
	for (int i = 0; i < 2; i++) {
		ASSERT_EQ(sparse_array[i+2], static_cast<int*>(data.first)[i]);
	}
}

/*
 * Struct prepared for ArrayAccessAdvancedSequenceTest.
 *
 */
struct ArrayAccessAdvancedSequenceTest : public ::testing::Test {

	Array_datatype m_sparse_matrix_type;
	std::unique_ptr<int[]> m_sparse_matrix;
	
	ArrayAccessAdvancedSequenceTest():
		m_sparse_matrix_type{Datatype_uptr{new Array_datatype{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 8, 1, 4 }}, 8, 1, 4},
	m_sparse_matrix{new int[64]}
	{
		for (int i = 0; i < 64; i++) {
			m_sparse_matrix[i] = i;
		}
	}
};

/*
 * Name:                ArrayAccessAdvancedSequenceTest.access_sequence_subtype_sparse_data
 *
 * Tested functions:    PDI::Array_datatype::get_subtype_ptr
 *
 * Description:         Test checks if passing sparse 2D array return correct subtype and values.
 */
TEST_F(ArrayAccessAdvancedSequenceTest, access_sequence_subtype_sparse_data)
{
	std::pair<void*, Datatype_uptr> data {m_sparse_matrix.get(), m_sparse_matrix_type.clone_type()};
	std::vector<std::unique_ptr<Datatype::Accessor_base>> accessors;
	accessors.emplace_back(new Array_datatype::Index_accessor{0});
	accessors.emplace_back(new Array_datatype::Slice_accessor{0, 4});
	for (auto&& accessor : accessors) {
		data = data.second->subaccess(data.first, *accessor);
	}
	for (int i = 0; i < 4 ; i++) {
		ASSERT_EQ(m_sparse_matrix[i+9], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_sparse_matrix_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 4 };
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessAdvancedSequenceTest.access_sequence_subtype_sparse_data_vector
 *
 * Tested functions:    PDI::Array_datatype::get_subtype_ptr
 *
 * Description:         Test checks if passing sparse 2D array return correct subtype and values.
 */
TEST_F(ArrayAccessAdvancedSequenceTest, access_sequence_subtype_sparse_data_vector)
{
	std::pair<void*, Datatype_uptr> data {m_sparse_matrix.get(), m_sparse_matrix_type.clone_type()};
	std::vector<std::unique_ptr<Datatype::Accessor_base>> accessors;
	accessors.emplace_back(new Array_datatype::Index_accessor{0});
	accessors.emplace_back(new Array_datatype::Slice_accessor{0, 4});
	data = data.second->subaccess(data.first, accessors);
	for (int i = 0; i < 4 ; i++) {
		ASSERT_EQ(m_sparse_matrix[i+9], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_sparse_matrix_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 4 };
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessAdvancedSequenceTest.access_sequence_subtype_sparse_data_second
 *
 * Tested functions:    PDI::Array_datatype::get_subtype_ptr
 *
 * Description:         Test checks if passing sparse 2D array return correct subtype and values.
 */
TEST_F(ArrayAccessAdvancedSequenceTest, access_sequence_subtype_sparse_data_second)
{
	std::pair<void*, Datatype_uptr> data = m_sparse_matrix_type.subaccess(m_sparse_matrix.get(), Array_datatype::Index_accessor{1});
	for (int i = 0; i < 8 ; i++) {
		ASSERT_EQ(m_sparse_matrix[i+16], static_cast<int*>(data.first)[i]);
	}
	ASSERT_NE(m_sparse_matrix_type, *data.second);
	Array_datatype array_test{Datatype_uptr{new Scalar_datatype {Scalar_kind::SIGNED,sizeof(int)}}, 8, 1, 4};
	ASSERT_EQ(array_test, *data.second);
}

/*
 * Name:                ArrayAccessAdvancedSequenceTest.access_sequence_subtype_sparse_data_third
 *
 * Tested functions:    PDI::Array_datatype::get_subtype_ptr
 *
 * Description:         Test checks if passing sparse 2D array return correct subtype and values.
 */
TEST_F(ArrayAccessAdvancedSequenceTest, access_sequence_subtype_sparse_data_third)
{
	std::pair<void*, Datatype_uptr> data {m_sparse_matrix.get(), m_sparse_matrix_type.clone_type()};
	std::vector<std::unique_ptr<Datatype::Accessor_base>> accessors;
	data = data.second->subaccess(data.first, Array_datatype::Index_accessor{0});
	data = data.second->subaccess(data.first, Array_datatype::Index_accessor{0});
	ASSERT_EQ(m_sparse_matrix[9], static_cast<int*>(data.first)[0]);
	ASSERT_NE(m_sparse_matrix_type, *data.second);
	Scalar_datatype scalar_test{Scalar_kind::SIGNED, sizeof(int)};
	ASSERT_EQ(scalar_test, *data.second);
}
