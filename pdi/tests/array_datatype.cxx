/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
using std::make_shared;
using std::shared_ptr;
using std::static_pointer_cast;
using ::testing::Return;

namespace PDI {
inline void PrintTo(Datatype_sptr dt, ::std::ostream* os)
{
	*os << dt->debug_string();
}
} // namespace PDI

/*
 * Struct prepared for ArrayDatatypeTest.
 */
struct ArrayDatatypeTest: public ::testing::Test {
	shared_ptr<MockDatatype> test_subtype = std::make_shared<MockDatatype>();
	size_t test_size = 10;
	size_t test_start = 0;
	size_t test_subsize = 5;
	shared_ptr<Array_datatype> test_array = Array_datatype::make(test_subtype, test_size, test_start, test_subsize);
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
	EXPECT_EQ(this->test_size, this->test_array->size());
	EXPECT_EQ(this->test_start, this->test_array->start());
	EXPECT_EQ(this->test_subsize, this->test_array->subsize());
	EXPECT_EQ(this->test_subtype, this->test_array->subtype());
}

/*
 * Name:                ArrayDatatypeTest.check_less_args
 *
 * Tested functions:    PDI::ArrayDatatypeTest::Array_datatype(Datatype_sptr, size_t)
 *
 * Description:         Test checks if constructor with less arguments correctly
 *                      assign values to other fields.
 *
 */
TEST_F(ArrayDatatypeTest, check_less_args)
{
	size_t size = 10;
	auto&& array = Array_datatype::make(Datatype_sptr{new MockDatatype()}, size);
	EXPECT_EQ(size, array->size());
	EXPECT_EQ(0, array->start());
	EXPECT_EQ(size, array->subsize());
}

/*
 * Name:                ArrayDatatypeTest.check_densify
 *
 * Tested functions:    PDI::ArrayDatatypeTest::densify()
 *
 * Description:         Test checks if correct densified version is created.
 *
 */
TEST_F(ArrayDatatypeTest, check_densify)
{
	EXPECT_CALL(*test_subtype, densify()).WillOnce(Return(test_subtype));
	auto&& densified_array = static_pointer_cast<const Array_datatype>(this->test_array->densify());

	EXPECT_EQ(test_array->subsize(), densified_array->size());
	EXPECT_EQ(test_array->start(), densified_array->start());
	EXPECT_EQ(test_array->subsize(), densified_array->subsize());
	EXPECT_EQ(test_array->subtype(), densified_array->subtype());

	testing::Mock::VerifyAndClearExpectations(test_subtype.get());
}

/*
 * Name:                ArrayDatatypeTest.check_evaluate
 *
 * Tested functions:    PDI::ArrayDatatypeTest::evaluate()
 *
 * Description:         Test checks if correct evaluation is created.
 *
 */
TEST_F(ArrayDatatypeTest, check_evaluate)
{
	MockContext mockCtx;
	auto&& evaluated_array = static_pointer_cast<const Array_datatype>(this->test_array->evaluate(mockCtx));

	EXPECT_EQ(this->test_array->size(), evaluated_array->size());
	EXPECT_EQ(this->test_array->start(), evaluated_array->start());
	EXPECT_EQ(this->test_array->subsize(), evaluated_array->subsize());
	EXPECT_EQ(this->test_array->subtype(), evaluated_array->subtype());
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
	EXPECT_CALL(*this->test_subtype, dense()).Times(0);
	EXPECT_EQ(false, this->test_array->dense());
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
	auto&& dense_array = Array_datatype::make(Datatype_sptr(mocksparse), 10);
	EXPECT_EQ(false, dense_array->dense());
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
	auto&& dense_array = Array_datatype::make(Datatype_sptr(mocksparse), 10);
	EXPECT_EQ(true, dense_array->dense());
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
	EXPECT_CALL(*this->test_subtype, datasize()).WillOnce(Return((subtype_datasize)));
	EXPECT_EQ(subtype_datasize * this->test_subsize, this->test_array->datasize());
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
	EXPECT_CALL(*this->test_subtype, buffersize()).WillOnce(Return(subtype_buffersize));
	EXPECT_EQ(subtype_buffersize * this->test_size, this->test_array->buffersize());
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
	EXPECT_CALL(*this->test_subtype, alignment()).WillOnce(Return(subtype_alignment));
	EXPECT_EQ(subtype_alignment, this->test_array->alignment());
}

/*
 * Struct prepared for SparseArrayDeepCopyTest.
 *
 */
struct SparseArrayDeepCopyTest: public ::testing::Test {
	int* sparse_array{new int[100]}; //buffer: 10 x 10; data: 4 x 4; start: (3, 3)
	int* dense_array{new int[16]};

	SparseArrayDeepCopyTest()
	{
		for (int i = 0; i < 100; i++) {
			sparse_array[i] = 0;
		}
		for (int i = 0; i < 16; i++) {
			dense_array[i] = i;
		}
	}

	Datatype_sptr datatype{Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 10, 3, 4), 10, 3, 4)};
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
			EXPECT_EQ(dense_array[(i - 3) * 4 + j - 3], sparse_array[10 * i + j]);
		}
	}
}

/*
 * Struct prepared for ArrayAccessSequenceTest.
 *
 */
struct ArrayAccessSequenceTest: public ::testing::Test {
	std::shared_ptr<Array_datatype> m_array_type;
	std::unique_ptr<int[]> m_array;

	ArrayAccessSequenceTest()
		: m_array_type{Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 16)}
		, m_array{new int[16]}
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
	std::pair<void*, Datatype_sptr> data = m_array_type->index(5, m_array.get());
	EXPECT_EQ(m_array.get() + 5, data.first);
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	EXPECT_EQ(*scalar_type, *data.second);
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
	std::pair<void*, Datatype_sptr> data = m_array_type->slice(0, 3, m_array.get()); //"[0:3]"
	EXPECT_EQ(m_array.get(), data.first);
	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 3);
	EXPECT_EQ(*array_test, *data.second);
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
	std::pair<void*, Datatype_sptr> data = m_array_type->slice(5, 7, m_array.get());
	EXPECT_EQ(m_array.get() + 5, data.first);
	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 2);
	EXPECT_EQ(*array_test, *data.second);
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
	std::pair<void*, Datatype_sptr> data = m_array_type->slice(12, 16, m_array.get());
	EXPECT_EQ(m_array.get() + 12, data.first);
	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 4);
	EXPECT_EQ(*array_test, *data.second);
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

	auto&& sparse_array_type = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 16, 2, 4);

	std::pair<void*, Datatype_sptr> data = sparse_array_type->slice(0, 2, sparse_array);
	EXPECT_EQ(sparse_array + 2, data.first);
}

/*
 * Struct prepared for ArrayAccessAdvancedSequenceTest.
 *
 */
struct ArrayAccessAdvancedSequenceTest: public ::testing::Test {
	static constexpr size_t s_size[2]{8, 8};
	static constexpr size_t s_start[2]{1, 1};
	static constexpr size_t s_subsize[2]{4, 4};
	std::shared_ptr<Array_datatype> m_sparse_matrix_type = Array_datatype::make(
		Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), s_size[1], s_start[1], s_subsize[1]),
		s_size[0],
		s_start[0],
		s_subsize[0]
	);
	std::unique_ptr<int[]> m_sparse_matrix{new int[s_size[0] * s_size[1]]};
	int* m_matrix_data_start = m_sparse_matrix.get() + s_size[1] * s_start[0] + s_start[0];

	ArrayAccessAdvancedSequenceTest()
	{
		for (int i = 0; i < 64; i++) {
			m_sparse_matrix[i] = i;
		}
	}
};

constexpr size_t ArrayAccessAdvancedSequenceTest::s_size[2];
constexpr size_t ArrayAccessAdvancedSequenceTest::s_start[2];
constexpr size_t ArrayAccessAdvancedSequenceTest::s_subsize[2];

/*
 * Name:                ArrayAccessAdvancedSequenceTest.access_sequence_subtype_sparse_data
 *
 * Tested functions:    PDI::Array_datatype::get_subtype_ptr
 *
 * Description:         Test checks if passing sparse 2D array return correct subtype and values.
 */
TEST_F(ArrayAccessAdvancedSequenceTest, access_sequence_subtype_sparse_data)
{
	constexpr size_t idx0 = 1;
	constexpr std::pair<size_t, size_t> slc1 = {2, 4};

	std::pair<void*, Datatype_sptr> data = m_sparse_matrix_type->index(idx0, m_sparse_matrix.get());
	data = data.second->slice(slc1.first, slc1.second, data.first);

	const void* expected_address = m_matrix_data_start + idx0 * s_size[1] + slc1.first;
	EXPECT_EQ(expected_address, data.first) << static_cast<uint8_t*>(data.first) - static_cast<const uint8_t*>(expected_address) << " bytes off";

	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), slc1.second - slc1.first);
	EXPECT_EQ(*array_test, *data.second);
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
	constexpr size_t idx0 = 1;
	constexpr std::pair<size_t, size_t> slc1 = {2, 4};

	std::pair<void*, Datatype_sptr> data = m_sparse_matrix_type->index(idx0, m_sparse_matrix.get());
	data = data.second->slice(slc1.first, slc1.second, data.first);

	const void* expected_address = m_matrix_data_start + idx0 * s_size[1] + slc1.first;
	EXPECT_EQ(expected_address, data.first) << static_cast<uint8_t*>(data.first) - static_cast<const uint8_t*>(expected_address) << " bytes off";

	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), slc1.second - slc1.first);
	EXPECT_EQ(*array_test, *data.second);
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
	std::pair<void*, Datatype_sptr> data = m_sparse_matrix_type->index(1, m_sparse_matrix.get());
	EXPECT_EQ(m_sparse_matrix.get() + 16, data.first);
	auto&& array_test = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 8, 1, 4);
	EXPECT_EQ(*array_test, *data.second);
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
	std::pair<void*, Datatype_sptr> data{m_sparse_matrix.get(), m_sparse_matrix_type};
	data = data.second->index(0, data.first);
	data = data.second->index(0, data.first);
	EXPECT_EQ(m_sparse_matrix.get() + 9, data.first);
	auto&& scalar_test = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	EXPECT_EQ(*scalar_test, *data.second);
}
