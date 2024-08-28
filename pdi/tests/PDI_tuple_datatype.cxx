/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <pdi/tuple_datatype.h>

#include "PDI_tuple_datatype_cases.h"

using namespace PDI;
using namespace std;

template <class T>
struct TupleDatatypeTest: public ::testing::Test {
	TupleDatatypeTest()
		: test_structure{new T}
	{}

	unique_ptr<T> test_structure;
};

typedef ::testing::Types<
	TupleAlignedScalarsTest,
	TupleNotAlignedScalarsTest,
	DenseArrayScalarsTest,
	TupleSparseArrayScalarsTest,
	DenseTuplesInTupleTest,
	SparseTuplesInTupleTest>
	TypesForTuple;
TYPED_TEST_CASE(TupleDatatypeTest, TypesForTuple);

/*
 * Name:                TupleDatatypeTest/<structname>.check_dense
 *
 * Tested functions:    PDI::Tuple_datatype::dense()
 *
 * Description:         Test checks if tuple has correct dense value.
 *
 */
TYPED_TEST(TupleDatatypeTest, check_dense)
{
	ASSERT_EQ(this->test_structure->dense(), this->test_structure->test_tuple()->dense());
}

/*
 * Name:                TupleDatatypeTest/<structname>.check_buffersize
 *
 * Tested functions:    PDI::Tuple_datatype::buffersize()
 *
 * Description:         Test checks if tuple has correct buffersize.
 *
 */
TYPED_TEST(TupleDatatypeTest, check_buffersize)
{
	ASSERT_EQ(this->test_structure->buffersize(), this->test_structure->test_tuple()->buffersize());
}

/*
 * Name:                TupleDatatypeTest/<structname>.check_datasize
 *
 * Tested functions:    PDI::Tuple_datatype::datasize()
 *
 * Description:         Test checks if tuple has correct datasize.
 *
 */
TYPED_TEST(TupleDatatypeTest, check_datasize)
{
	ASSERT_EQ(this->test_structure->datasize(), this->test_structure->test_tuple()->datasize());
}

/*
 * Name:                TupleDatatypeTest/<structname>.check_alignment
 *
 * Tested functions:    PDI::Tuple_datatype::alignment()
 *
 * Description:         Test checks if tuple has correct alignment.
 *
 */
TYPED_TEST(TupleDatatypeTest, check_alignment)
{
	ASSERT_EQ(this->test_structure->alignment(), this->test_structure->test_tuple()->alignment());
}

/*
 * Name:                TupleDatatypeTest/<structname>.check_densify
 *
 * Tested functions:    PDI::Tuple_datatype::densify()
 *
 * Description:         Test checks if dense tuple is created.
 *
 */
TYPED_TEST(TupleDatatypeTest, check_densify)
{
	Datatype_sptr newTuple{this->test_structure->test_tuple()->densify()};
	ASSERT_EQ(this->test_structure->datasize(), newTuple->datasize());
	ASSERT_EQ(this->test_structure->buffersize_after_densify(), newTuple->buffersize());
}

/*
 * Struct prepared for TupleDeepCopyTest.
 *
 */
struct TupleDeepCopyTest: public ::testing::Test {
	struct Dense_tuple_t {
		int my_int_array[9];
		char my_char;
		long my_long_array[10];
	};

	struct Sparse_tuple_t {
		int my_int_array[25];
		char my_char;
		long my_long_array[100];
	};

	int copied_scalar;

	TupleDeepCopyTest()
	{
		for (int i = 0; i < 9; i++) {
			dense_tuple.my_int_array[i] = i;
		}
		dense_tuple.my_char = 9;
		for (int i = 0; i < 10; i++) {
			dense_tuple.my_long_array[i] = i + 10;
		}

		for (int i = 0; i < 25; i++) {
			sparse_tuple.my_int_array[i] = 0;
		}
		sparse_tuple.my_char = 0;
		for (int i = 0; i < 100; i++) {
			sparse_tuple.my_long_array[i] = 0;
		}
	}

	Dense_tuple_t dense_tuple;
	Sparse_tuple_t sparse_tuple;

	Datatype_sptr datatype = Tuple_datatype::make(
		vector<Tuple_datatype::Element>{
			Tuple_datatype::Element{
				0,
				Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 5, 1, 3), 5, 1, 3)
			},
			Tuple_datatype::Element{100, Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char))},
			Tuple_datatype::Element{
				104,
				Array_datatype::make(Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long)), 10, 2, 5), 10, 5, 2)
			}
		},
		908
	);
};

/*
 * Name:                TupleDeepCopyTest.dense_to_sparse
 *
 * Tested functions:    PDI::ScalarDatatype::data_sparse_copy()
 *
 * Description:         Test checks if correct copy is returned
 *
 */
TEST_F(TupleDeepCopyTest, dense_to_sparse)
{
	this->datatype->data_from_dense_copy(&this->sparse_tuple, &this->dense_tuple);
	for (int i = 1; i < 4; i++) {
		for (int j = 1; j < 4; j++) {
			ASSERT_EQ(this->dense_tuple.my_int_array[3 * (i - 1) + j - 1], this->sparse_tuple.my_int_array[i * 5 + j]);
		}
	}
	ASSERT_EQ(this->dense_tuple.my_char, this->sparse_tuple.my_char);
	for (int i = 5; i < 7; i++) {
		for (int j = 2; j < 7; j++) {
			ASSERT_EQ(this->dense_tuple.my_long_array[5 * (i - 5) + j - 2], this->sparse_tuple.my_long_array[i * 10 + j]);
		}
	}
}

/*
 * Name:                TupleAccessSequenceTest.invalid_subtype_access_sequence_check
 *
 * Tested functions:    PDI::Tuple_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(TupleAccessSequenceTest, invalid_subtype_access_sequence_check)
{
	struct Simple_tuple_t {
		int m_array[5];
		char m_char;
		long m_long;
	};

	Simple_tuple_t simple_tuple;

	for (int i = 0; i < 5; i++) {
		simple_tuple.m_array[i] = i;
	}

	simple_tuple.m_char = 5;
	simple_tuple.m_long = 987654;
	std::vector<Tuple_datatype::Element> elements;
	auto&& array_data = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 5);
	auto&& char_data = Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char));
	auto&& long_data = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long));

	elements.emplace_back(offsetof(Simple_tuple_t, m_array), array_data);
	elements.emplace_back(offsetof(Simple_tuple_t, m_char), char_data);
	elements.emplace_back(offsetof(Simple_tuple_t, m_long), long_data);

	auto&& tuple_type = Tuple_datatype::make(std::move(elements), sizeof(Simple_tuple_t));

	std::pair<void*, Datatype_sptr> data = tuple_type->index(0, &simple_tuple);
	ASSERT_EQ(*array_data, *data.second);
	for (int i = 0; i < 5; i++) {
		ASSERT_EQ(simple_tuple.m_array[i], static_cast<int*>(data.first)[i]);
	}

	data = tuple_type->index(1, &simple_tuple);
	ASSERT_EQ(*char_data, *data.second);
	ASSERT_EQ(&simple_tuple.m_char, data.first);

	data = tuple_type->index(2, &simple_tuple);
	ASSERT_EQ(*long_data, *data.second);
	ASSERT_EQ(&simple_tuple.m_long, data.first);
}

/*
 * Name:                TupleAccessSequenceTest.subtype_and_value_check_for_tuple_of_arrays_with_pointers
 *
 * Tested functions:    PDI::Tuple_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(TupleAccessSequenceTest, subtype_and_value_check_for_tuple_of_arrays_with_pointers)
{
	struct Simple_tuple_t {
		int* m_array[10];
	};

	Simple_tuple_t simple_tuple;

	for (int i = 0; i < 10; i++) {
		simple_tuple.m_array[i] = new int;
		*simple_tuple.m_array[i] = i;
	}

	std::vector<Tuple_datatype::Element> elements;
	auto&& scalar_type = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int));
	auto&& pointer_type = Pointer_datatype::make(scalar_type);
	auto&& array_type = Array_datatype::make(pointer_type, 10);
	elements.emplace_back(offsetof(Simple_tuple_t, m_array), array_type);
	auto&& tuple_type = Tuple_datatype::make(std::move(elements), sizeof(Simple_tuple_t));

	std::pair<void*, Datatype_sptr> data = tuple_type->index(0, &simple_tuple);
	ASSERT_EQ(simple_tuple.m_array, data.first);
	ASSERT_EQ(*array_type, *data.second);
	for (int i = 0; i < 10; i++) {
		ASSERT_EQ(*simple_tuple.m_array[i], *(static_cast<int**>(data.first)[i]));
	}

	data = data.second->index(3, data.first);
	data = data.second->dereference(data.first);
	ASSERT_EQ(3, *static_cast<int*>(data.first));
	ASSERT_EQ(*scalar_type, *data.second);

	for (int i = 0; i < 10; i++) {
		delete simple_tuple.m_array[i];
	}
}

/*
 * Name:                TupleAccessSequenceTest.slice_access_check
 *
 * Tested functions:    PDI::Tuple_datatype::subaccess
 *
 * Description:         Test checks if returned subtype and data are correct
 */
TEST(TupleSliceAccessSequenceTest, slice_access_check)
{
	struct Simple_tuple_t {
		int m_array[5];
		char m_char;
		long m_long;
	};

	struct Result_tuple_t {
		char m_char;
		long m_long;
	};

	Simple_tuple_t simple_tuple;

	for (int i = 0; i < 5; i++) {
		simple_tuple.m_array[i] = i;
	}

	simple_tuple.m_char = 5;
	simple_tuple.m_long = 987654;
	std::vector<Tuple_datatype::Element> elements;
	auto&& array_data = Array_datatype::make(Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)), 5);
	auto&& char_data = Scalar_datatype::make(Scalar_kind::UNSIGNED, sizeof(char));
	auto&& long_data = Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(long));

	elements.emplace_back(offsetof(Simple_tuple_t, m_array), array_data);
	elements.emplace_back(offsetof(Simple_tuple_t, m_char), char_data);
	elements.emplace_back(offsetof(Simple_tuple_t, m_long), long_data);

	auto&& tuple_type = Tuple_datatype::make(std::move(elements), sizeof(Simple_tuple_t));

	std::pair<void*, Datatype_sptr> data = tuple_type->slice(1, 3, &simple_tuple);

	std::vector<Tuple_datatype::Element> result_elements;
	result_elements.emplace_back(0, char_data);
	result_elements.emplace_back(offsetof(Simple_tuple_t, m_long) - offsetof(Simple_tuple_t, m_char), long_data);
	auto&& result_type = Tuple_datatype::make(std::move(result_elements), sizeof(Simple_tuple_t) - offsetof(Simple_tuple_t, m_char));

	ASSERT_EQ(&simple_tuple.m_char, data.first);
	ASSERT_EQ(*result_type, *data.second) << result_type->debug_string() << "\n\n" << data.second->debug_string();

	data = result_type->index(0, &simple_tuple.m_char);
	ASSERT_EQ(*char_data, *data.second) << char_data->debug_string() << "\n\n" << data.second->debug_string();
	ASSERT_EQ(data.first, &simple_tuple.m_char);

	data = result_type->index(1, &simple_tuple.m_char);
	ASSERT_EQ(*long_data, *data.second) << long_data->debug_string() << "\n\n" << data.second->debug_string();
	ASSERT_EQ(data.first, &simple_tuple.m_long);
}
