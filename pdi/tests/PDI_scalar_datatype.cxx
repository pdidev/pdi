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

#include <pdi/scalar_datatype.h>

#include "mocks/context_mock.h"

using namespace PDI;
using namespace std;

/*
 * Struct prepared for ScalarDatatypeTest.
 */
template <typename T>
struct ScalarDatatypeTest : public ::testing::Test {
	//set kind and size depending on type
	ScalarDatatypeTest()
	{
		if (is_same<T,int>::value || is_same<T,long>::value) {
			test_kind = Scalar_kind::SIGNED;
		} else if (is_same<T,unsigned int>::value || is_same<T,unsigned long>::value) {
			test_kind = Scalar_kind::UNSIGNED;
		} else if (is_same<T,float>::value || is_same<T,double>::value) {
			test_kind = Scalar_kind::FLOAT;
		}
		test_size = sizeof(T);
		test_scalar = unique_ptr<Scalar_datatype>(new Scalar_datatype {test_kind, test_size});
	}
	
	//kind used to create Scalar_datatype
	Scalar_kind test_kind ;
	
	//size used to create Scalar_datatype
	size_t test_size;
	
	unique_ptr<Scalar_datatype> test_scalar;
};

typedef ::testing::Types<int, long, unsigned int, unsigned long, float, double> TypesForScalar;
TYPED_TEST_CASE(ScalarDatatypeTest, TypesForScalar);

/*
 * Name:                ScalarDatatypeTest/<typename>.check_kind
 *
 * Tested functions:    PDI::Scalar_datatype::kind()
 *
 * Description:         Test checks if correct kind is returned.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_kind)
{
	ASSERT_EQ(this->test_kind, this->test_scalar->kind());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.datasize
 *
 * Tested functions:    PDI::Scalar_datatype::datasize()
 *
 * Description:         Test checks if correct datasize is returned.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_datasize)
{
	ASSERT_EQ(this->test_size, this->test_scalar->datasize());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.buffersize
 *
 * Tested functions:    PDI::Scalar_datatype::buffersize()
 *
 * Description:         Test checks if correct buffersize is returned.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_buffersize)
{
	ASSERT_EQ(this->test_size, this->test_scalar->buffersize());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_align_size
 *
 * Tested functions:    PDI::Scalar_datatype::alignment()
 *
 * Description:         Test checks if correct alignment size is returned.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_align_size)
{
	ASSERT_EQ(this->test_size, this->test_scalar->alignment());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_dense
 *
 * Tested functions:    PDI::Scalar_datatype::dense()
 *
 * Description:         Test checks if correct density is returned.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_dense)
{
	ASSERT_EQ(true, this->test_scalar->dense());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_clone_type
 *
 * Tested functions:    PDI::Scalar_datatype::clone_type()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_clone_type)
{
	Datatype_uptr cloned_datatype {this->test_scalar->clone_type()};
	
	//need to cast to unique_ptr<Scalar_datatype> to get the kind()
	unique_ptr<Scalar_datatype> cloned_scalar {static_cast<Scalar_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_scalar->kind(), cloned_scalar->kind());
	ASSERT_EQ(this->test_scalar->datasize(), cloned_scalar->datasize());
	ASSERT_EQ(this->test_scalar->buffersize(), cloned_scalar->buffersize());
	ASSERT_EQ(this->test_scalar->alignment(), cloned_scalar->alignment());
	ASSERT_EQ(this->test_scalar->dense(), cloned_scalar->dense());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_clone
 *
 * Tested functions:    PDI::Scalar_datatype::clone()
 *
 * Description:         Test checks if correct clone is created.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_clone)
{
	Datatype_template_uptr cloned_datatype {this->test_scalar->clone()};
	
	//need to cast to unique_ptr<Scalar_datatype> to get the kind()
	MockContext mockCtx;
	unique_ptr<Scalar_datatype> cloned_scalar {static_cast<Scalar_datatype*>(cloned_datatype->evaluate(mockCtx).release())};
	ASSERT_EQ(this->test_scalar->kind(), cloned_scalar->kind());
	ASSERT_EQ(this->test_scalar->datasize(), cloned_scalar->datasize());
	ASSERT_EQ(this->test_scalar->buffersize(), cloned_scalar->buffersize());
	ASSERT_EQ(this->test_scalar->alignment(), cloned_scalar->alignment());
	ASSERT_EQ(this->test_scalar->dense(), cloned_scalar->dense());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_evaluate
 *
 * Tested functions:    PDI::Scalar_datatype::evaluate()
 *
 * Description:         Test checks if correct clone is created on evaluation.
 *
 */
TYPED_TEST(ScalarDatatypeTest, check_evaluate)
{
	MockContext mockCtx;
	Datatype_uptr cloned_datatype {this->test_scalar->evaluate(mockCtx)};
	
	//need to cast to unique_ptr<Scalar_datatype> to get the kind()
	unique_ptr<Scalar_datatype> cloned_scalar {static_cast<Scalar_datatype*>(cloned_datatype.release())};
	ASSERT_EQ(this->test_scalar->kind(), cloned_scalar->kind());
	ASSERT_EQ(this->test_scalar->datasize(), cloned_scalar->datasize());
	ASSERT_EQ(this->test_scalar->buffersize(), cloned_scalar->buffersize());
	ASSERT_EQ(this->test_scalar->alignment(), cloned_scalar->alignment());
	ASSERT_EQ(this->test_scalar->dense(), cloned_scalar->dense());
}

/*
 * Struct prepared for ScalarDeepCopyTest.
 *
 */
struct ScalarDeepCopyTest : public ::testing::Test {

	int data_scalar;
	int copied_scalar;
	
	ScalarDeepCopyTest()
	{
		data_scalar = 123;
		copied_scalar = 0;
	}
	
	Datatype_uptr datatype {new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)}};
	
};

/*
 * Name:                ScalarDeepCopyTest.dense_to_sparse
 *
 * Tested functions:    PDI::ScalarDatatype::data_sparse_copy()
 *
 * Description:         Test checks if correct copy is returned
 *
 */
TEST_F(ScalarDeepCopyTest, dense_to_sparse)
{
	this->datatype->data_from_dense_copy(&copied_scalar, &data_scalar);
	ASSERT_EQ(copied_scalar, data_scalar);
}