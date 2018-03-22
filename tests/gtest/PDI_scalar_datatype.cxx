#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <list>
#include <memory>
#include <type_traits>

#include <mpi.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/datatype_template.h>
#include <pdi/pdi_fwd.h>
#include <pdi/scalar_datatype.h>

/*
 * Class prepared for ScalarDatatypeTest.
 */
template <typename T>
struct ScalarDatatypeTest : public ::testing::Test
{
    //set kind and size depending on type
    ScalarDatatypeTest() {
        if (std::is_same<T,int>::value || std::is_same<T,long>::value) {
            test_kind = PDI::Scalar_kind::SIGNED;
        } else if (std::is_same<T,unsigned int>::value || std::is_same<T,unsigned long>::value) {
            test_kind = PDI::Scalar_kind::UNSIGNED;
        } else if (std::is_same<T,float>::value || std::is_same<T,double>::value) {
            test_kind = PDI::Scalar_kind::FLOAT;
        }
        test_size = sizeof(T);
        test_scalar = new PDI::Scalar_datatype(test_kind, test_size);
    }
    
    //kind used to create Scalar_datatype
    PDI::Scalar_kind test_kind ;
    
    //size used to create Scalar_datatype
    size_t test_size;
    
    PDI::Scalar_datatype* test_scalar;
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
    PDI::Datatype_uptr cloned_datatype {this->test_scalar->clone_type()};

    //need to cast to unique_ptr<PDI::Scalar_datatype> to get the kind()
    PDI::Scalar_datatype* ptr = static_cast<PDI::Scalar_datatype*>(cloned_datatype.release());
    std::unique_ptr<PDI::Scalar_datatype> cloned_scalar = std::unique_ptr<PDI::Scalar_datatype> {ptr};
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
    PDI::Datatype_template_uptr cloned_datatype {this->test_scalar->clone()};
    
    //need to cast to unique_ptr<PDI::Scalar_datatype> to get the kind()
    PDI::Scalar_datatype* ptr = static_cast<PDI::Scalar_datatype*>(cloned_datatype.release());
    std::unique_ptr<PDI::Scalar_datatype> cloned_scalar = std::unique_ptr<PDI::Scalar_datatype> {ptr};
    ASSERT_EQ(this->test_scalar->kind(), cloned_scalar->kind());
    ASSERT_EQ(this->test_scalar->datasize(), cloned_scalar->datasize());
    ASSERT_EQ(this->test_scalar->buffersize(), cloned_scalar->buffersize());
    ASSERT_EQ(this->test_scalar->alignment(), cloned_scalar->alignment());
    ASSERT_EQ(this->test_scalar->dense(), cloned_scalar->dense());
}

/*
 * Name:                ScalarDatatypeTest/<typename>.check_evaluate
 * 
 * Tested functions:    PDI::Scalar_datatype::check_evaluate()
 * 
 * Description:         Test checks if correct clone is created on evaluation.
 * 
 */
TYPED_TEST(ScalarDatatypeTest, check_evaluate)
{
    //just need something for evalute function (not used)
    PDI::Context* context;
    PDI::Datatype_uptr cloned_datatype {this->test_scalar->evaluate(*context)};

    //need to cast to unique_ptr<PDI::Scalar_datatype> to get the kind()
    PDI::Scalar_datatype* ptr = static_cast<PDI::Scalar_datatype*>(cloned_datatype.release());
    std::unique_ptr<PDI::Scalar_datatype> cloned_scalar = std::unique_ptr<PDI::Scalar_datatype> {ptr};
    ASSERT_EQ(this->test_scalar->kind(), cloned_scalar->kind());
    ASSERT_EQ(this->test_scalar->datasize(), cloned_scalar->datasize());
    ASSERT_EQ(this->test_scalar->buffersize(), cloned_scalar->buffersize());
    ASSERT_EQ(this->test_scalar->alignment(), cloned_scalar->alignment());
    ASSERT_EQ(this->test_scalar->dense(), cloned_scalar->dense());
}
