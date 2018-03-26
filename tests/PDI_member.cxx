#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <string>
#include <map>
#include <stddef.h>


#include <pdi.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/pdi_fwd.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

using namespace PDI;
using ::testing::Return;

struct MockDatatype : public Datatype {
	Datatype_template_uptr clone() const override
	{
		return Datatype_template_uptr{clone_proxy()};
	}
	
	Datatype_uptr clone_type() const override
	{
		return Datatype_uptr{clone_type_proxy()};
	}
	
	Datatype_uptr densify() const override
	{
		return Datatype_uptr{densify_proxy()};
	}
	
	Datatype_uptr evaluate(Context&) const override
	{
		return Datatype_uptr{evaluate_proxy()};
	}
	
	MOCK_CONST_METHOD0(dense, bool());
	MOCK_CONST_METHOD0(datasize, size_t());
	MOCK_CONST_METHOD0(buffersize, size_t());
	MOCK_CONST_METHOD0(alignment, size_t());
	
	MOCK_CONST_METHOD0(clone_type_proxy, MockDatatype*());
	MOCK_CONST_METHOD0(clone_proxy, MockDatatype*());
	MOCK_CONST_METHOD0(densify_proxy, MockDatatype*());
	MOCK_CONST_METHOD0(evaluate_proxy, MockDatatype*());
};

/*
 * Member is int with displacement = 8 and named "intMember"
 */
struct ScalarMemberTest : public ::testing::Test {
	ScalarMemberTest()
	{
		test_displacement = 8;
		test_name = "intMember";
		test_member = std::unique_ptr<Record_datatype::Member> {new Record_datatype::Member(test_displacement, Datatype_uptr{mocked_datatype}, test_name)};
	}
	
	//displacement of member
	size_t test_displacement;
	
	MockDatatype* mocked_datatype {new MockDatatype()};
	
	//name of the member
	std::string test_name;
	
	std::unique_ptr<Record_datatype::Member> test_member;
	Record_datatype::Member* test_member_copy;
};

/*
 * Name:                ScalarMemberTest.check_fields
 *
 * Tested functions:    Record_datatype::Member::Member(size_t displacement, Datatype_uptr type, const string& name)
 *
 * Description:         Test checks values of fields.
 *
 */
TEST_F(ScalarMemberTest, check_fields)
{
	ASSERT_EQ(8, test_member->displacement());
	ASSERT_EQ(this->test_name, test_member->name());
	ASSERT_EQ(this->mocked_datatype, &test_member->type());
}


/*
 * Name:                ScalarMemberTest.check_clone_constructor
 *
 * Tested functions:    Record_datatype::Member::Member(const Member&)
 *
 * Description:         Test checks clone constructor.
 *
 */
TEST_F(ScalarMemberTest, check_clone_constructor)
{
	MockDatatype* cloned_type {new MockDatatype()};
	EXPECT_CALL(*mocked_datatype, clone_type_proxy())
	.WillOnce(Return(cloned_type));
	
	Record_datatype::Member cloned {*test_member};
	ASSERT_EQ(cloned.displacement(), test_member->displacement());
	ASSERT_EQ(cloned.name(), test_member->name());
	ASSERT_EQ(&cloned.type(), cloned_type);
}
