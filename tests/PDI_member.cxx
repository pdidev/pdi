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

#include "mocks/datatype_mock.h"

using namespace PDI;
using ::testing::Return;

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
 * Tested functions:    PDI::Record_datatype::Member::Member(size_t displacement, Datatype_uptr type, const string& name)
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
 * Tested functions:    PDI::Record_datatype::Member::Member(const Member&)
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
