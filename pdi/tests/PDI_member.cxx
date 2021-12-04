/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
struct ScalarMemberTest: ::testing::Test {
	
	//displacement of member
	size_t test_displacement = 8;
	
	Datatype_sptr mocked_datatype = std::make_shared<MockDatatype>();
	
	//name of the member
	std::string test_name = "intMember";
	
	Record_datatype::Member test_member = {test_displacement, mocked_datatype, test_name};
};

/*
 * Name:                ScalarMemberTest.check_fields
 *
 * Tested functions:    PDI::Record_datatype::Member::Member(size_t displacement, Datatype_sptr type, const string& name)
 *
 * Description:         Test checks values of fields.
 *
 */
TEST_F(ScalarMemberTest, check_fields)
{
	ASSERT_EQ(test_displacement, test_member.displacement());
	ASSERT_EQ(test_name, test_member.name());
	ASSERT_EQ(mocked_datatype, test_member.type());
}


/*
 * Name:                ScalarMemberTest.check_copy_constructor 
 *
 * Tested functions:    PDI::Record_datatype::Member::Member(const Member&)
 *
 * Description:         Test copy constructor.
 *
 */
TEST_F(ScalarMemberTest, check_copy_constructor)
{
	Record_datatype::Member copy {test_member};
	ASSERT_EQ(copy.displacement(), test_member.displacement());
	ASSERT_EQ(copy.name(), test_member.name());
	ASSERT_EQ(copy.type(), test_member.type());
}
