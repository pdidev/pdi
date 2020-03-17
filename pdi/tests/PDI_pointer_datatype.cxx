/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <pdi/pointer_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

using namespace PDI;
using namespace std;

/*
 * Struct prepared for PointerDatatypeTest.
 */
struct PointerDatatypeTest : public ::testing::Test {
	PointerDatatypeTest()
	{
		m_scalar_type.reset(new Scalar_datatype {Scalar_kind::SIGNED, sizeof(int)});
		m_ptr_scalar_type.reset(new Pointer_datatype{m_scalar_type->clone_type()});
		
		m_array_type.reset(new Array_datatype {m_scalar_type->clone_type(), 32});
		m_ptr_array_type.reset(new Pointer_datatype{m_array_type->clone_type()});
		
		vector<Record_datatype::Member> members;
		members.emplace_back(0, m_scalar_type->clone_type(), "member_0");
		members.emplace_back(sizeof(int), m_array_type->clone_type(), "member_1");
		
		m_record_type.reset(new Record_datatype {std::move(members), 33*sizeof(int)});
		m_ptr_record_type.reset(new Pointer_datatype{m_record_type->clone_type()});
		
	}
	
	unique_ptr<Datatype> m_scalar_type;
	unique_ptr<Pointer_datatype> m_ptr_scalar_type;
	
	unique_ptr<Datatype> m_array_type;
	unique_ptr<Pointer_datatype> m_ptr_array_type;
	
	unique_ptr<Datatype> m_record_type;
	unique_ptr<Pointer_datatype> m_ptr_record_type;
};

/*
 * Name:                ScalarPointerDatatypeTest.scalar_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct scalar is returned.
 *
 */
TEST_F(PointerDatatypeTest, scalar_equality)
{
	if (*this->m_ptr_scalar_type->dereference() != *this->m_scalar_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_scalar_type->debug_string() << endl;
		cerr << "Scalar datatype:\n" << this->m_scalar_type->debug_string() << endl;
		FAIL();
	}
}

/*
 * Name:                PointerDatatypeTest.array_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct array is returned.
 *
 */
TEST_F(PointerDatatypeTest, array_equality)
{
	if (*this->m_ptr_array_type->dereference() != *this->m_array_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_array_type->debug_string() << endl;
		cerr << "Array datatype:\n" << this->m_array_type->debug_string() << endl;
		FAIL();
	}
}

/*
 * Name:                PointerDatatypeTest.record_equality
 *
 * Tested functions:    PDI::Pointer_datatype::dereference()
 *
 * Description:         Test checks if correct record is returned.
 *
 */
TEST_F(PointerDatatypeTest, record_equality)
{
	if (*this->m_ptr_record_type->dereference() != *this->m_record_type) {
		cerr << "Pointer datatype:\n" << this->m_ptr_record_type->dereference()->debug_string() << endl;
		cerr << "Record datatype:\n" << this->m_record_type->debug_string() << endl;
		FAIL();
	}
}
