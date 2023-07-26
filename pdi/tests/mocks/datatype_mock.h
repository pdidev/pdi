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

#ifndef PDI_DATATYPE_MOCK_H_
#define PDI_DATATYPE_MOCK_H_

#include <gmock/gmock.h>

#include <pdi/context.h>
#include <pdi/datatype.h>

struct MockDatatype : public PDI::Datatype {
	MockDatatype():
		PDI::Datatype()
	{}
	
	bool operator == (const Datatype& other) const override
	{
		return equals(other);
	}
	
	MOCK_CONST_METHOD1(equals, bool(const PDI::Datatype&));
	MOCK_CONST_METHOD0(densify, PDI::Datatype_sptr());
	MOCK_CONST_METHOD0(dense, bool());
	MOCK_CONST_METHOD0(datasize, size_t());
	MOCK_CONST_METHOD0(buffersize, size_t());
	MOCK_CONST_METHOD0(alignment, size_t());
	MOCK_CONST_METHOD0(simple, bool());
	MOCK_CONST_METHOD2(data_to_dense_copy, void* (void* to, const void* from));
	MOCK_CONST_METHOD2(data_from_dense_copy, void* (void* to, const void* from));
	MOCK_METHOD(PDI::Datatype_sptr, index, (size_t), (const, override));
	MOCK_METHOD((std::pair<void*,PDI::Datatype_sptr>), index, (size_t,void*), (const, override));
	MOCK_METHOD(PDI::Datatype_sptr, slice, (size_t, size_t), (const, override));
	MOCK_METHOD((std::pair<void*,PDI::Datatype_sptr>), slice, (size_t,size_t,void*), (const, override));
	MOCK_METHOD(PDI::Datatype_sptr, member, (const char*), (const, override));
	MOCK_METHOD((std::pair<void*,PDI::Datatype_sptr>), member, (const char*,void*), (const, override));
	MOCK_METHOD(PDI::Datatype_sptr, dereference, (), (const, override));
	MOCK_METHOD((std::pair<void*,PDI::Datatype_sptr>), dereference, (void*), (const, override));
	MOCK_METHOD(void, destroy_data, (void*), (const, override));
	MOCK_METHOD(std::string, debug_string, (), (const, override));
	MOCK_CONST_METHOD1(evaluate, PDI::Datatype_sptr(PDI::Context&));
};

#endif //PDI_DATATYPE_MOCK_H_
