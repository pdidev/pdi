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

#ifndef PDI_DATATYPE_MOCK_H_
#define PDI_DATATYPE_MOCK_H_

#include <gmock/gmock.h>

#include <pdi/datatype.h>
#include <pdi/pdi_fwd.h>

struct MockDatatype : public PDI::Datatype {
	PDI::Datatype_template_uptr clone() const override
	{
		return PDI::Datatype_template_uptr{clone_proxy()};
	}
	
	PDI::Datatype_uptr clone_type() const override
	{
		return PDI::Datatype_uptr{clone_type_proxy()};
	}
	
	PDI::Datatype_uptr densify() const override
	{
		return PDI::Datatype_uptr{densify_proxy()};
	}
	
	PDI::Datatype_uptr evaluate(PDI::Context&) const override
	{
		return PDI::Datatype_uptr{evaluate_proxy()};
	}
	
	bool operator == (const Datatype& other) const
	{
		return equals(other);
	}
	
	MOCK_CONST_METHOD0(dense, bool());
	MOCK_CONST_METHOD0(datasize, size_t());
	MOCK_CONST_METHOD0(buffersize, size_t());
	MOCK_CONST_METHOD0(alignment, size_t());
	
	//proxies are needed to work with unique_ptr<>
	MOCK_CONST_METHOD0(clone_type_proxy, Datatype*());
	MOCK_CONST_METHOD0(clone_proxy, Datatype_template*());
	MOCK_CONST_METHOD0(densify_proxy, Datatype*());
	MOCK_CONST_METHOD0(evaluate_proxy, Datatype*());
	
	MOCK_CONST_METHOD0(simple, bool());
	MOCK_CONST_METHOD2(data_to_dense_copy, void* (void* to, const void* from));
	MOCK_CONST_METHOD2(data_from_dense_copy, void* (void* to, const void* from));
	MOCK_CONST_METHOD1(destroy_data, void(void* ptr));
	MOCK_CONST_METHOD1(equals, bool(const Datatype&));
	MOCK_CONST_METHOD0(debug_string, std::string());
};

#endif //PDI_DATATYPE_MOCK_H_
