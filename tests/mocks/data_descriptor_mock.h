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

#ifndef PDI_DATA_DESCRIPTOR_MOCK_H_
#define PDI_DATA_DESCRIPTOR_MOCK_H_

#include <gmock/gmock.h>
#include <pdi/data_descriptor.h>

struct MockDataDescriptor : public PDI::Data_descriptor {
	MOCK_METHOD1(creation_template, void(PC_tree_t));
	MOCK_CONST_METHOD0(config, PC_tree_t());
	MOCK_CONST_METHOD0(metadata, bool());
	MOCK_METHOD1(metadata, void(bool));
	MOCK_CONST_METHOD0(name, const std::string&());
	MOCK_METHOD0(ref, PDI::Ref());
	MOCK_METHOD0(empty, bool());
	MOCK_METHOD3(share, void(void*, bool, bool));
	MOCK_METHOD3(share, void* (PDI::Ref, bool, bool));
	MOCK_METHOD0(release, void());
	MOCK_METHOD0(reclaim, void* ());
};


#endif //PDI_DATA_DESCRIPTOR_MOCK_H_
