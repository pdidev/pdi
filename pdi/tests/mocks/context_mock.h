/*******************************************************************************
 * Copyright (C) 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_CONTEXT_MOCK_H_
#define PDI_CONTEXT_MOCK_H_

#include <memory>
#include <gmock/gmock.h>
#include <pdi/context.h>
#include <pdi/datatype_template.h>
#include <pdi/plugin.h>

struct MockContext: public PDI::Context {
	MOCK_METHOD(PDI::Data_descriptor&, desc, (const std::string&), (override));
	MOCK_METHOD(PDI::Data_descriptor&, desc, (const char*), (override));

	MOCK_METHOD(PDI::Data_descriptor&, BracketOp1, (const std::string&));

	PDI::Data_descriptor& operator[] (const std::string& str) override { return BracketOp1(str); }

	MOCK_METHOD(PDI::Data_descriptor&, BracketOp2, (const char*));

	PDI::Data_descriptor& operator[] (const char* str) override { return BracketOp2(str); }

	MOCK_METHOD(PDI::Context::Iterator, begin, (), (override));
	MOCK_METHOD(PDI::Context::Iterator, end, (), (override));
	MOCK_METHOD(PDI::Context::Iterator, find, (const std::string& name), (override));

	MOCK_METHOD(void, event, (const char*), (override));

	MOCK_METHOD(PDI::Logger&, logger, (), (override));

	MOCK_METHOD(PDI::Datatype_template_sptr, datatype, (PC_tree_t), (override));
	MOCK_METHOD(void, add_datatype, (const std::string&, Datatype_template_parser), (override));

	MOCK_METHOD(std::function<void()>, on_init, (const std::function<void()>& callback), (override));
	MOCK_METHOD(
		std::function<void()>,
		on_data,
		(const std::function<void(const std::string&, PDI::Ref)>& callback, const std::string& name),
		(override)
	);
	MOCK_METHOD(
		std::function<void()>,
		on_data_remove,
		(const std::function<void(const std::string&, PDI::Ref)>& callback, const std::string& name),
		(override)
	);
	MOCK_METHOD(std::function<void()>, on_event, (const std::function<void(const std::string&)>& callback, const std::string& name), (override));
	MOCK_METHOD(
		std::function<void()>,
		on_missing_data,
		(const std::function<void(const std::string&)>& callback, const std::string& name),
		(override)
	);
};


#endif //PDI_CONTEXT_MOCK_H_
