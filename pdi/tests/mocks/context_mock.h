/*
 * SPDX-FileCopyrightText: 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_CONTEXT_MOCK_H_
#define PDI_CONTEXT_MOCK_H_

#include <memory>
#include <gmock/gmock.h>
#include <pdi/callbacks.h>
#include <pdi/context.h>
#include <pdi/datatype_template.h>
#include <pdi/plugin.h>

struct MockContext: public PDI::Context {
	MOCK_METHOD1(desc, PDI::Data_descriptor&(const std::string&));
	MOCK_METHOD1(desc, PDI::Data_descriptor&(const char*));

	MOCK_METHOD1(BracketOp1, PDI::Data_descriptor&(const std::string&));

	PDI::Data_descriptor& operator[] (const std::string& str) override { return BracketOp1(str); }

	MOCK_METHOD1(BracketOp2, PDI::Data_descriptor&(const char*));

	PDI::Data_descriptor& operator[] (const char* str) override { return BracketOp2(str); }

	MOCK_METHOD0(begin, PDI::Context::Iterator());
	MOCK_METHOD0(end, PDI::Context::Iterator());
	MOCK_METHOD1(find, PDI::Context::Iterator(const std::string& name));

	MOCK_METHOD1(event, void(const char*));

	MOCK_METHOD0(logger, PDI::Logger&());

	MOCK_METHOD1(datatype, PDI::Datatype_template_sptr(PC_tree_t));
	MOCK_METHOD2(add_datatype, void(const std::string&, Datatype_template_parser));
	MOCK_METHOD0(callbacks, PDI::Callbacks&());
	MOCK_METHOD0(finalize_and_exit, void());
};


#endif //PDI_CONTEXT_MOCK_H_
