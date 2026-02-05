/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_PLUGIN_MOCK_H_
#define PDI_PLUGIN_MOCK_H_

#include <gmock/gmock.h>

#include <pdi/plugin.h>
#include <pdi/ref_any.h>

struct MockPlugin: public PDI::Plugin {
	MockPlugin(PDI::Context& ctx)
		: Plugin(ctx)
	{}

	MOCK_METHOD0(init, void());
	MOCK_METHOD1(event, void(const char*));
	MOCK_METHOD1(empty_desc_access, void(const char*));
	MOCK_METHOD2(data, void(const char*, PDI::Ref));
};

#endif //PDI_PLUGIN_MOCK_H_
