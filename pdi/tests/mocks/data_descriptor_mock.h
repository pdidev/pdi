/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_DATA_DESCRIPTOR_MOCK_H_
#define PDI_DATA_DESCRIPTOR_MOCK_H_

#include <gmock/gmock.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype_template.h>

struct MockDataDescriptor: public PDI::Data_descriptor {
	MOCK_METHOD1(default_type, void(PDI::Datatype_template_sptr));
	MOCK_METHOD0(default_type, PDI::Datatype_template_sptr());
	MOCK_CONST_METHOD0(metadata, bool());
	MOCK_METHOD1(metadata, void(bool));
	MOCK_CONST_METHOD0(name, const std::string&());
	MOCK_METHOD0(ref, PDI::Ref());
	MOCK_METHOD0(empty, bool());
	MOCK_METHOD3(share, void(void*, bool, bool));
	MOCK_METHOD3(share, void*(PDI::Ref, bool, bool));
	MOCK_METHOD0(release, void());
	MOCK_METHOD0(reclaim, void*());
};


#endif //PDI_DATA_DESCRIPTOR_MOCK_H_
