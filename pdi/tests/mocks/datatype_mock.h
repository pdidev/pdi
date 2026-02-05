/*
 * SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_DATATYPE_MOCK_H_
#define PDI_DATATYPE_MOCK_H_

#include <gmock/gmock.h>

#include <pdi/context.h>
#include <pdi/datatype.h>

struct MockDatatype: public PDI::Datatype {
	MockDatatype()
		: PDI::Datatype()
	{}

	bool operator== (const Datatype& other) const override { return equals(other); }

	MOCK_CONST_METHOD0(dense, bool());
	MOCK_CONST_METHOD0(datasize, size_t());
	MOCK_CONST_METHOD0(buffersize, size_t());
	MOCK_CONST_METHOD0(alignment, size_t());
	MOCK_CONST_METHOD0(densify, PDI::Datatype_sptr());
	MOCK_CONST_METHOD1(evaluate, PDI::Datatype_sptr(PDI::Context&));
	MOCK_CONST_METHOD0(simple, bool());
	MOCK_CONST_METHOD2(data_to_dense_copy, void*(void* to, const void* from));
	MOCK_CONST_METHOD2(data_from_dense_copy, void*(void* to, const void* from));
	MOCK_CONST_METHOD1(destroy_data, void(void* ptr));
	MOCK_CONST_METHOD1(equals, bool(const PDI::Datatype&));
	MOCK_CONST_METHOD0(debug_string, std::string());
};

#endif //PDI_DATATYPE_MOCK_H_
