// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <string>
#include <benchmark/benchmark.h>

#include <paraconf.h>
#include <pdi/callbacks.h>
#include <pdi/scalar_datatype.h>
#include "global_context.h"

class PDI_Context: public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;

public:
	PDI::Context& context() { return *m_ctx; }

	void SetUp(const ::benchmark::State& state) { m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")}); }

	void TearDown(const ::benchmark::State& state) {}
};

BENCHMARK_F(PDI_Context, CreateDestroyContext)(benchmark::State& state)
{
	for (auto _: state) {
		PDI::Global_context{PC_parse_string("{logging: off}")};
	}
}

BENCHMARK_F(PDI_Context, CreateScalarDatatypeTemplate)(benchmark::State& state)
{
	PC_tree_t type_tree = PC_parse_string("int");
	for (auto _: state) {
		context().datatype(type_tree);
	}
}

BENCHMARK_F(PDI_Context, CreateArrayDatatypeTemplate)(benchmark::State& state)
{
	int size = 32;
	PDI::Ref size_ref{(void*)&size, [](void*) {}, PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), true, false};
	context().desc("size").share(size_ref, true, false);
	PC_tree_t type_tree = PC_parse_string("{type: array, subtype: int, size: $size}");
	for (auto _: state) {
		context().datatype(type_tree);
	}
}

BENCHMARK_F(PDI_Context, CreateTupleDatatypeTemplate)(benchmark::State& state)
{
	PC_tree_t type_tree = PC_parse_string("{type: tuple, elements: [int, double]}");
	for (auto _: state) {
		context().datatype(type_tree);
	}
}

BENCHMARK_F(PDI_Context, CreateRecordDatatypeTemplate)(benchmark::State& state)
{
	PC_tree_t type_tree = PC_parse_string("{type: record, buffersize: 16, "
	                                      "members: {first: {disp: 0, type: int}, second: {disp: 8, type: double}}}");
	for (auto _: state) {
		context().datatype(type_tree);
	}
}

BENCHMARK_F(PDI_Context, CreateStructDatatypeTemplate)(benchmark::State& state)
{
	PC_tree_t type_tree = PC_parse_string("{type: struct, members: [{first: int}, {second: double}]}");
	for (auto _: state) {
		context().datatype(type_tree);
	}
}
