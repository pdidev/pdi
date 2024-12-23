/*******************************************************************************
 * Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
