// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <string>
#include <benchmark/benchmark.h>

#include <paraconf.h>
#include <pdi/pdi_fwd.h>
#include <pdi/datatype_template.h>
#include <pdi/scalar_datatype.h>
#include "global_context.h"

class PDI_Datatype_template: public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;

public:
	PDI::Context& context() { return *m_ctx; }

	void SetUp(const ::benchmark::State& state) { m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")}); }

	void TearDown(const ::benchmark::State& state) {}
};

BENCHMARK_F(PDI_Datatype_template, EvaluateScalar)(benchmark::State& state)
{
	PDI::Datatype_template_sptr scalar_datatype_template = context().datatype(PC_parse_string("int"));
	for (auto _: state) {
		scalar_datatype_template->evaluate(context());
	}
}

BENCHMARK_F(PDI_Datatype_template, EvaluateArray)(benchmark::State& state)
{
	int size = 32;
	PDI::Ref size_ref{(void*)&size, [](void*) {}, PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), true, false};
	context().desc("size").share(size_ref, true, false);
	PC_tree_t type_tree = PC_parse_string("{type: array, subtype: int, size: $size}");
	PDI::Datatype_template_sptr array_datatype_template = context().datatype(type_tree);
	for (auto _: state) {
		array_datatype_template->evaluate(context());
	};
	context().desc("size").release();
}

BENCHMARK_F(PDI_Datatype_template, EvaluateRecord)(benchmark::State& state)
{
	PC_tree_t type_tree = PC_parse_string("{type: struct, members: [{first: int}, {second: double}]}");
	PDI::Datatype_template_sptr record_datatype_template = context().datatype(type_tree);
	for (auto _: state) {
		record_datatype_template->evaluate(context());
	};
}
