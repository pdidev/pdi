// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <benchmark/benchmark.h>

#include <paraconf.h>
#include <pdi/expression.h>
#include "global_context.h"

class PDI_Expression: public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;
	int int_value;
	double double_value;

public:
	PDI::Context& context() { return *m_ctx; }

	void SetUp(const ::benchmark::State& state)
	{
		int_value = 42;
		double_value = 42.42;
		m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")});
		m_ctx->desc("int_value").share(&int_value, true, false);
	}

	void TearDown(const ::benchmark::State& state) { m_ctx->desc("int_value").reclaim(); }
};

BENCHMARK_F(PDI_Expression, EvaluateDouble)(benchmark::State& state)
{
	PDI::Expression double_expr{"42.42"};
	for (auto _: state) {
		double_expr.to_double(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateLong)(benchmark::State& state)
{
	PDI::Expression long_expr{"42"};
	for (auto _: state) {
		long_expr.to_long(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateString)(benchmark::State& state)
{
	PDI::Expression string_expr{"string_value"};
	for (auto _: state) {
		string_expr.to_string(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateOperation)(benchmark::State& state)
{
	PDI::Expression operation_expr{"1.23 + 2.34 / 3.45 * 4.56 - 5.67"};
	for (auto _: state) {
		operation_expr.to_double(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateReference)(benchmark::State& state)
{
	PC_tree_t tree = PC_parse_string("$int_value");
	PDI::Expression sequence_expr{tree};
	for (auto _: state) {
		sequence_expr.to_ref(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateSequence)(benchmark::State& state)
{
	PC_tree_t tree = PC_parse_string("[value_1, value_2, 3, 4.56, value_7]");
	PDI::Expression sequence_expr{tree};
	for (auto _: state) {
		sequence_expr.to_ref(context());
	}
}

BENCHMARK_F(PDI_Expression, EvaluateMapping)(benchmark::State& state)
{
	PC_tree_t tree = PC_parse_string("{key_1: {subkey_1: value_1, subkey_2: value_2}, key_2: {subkey_1: value_1, subkey_2: value_2}}");
	PDI::Expression mapping_expr{tree};
	for (auto _: state) {
		mapping_expr.to_ref(context());
	}
}
