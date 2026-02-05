// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <string>
#include <benchmark/benchmark.h>

#include <paraconf.h>
#include <pdi/callbacks.h>
#include "global_context.h"

class PDI_Callbacks: public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;

public:
	PDI::Context& context() { return *m_ctx; }

	void SetUp(const ::benchmark::State& state) { m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")}); }

	void TearDown(const ::benchmark::State& state) {}
};

BENCHMARK_F(PDI_Callbacks, InitCallback)(benchmark::State& state)
{
	for (auto _: state) {
		context().callbacks().call_init_callbacks();
	}
}

BENCHMARK_F(PDI_Callbacks, DataCallback)(benchmark::State& state)
{
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref) {});
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref) {});
	for (auto _: state) {
		context().callbacks().call_data_callbacks("data", PDI::Ref{});
	}
}

BENCHMARK_F(PDI_Callbacks, NamedDataCallback)(benchmark::State& state)
{
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref) {}, "data");
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref) {}, "other");
	for (auto _: state) {
		context().callbacks().call_data_callbacks("data", PDI::Ref{});
	}
}
