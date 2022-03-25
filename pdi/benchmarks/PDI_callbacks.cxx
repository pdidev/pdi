/*******************************************************************************
 * Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <benchmark/benchmark.h>
#include <iostream>
#include <string>

#include "global_context.h"
#include <pdi/callbacks.h>
#include <paraconf.h>

class PDI_Callbacks : public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;
	
public:
	PDI::Context& context()
	{
		return *m_ctx;
	}
	
	void SetUp(const ::benchmark::State& state)
	{
		m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")});
	}
	
	void TearDown(const ::benchmark::State& state)
	{
	}
};

BENCHMARK_F(PDI_Callbacks, InitCallback)(benchmark::State& state)
{
	for (auto _ : state) {
		context().callbacks().call_init_callbacks();
	}
}

BENCHMARK_F(PDI_Callbacks, DataCallback)(benchmark::State& state)
{
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref){});
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref){});
	for (auto _ : state) {
		context().callbacks().call_data_callbacks("data", PDI::Ref{});
	}
}

BENCHMARK_F(PDI_Callbacks, NamedDataCallback)(benchmark::State& state)
{
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref){}, "data");
	context().callbacks().add_data_callback([](const std::string& data_name, PDI::Ref ref){}, "other");
	for (auto _ : state) {
		context().callbacks().call_data_callbacks("data", PDI::Ref{});
	}
}
