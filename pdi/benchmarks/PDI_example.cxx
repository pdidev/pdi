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
#include <memory>
#include <string>

#include <pdi.h>
#include <paraconf.h>

class PDI_Example : public benchmark::Fixture
{
public:
	const char* CONFIG_YAML =
		"logging: off                                                                      \n"
		"metadata:                                                                         \n"
		"  iter:   int                                                                     \n"
		"  dsize:  { size: 2, type: array, subtype: int }                                  \n"
		"  psize:  { size: 2, type: array, subtype: int }                                  \n"
		"  pcoord: { size: 2, type: array, subtype: int }                                  \n"
		"data:                                                                             \n"
		"  main_field: { size: [ '$dsize[0]', '$dsize[1]' ], type: array, subtype: double }\n"
		"plugins:                                                                          \n"
		;
	
	void SetUp(const ::benchmark::State& state)
	{
	}
	
	void TearDown(const ::benchmark::State& state)
	{
	}
};

BENCHMARK_F(PDI_Example, InitFinalizePDI)(benchmark::State& state)
{
	PC_tree_t spec_tree = PC_parse_string(CONFIG_YAML);
	for (auto _ : state) {
		PDI_init(spec_tree);
		PDI_finalize();
	}
}

BENCHMARK_F(PDI_Example, ShareAllMetadata)(benchmark::State& state)
{
	int dsize[2] = {20, 12};
	int psize[2] = {1, 1};
	int pcoord[2] = {0, 0};

	PDI_init(PC_parse_string(CONFIG_YAML));
	for (auto _ : state) {
		PDI_expose("dsize", dsize, PDI_OUT);
		PDI_expose("psize", psize, PDI_OUT);
		PDI_expose("pcoord", pcoord, PDI_OUT);
	}
	PDI_finalize();
}

BENCHMARK_F(PDI_Example, MainLoop)(benchmark::State& state)
{
	int dsize[2] = {20, 12};
	int psize[2] = {1, 1};
	int pcoord[2] = {0, 0};

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	PDI_expose("dsize", dsize, PDI_OUT);
	PDI_expose("psize", psize, PDI_OUT);
	PDI_expose("pcoord", pcoord, PDI_OUT);
	int i = 0;
	std::unique_ptr<double> cur {(double*) operator new (dsize[0] * dsize[1] * sizeof(double))};
	for (auto _ : state) {
		PDI_multi_expose("newiter",
				"iter", &i, PDI_INOUT,
				"main_field", cur.get(), PDI_INOUT,
				NULL);
	}
	
	PDI_finalize();
}
