// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <memory>
#include <string>
#include <benchmark/benchmark.h>

#include <paraconf.h>
#include <pdi.h>

class PDI_Example: public benchmark::Fixture
{
public:
	const char* CONFIG_YAML
		= "logging: off                                                                      \n"
		  "metadata:                                                                         \n"
		  "  iter:   int                                                                     \n"
		  "  dsize:  { size: 2, type: array, subtype: int }                                  \n"
		  "  psize:  { size: 2, type: array, subtype: int }                                  \n"
		  "  pcoord: { size: 2, type: array, subtype: int }                                  \n"
		  "data:                                                                             \n"
		  "  main_field: { size: [ '$dsize[0]', '$dsize[1]' ], type: array, subtype: double }\n"
		  "plugins:                                                                          \n";

	void SetUp(const ::benchmark::State& state) {}

	void TearDown(const ::benchmark::State& state) {}
};

BENCHMARK_F(PDI_Example, InitFinalizePDI)(benchmark::State& state)
{
	PC_tree_t spec_tree = PC_parse_string(CONFIG_YAML);
	for (auto _: state) {
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
	for (auto _: state) {
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
	std::unique_ptr<double> cur{(double*)operator new (dsize[0] * dsize[1] * sizeof(double))};
	for (auto _: state) {
		PDI_multi_expose("newiter", "iter", &i, PDI_INOUT, "main_field", cur.get(), PDI_INOUT, NULL);
	}

	PDI_finalize();
}
