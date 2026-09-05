/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the names of CEA, nor the names of the contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/


/// \file
/// The simulation example of the JSON plugin documentation, run for real so
/// that both the %PDI calls it shows and the JSON it produces are checked.

#include <filesystem>
#include <fstream>
#include <sstream>

#include <pdi.h>
#include <pdi/testing.h>

struct JsonDoc: public ::PDI::PdiTest {};

TEST_F(JsonDoc, simulation)
{
	std::filesystem::remove("simulation.json");

	InitPdi(PC_parse_string(R"==(
metadata: {rank: int, step: int}
data:
  simulation_name: {type: array, subtype: char, size: 24}
  max_steps: int
  temp: {type: array, subtype: double, size: 3}
plugins:
  json:
    - file: simulation.json
      write: [simulation_name, max_steps, step, temp]
)=="));

	int rank = 7;
	PDI_expose("rank", &rank, PDI_OUT);

	char simulation_name[24]
		= {'H', 'e', 'a', 't', '_', 't', 'r', 'a', 'n', 's', 'f', 'e', 'r', '_', 's', 'i', 'm', 'u', 'l', 'a', 't', 'i', 'o', 'n'};
	int max_steps = 10;

	//! [simulation]
	PDI_multi_expose("init", "simulation_name", simulation_name, PDI_OUT, "max_steps", &max_steps, PDI_OUT, NULL);

	// main loop
	for (int step = 0; step < 2; ++step) {
		double temp[3] = {75.3, 74.7, 76.1};

		// share data at every iteration
		PDI_multi_expose("iter", "step", &step, PDI_OUT, "temp", temp, PDI_OUT, NULL);
	}
	//! [simulation]

	FinalizePdi();

	// the plugin really wrote the JSON the documentation shows
	ASSERT_TRUE(std::filesystem::exists("simulation.json"));
	std::ifstream produced{"simulation.json"};
	std::stringstream produced_content;
	produced_content << produced.rdbuf();

	std::ifstream expected{EXPECTED_JSON};
	ASSERT_TRUE(expected.good()) << "cannot read " << EXPECTED_JSON;
	std::stringstream expected_content;
	expected_content << expected.rdbuf();

	ASSERT_EQ(expected_content.str(), produced_content.str());
}
