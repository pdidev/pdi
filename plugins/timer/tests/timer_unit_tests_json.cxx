/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <filesystem>
#include <iostream>
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

class Timer: public ::PDI::PdiTest
{};

/* Metadata use in filename expression & write on data
 */
TEST_F(Timer, json)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  timer:
    - timer_json: "json"
    - timer_pdi: "pdi"
  json:
    file: "file${meta_var}.json"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.json"));
}

TEST_F(Timer, js_output_to)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  timer:
    - output_to: timer.csv
    - timer_pdi: "pdi"
  json:
    file: "file${meta_var}.json"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.json"));
	FinalizePdi();
	EXPECT_TRUE(std::filesystem::exists("timer.csv"));
}
