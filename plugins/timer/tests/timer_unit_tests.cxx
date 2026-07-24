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

/* Metatadata use in filename expression & write on data
 */
TEST_F(Timer, hdf5)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  timer:
    - timer_hdf5: "decl_hdf5"
    - timer_pdi: "pdi"
  decl_hdf5:
    file: "file${meta_var}.h5"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.h5"));
}

TEST_F(Timer, netcdf)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  timer:
    - timer_netcdf:
        start: "decl_netcdf_start_timer"
        stop: "decl_netcdf_stop_timer"
    - timer_pdi: "pdi"
  decl_netcdf:
    file: "file${meta_var}.nc"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.nc"));
}

TEST_F(Timer, output_to)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { meta_var: int }
data: { test_var: double }
plugins:
  timer:
    - output_to: timer.csv
    - timer_pdi: "pdi"
  decl_netcdf:
    file: "file${meta_var}.nc"
    write: [ test_var ]
)=="));

	int const meta_var = 1;
	PDI_expose("meta_var", &meta_var, PDI_OUT);

	auto const test_var = make_a<double>();
	PDI_expose("test_var", &test_var, PDI_OUT);
	EXPECT_TRUE(std::filesystem::exists("file1.nc"));
	FinalizePdi(); 
	EXPECT_TRUE(std::filesystem::exists("timer.csv"));

}
