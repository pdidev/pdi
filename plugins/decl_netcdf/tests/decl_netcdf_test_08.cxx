/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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


using PDI::make_random;
using PDI::random_init;

struct Record_t {
	int int_scalar;
	std::array<double, 32> double_array;

	bool operator== (const Record_t&) const = default;

	void init_from(std::uniform_random_bit_generator auto& gen)
	{
		random_init(gen, int_scalar);
		random_init(gen, double_array);
	}
};

class DeclNetcdf: public ::PDI::PdiTest
{};

// Tests simple write and read of scalar and array depending on event
TEST_F(DeclNetcdf, RecordScalarAndArray)
{
  InitPdi(PC_parse_string(R"==(
logging: trace
data:
  record:
    type: struct
    +decl_netcdf.type: record_type
    members:
      - int_scalar: int
      - double_array:
          type: array
          subtype: double
          size: 32
plugins:
  decl_netcdf:
    - file: "test_08.nc"
      on_event: "write"
      write:
        record:
          variable: /data/record
    - file: "test_08.nc"
      on_event: "read"
      read:
        record:
          variable: /data/record
)=="));

	auto const write_record_data = make_a<Record_t>();

	// write data
	PDI_multi_expose("write", "record", &write_record_data, PDI_OUT, NULL);

	Record_t read_record_data;

	// read data
	PDI_multi_expose("read", "record", &read_record_data, PDI_IN, NULL);

	EXPECT_EQ(read_record_data, write_record_data);
}
