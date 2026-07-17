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

#include <hdf5.h>

#include <pdi/testing.h>

using PDI::make_random;
using PDI::random_init;
using testing::AllOf;
using testing::Eq;
using testing::HasSubstr;
using testing::StartsWith;
using testing::StrEq;

struct Simple_record {
	int x;
	int y;
	int z;
	bool operator== (const Simple_record&) const = default;

	void init_from(std::uniform_random_bit_generator auto& gen)
	{
		random_init(gen, x);
		random_init(gen, y);
		random_init(gen, z);
	}
};

std::ostream& operator<< (std::ostream& out, Simple_record const & r)
{
	return out << "Simple_record(x=" << r.x << ", y=" << r.y << ", z=" << r.z << ")";
}

struct Complex_record {
	int id;
	Simple_record value[4];
	bool operator== (const Complex_record&) const = default;

	void init_from(std::uniform_random_bit_generator auto& gen)
	{
		random_init(gen, id);
		random_init(gen, value);
	}
};

std::ostream& operator<< (std::ostream& out, Complex_record const & r)
{
	return out << "Complex_record(id=" << r.id << ", value=" << r.value << ")";
}

class Timer: public ::PDI::PdiTest
{};

/* Metatadata use in filename expression & write on data
 */
TEST_F(Timer, Sequetial)
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