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
/// The specification tree examples of \ref Specification_tree_ref.
/// Every one of them is a snippet the reference pulls and that this test feeds
/// to %PDI, so that they are checked to be valid specification trees and not
/// merely valid YAML.

#include <string>
#include <vector>

#include <pdi.h>
#include <pdi/testing.h>

/// A datatype example, as it appears in the reference: it is the value of a
/// data descriptor, so it is checked wrapped in one.
struct Datatype_example {
	const char* name;
	const char* yaml;
};

const std::vector<Datatype_example> DATATYPES = {
	{"array_type",
     R"PDIYAML(
#! [array_type]
type: array
subtype: double
size: 5
#! [array_type]
)PDIYAML"},
	{"array_type_2",
     R"PDIYAML(
#! [array_type_2]
type: array
subtype: { type: character, kind: 4 }
size: [ '$size_1d', '$size_2d' ]
#! [array_type_2]
)PDIYAML"},
	{"byte_type",
     R"PDIYAML(
#! [byte_type]
type: byte
#! [byte_type]
)PDIYAML"},
	{"char_type",
     R"PDIYAML(
#! [char_type]
type: char
#! [char_type]
)PDIYAML"},
	{"character_type",
     R"PDIYAML(
#! [character_type]
type: character
#! [character_type]
)PDIYAML"},
	{"character_type_2",
     R"PDIYAML(
#! [character_type_2]
type: character
kind: 4
#! [character_type_2]
)PDIYAML"},
	{"double_type",
     R"PDIYAML(
#! [double_type]
type: double
#! [double_type]
)PDIYAML"},
	{"float_type",
     R"PDIYAML(
#! [float_type]
type: float
#! [float_type]
)PDIYAML"},
	{"int_type",
     R"PDIYAML(
#! [int_type]
type: int
#! [int_type]
)PDIYAML"},
	{"int16_type",
     R"PDIYAML(
#! [int16_type]
type: int16
#! [int16_type]
)PDIYAML"},
	{"int32_type",
     R"PDIYAML(
#! [int32_type]
type: int32
#! [int32_type]
)PDIYAML"},
	{"int64_type",
     R"PDIYAML(
#! [int64_type]
type: int64
#! [int64_type]
)PDIYAML"},
	{"int8_type",
     R"PDIYAML(
#! [int8_type]
type: int8
#! [int8_type]
)PDIYAML"},
	{"integer_type",
     R"PDIYAML(
#! [integer_type]
type: integer
#! [integer_type]
)PDIYAML"},
	{"integer_type_2",
     R"PDIYAML(
#! [integer_type_2]
type: integer
kind: 2
#! [integer_type_2]
)PDIYAML"},
	{"int_fast16_type",
     R"PDIYAML(
#! [int_fast16_type]
type: int_fast16
#! [int_fast16_type]
)PDIYAML"},
	{"int_fast32_type",
     R"PDIYAML(
#! [int_fast32_type]
type: int_fast32
#! [int_fast32_type]
)PDIYAML"},
	{"int_fast64_type",
     R"PDIYAML(
#! [int_fast64_type]
type: int_fast64
#! [int_fast64_type]
)PDIYAML"},
	{"int_fast8_type",
     R"PDIYAML(
#! [int_fast8_type]
type: int_fast8
#! [int_fast8_type]
)PDIYAML"},
	{"int_least16_type",
     R"PDIYAML(
#! [int_least16_type]
type: int_least16
#! [int_least16_type]
)PDIYAML"},
	{"int_least32_type",
     R"PDIYAML(
#! [int_least32_type]
type: int_least32
#! [int_least32_type]
)PDIYAML"},
	{"int_least64_type",
     R"PDIYAML(
#! [int_least64_type]
type: int_least64
#! [int_least64_type]
)PDIYAML"},
	{"int_least8_type",
     R"PDIYAML(
#! [int_least8_type]
type: int_least8
#! [int_least8_type]
)PDIYAML"},
	{"intmax_type",
     R"PDIYAML(
#! [intmax_type]
type: intmax
#! [intmax_type]
)PDIYAML"},
	{"intptr_type",
     R"PDIYAML(
#! [intptr_type]
type: intptr
#! [intptr_type]
)PDIYAML"},
	{"logging_output_map_2",
     R"PDIYAML(
#! [logging_output_map_2]
type: struct
members:
  - my_char: char
#! [logging_output_map_2]
)PDIYAML"},
	{"logging_output_map_3",
     R"PDIYAML(
#! [logging_output_map_3]
type: struct
members:
  - my_long: int64
  - my_array:
      type: array
      subtype: int64
      size: [10, 10]
#! [logging_output_map_3]
)PDIYAML"},
	{"logical_type",
     R"PDIYAML(
#! [logical_type]
type: logical
#! [logical_type]
)PDIYAML"},
	{"logical_type_2",
     R"PDIYAML(
#! [logical_type_2]
type: logical
kind: 1
#! [logical_type_2]
)PDIYAML"},
	{"long_type",
     R"PDIYAML(
#! [long_type]
type: long
#! [long_type]
)PDIYAML"},
	{"long_long_type",
     R"PDIYAML(
#! [long_long_type]
type: long long
#! [long_long_type]
)PDIYAML"},
	{"pointer_type",
     R"PDIYAML(
#! [pointer_type]
type: pointer
subtype: double
#! [pointer_type]
)PDIYAML"},
	{"pointer_type_2",
     R"PDIYAML(
#! [pointer_type_2]
type: pointer
subtype: { type: pointer, subtype: int }
#! [pointer_type_2]
)PDIYAML"},
	{"ptrdiff_t_type",
     R"PDIYAML(
#! [ptrdiff_t_type]
type: ptrdiff_t
#! [ptrdiff_t_type]
)PDIYAML"},
	{"real_type",
     R"PDIYAML(
#! [real_type]
type: real
#! [real_type]
)PDIYAML"},
	{"real_type_2",
     R"PDIYAML(
#! [real_type_2]
type: real
kind: 8
#! [real_type_2]
)PDIYAML"},
	{"record_type",
     R"PDIYAML(
#! [record_type]
type: record
buffersize: 8
members:
  first_int:
    disp: 0
    type: int32
  second_int:
    disp: 4
    type: int32
#! [record_type]
)PDIYAML"},
	{"record_type_2",
     R"PDIYAML(
#! [record_type_2]
type: record
buffersize: 1
members:
  my_char:
        disp: 0
        type: char
#! [record_type_2]
)PDIYAML"},
	{"record_type_3",
     R"PDIYAML(
#! [record_type_3]
type: record
buffersize: 808 
members:
  my_long:
    disp: 0
    type: int64
  my_array:
    disp: 8
    type: array
    subtype: int64
    size: [10, 10]
#! [record_type_3]
)PDIYAML"},
	{"short_type",
     R"PDIYAML(
#! [short_type]
type: short
#! [short_type]
)PDIYAML"},
	{"size_t_type",
     R"PDIYAML(
#! [size_t_type]
type: size_t
#! [size_t_type]
)PDIYAML"},
	{"struct_type",
     R"PDIYAML(
#! [struct_type]
type: struct
members:
  - first_int: int32
  - second_int: int32
#! [struct_type]
)PDIYAML"},
	{"tuple_type",
     R"PDIYAML(
#! [tuple_type]
type: tuple
buffersize: 16
elements:
  - {disp: 0, type: int32}
  - {disp: 8, type: double}
#! [tuple_type]
)PDIYAML"},
	{"tuple_type_2",
     R"PDIYAML(
#! [tuple_type_2]
type: tuple
elements:
  - int32
  - double
#! [tuple_type_2]
)PDIYAML"},
	{"uint16_type",
     R"PDIYAML(
#! [uint16_type]
type: uint16
#! [uint16_type]
)PDIYAML"},
	{"uint32_type",
     R"PDIYAML(
#! [uint32_type]
type: uint32
#! [uint32_type]
)PDIYAML"},
	{"uint64_type",
     R"PDIYAML(
#! [uint64_type]
type: uint64
#! [uint64_type]
)PDIYAML"},
	{"uint8_type",
     R"PDIYAML(
#! [uint8_type]
type: uint8
#! [uint8_type]
)PDIYAML"},
	{"uint_fast16_type",
     R"PDIYAML(
#! [uint_fast16_type]
type: uint_fast16
#! [uint_fast16_type]
)PDIYAML"},
	{"uint_fast32_type",
     R"PDIYAML(
#! [uint_fast32_type]
type: uint_fast32
#! [uint_fast32_type]
)PDIYAML"},
	{"uint_fast64_type",
     R"PDIYAML(
#! [uint_fast64_type]
type: uint_fast64
#! [uint_fast64_type]
)PDIYAML"},
	{"uint_fast8_type",
     R"PDIYAML(
#! [uint_fast8_type]
type: uint_fast8
#! [uint_fast8_type]
)PDIYAML"},
	{"uint_least16_type",
     R"PDIYAML(
#! [uint_least16_type]
type: uint_least16
#! [uint_least16_type]
)PDIYAML"},
	{"uint_least32_type",
     R"PDIYAML(
#! [uint_least32_type]
type: uint_least32
#! [uint_least32_type]
)PDIYAML"},
	{"uint_least64_type",
     R"PDIYAML(
#! [uint_least64_type]
type: uint_least64
#! [uint_least64_type]
)PDIYAML"},
	{"uint_least8_type",
     R"PDIYAML(
#! [uint_least8_type]
type: uint_least8
#! [uint_least8_type]
)PDIYAML"},
	{"uintmax_type",
     R"PDIYAML(
#! [uintmax_type]
type: uintmax
#! [uintmax_type]
)PDIYAML"},
	{"uintptr_type",
     R"PDIYAML(
#! [uintptr_type]
type: uintptr
#! [uintptr_type]
)PDIYAML"},
	{"unsigned_long_type",
     R"PDIYAML(
#! [unsigned_long_type]
type: unsigned long
#! [unsigned_long_type]
)PDIYAML"},
	{"unsigned_long_long_type",
     R"PDIYAML(
#! [unsigned_long_long_type]
type: unsigned long long
#! [unsigned_long_long_type]
)PDIYAML"},
	{"unsigned_short_type",
     R"PDIYAML(
#! [unsigned_short_type]
type: unsigned short
#! [unsigned_short_type]
)PDIYAML"},
};

/// A whole specification tree example, checked as it is.
const std::vector<Datatype_example> TREES = {
	{"logging_level",
     R"PDIYAML(
#! [logging_level]
logging: "debug"
#! [logging_level]
)PDIYAML"},
	{"logging_output_map",
     R"PDIYAML(
#! [logging_output_map]
logging:
  level: "debug"
  output:
    file: "test.log"
    console: "on"
#! [logging_output_map]
)PDIYAML"},
	{"plugin_path",
     R"PDIYAML(
#! [plugin_path]
plugin_path: "/home/user123/plugins"
#! [plugin_path]
)PDIYAML"},
	{"plugin_path_2",
     R"PDIYAML(
#! [plugin_path_2]
plugin_path: ["/home/user123/plugins", "/usr/lib/pdi/plugins"]
#! [plugin_path_2]
)PDIYAML"},
};

/// Wraps a datatype in the data section it describes the type of.
std::string as_data(const char* datatype)
{
	std::string result = "data:\n  a_data:\n";
	for (const char* line = datatype; *line;) {
		const char* end = line;
		while (*end && *end != '\n') {
			++end;
		}
		if (end != line) {
			result += "    " + std::string(line, end - line) + "\n";
		}
		line = *end ? end + 1 : end;
	}
	return result;
}

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct SpecTreeRefDoc: public ::PDI::PdiTest {};

TEST_F(SpecTreeRefDoc, specification_trees)
{
	for (auto&& example: DATATYPES) {
		PDI_init(PC_parse_string(as_data(example.yaml).c_str()));
		PDI_finalize();
	}
	for (auto&& example: TREES) {
		PDI_init(PC_parse_string(example.yaml));
		PDI_finalize();
	}
}
