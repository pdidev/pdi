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
/// The specification trees of the serialize documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <string>

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_SPARSE_ARRAY = R"PDIYAML(
#! [sparse_array]
data:
  sparse_array:
    type: array
    subtype: int
    size: 8
    start: 2
    subsize: 4
plugins:
  serialize:
    logging: debug
    sparse_array: dense_array
#! [sparse_array]
)PDIYAML";

const char* CONFIG_DENSE_TYPE = R"PDIYAML(
#! [dense_type]
type: array
subtype: int
size: 4
#! [dense_type]
)PDIYAML";

const char* CONFIG_POINTER_TO_SPARSE = R"PDIYAML(
#! [pointer_to_sparse]
data:
  pointer_to_sparse_array:
    type: pointer
    subtype:
      type: array
      subtype: int
      size: 8
      start: 2
      subsize: 4
plugins:
  serialize:
    pointer_to_sparse_array: dense_array
#! [pointer_to_sparse]
)PDIYAML";

const char* CONFIG_DENSE_TYPE_2 = R"PDIYAML(
#! [dense_type_2]
type: array
subtype: int
size: 4
#! [dense_type_2]
)PDIYAML";

/// Wraps a datatype in the data section it describes the type of: a datatype
/// example is the value of a data descriptor rather than a tree of its own.
std::string as_data(const char* datatype)
{
	std::string config = "data:\n  a_data:\n";
	for (const char* line = datatype; *line;) {
		const char* end = line;
		while (*end && *end != '\n') {
			++end;
		}
		if (end != line) {
			config += "    " + std::string(line, end - line) + "\n";
		}
		line = *end ? end + 1 : end;
	}
	return config;
}

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct SerializeDoc: public ::PDI::PdiTest {};

TEST_F(SerializeDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_SPARSE_ARRAY));
	FinalizePdi();
	InitPdi(PC_parse_string(as_data(CONFIG_DENSE_TYPE).c_str()));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_POINTER_TO_SPARSE));
	FinalizePdi();
	InitPdi(PC_parse_string(as_data(CONFIG_DENSE_TYPE_2).c_str()));
	FinalizePdi();
}
