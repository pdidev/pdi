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
/// The specification trees of the json documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_EXAMPLE_1 = R"PDIYAML(
#! [example_1]
plugins:
  json:
    data1: file_path.json
#! [example_1]
)PDIYAML";

const char* CONFIG_EXAMPLE_2 = R"PDIYAML(
#! [example_2]
plugins:
  json:
    - file: file_path.json
      when: iteration % 10 = 0 # This is optional
      write: [data1, data2, ...]
#! [example_2]
)PDIYAML";

const char* CONFIG_EXAMPLE_3 = R"PDIYAML(
#! [example_3]
types: # [...] including config_t description
    metadata: {rank: int, step: int}
    data:
        simulation_name: { type: array, subtype: char, size: 512 }
        max_steps: int
        mesh_config:
            type: struct
            members:
            - dimensions: { type: array, subtype: int, size: 3}
            - spacings: { type: array, subtype: int, size: 3}

        temp: # the main temperature field
        - type: array
        - subtype: double
        - size: '$mesh_config.dimensions'

plugins:
    json:
    - file: data-$rank.json
      write: [step, temp]
      when: '$step > 0'
#! [example_3]
)PDIYAML";

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct JsonDoc: public ::PDI::PdiTest {};

TEST_F(JsonDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_1));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_2));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_3));
	FinalizePdi();
}
