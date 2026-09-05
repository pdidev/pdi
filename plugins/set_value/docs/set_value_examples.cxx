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
/// The specification trees of the set_value documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <vector>

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_GRAMMAR_ON_INIT = R"PDIYAML(
#! [grammar_on_init]
plugins:
  set_value:
    on_init:
      - set: # value_list ...
      - share: # value_list ...
      - expose: # value_list ...
      - release: # value_list ...
      - event: # value ...
      - logger: # logger_map ...
#! [grammar_on_init]
)PDIYAML";

const char* CONFIG_GRAMMAR_ON_EVENT = R"PDIYAML(
#! [grammar_on_event]
plugins:
  set_value:
    on_event:
      event_1_name:
        - set: # value_list ...
        - share: # value_list ...
        - expose: # value_list ...
        - release: # value_list ...
        - event: # value ...
      event_2_name:
        - set: # value_list ...
        - share: # value_list ...
        - expose: # value_list ...
        - release: # value_list ...
        - event: # value ...
#! [grammar_on_event]
)PDIYAML";

const char* CONFIG_GRAMMAR_ON_DATA = R"PDIYAML(
#! [grammar_on_data]
metadata:
  metadata_1_name: int # a metadata_type
data:
  data_1_name: int # a data_type
plugins:
  set_value:
    on_data:
      data_1_name:
        - set: # value_list ...
        - share: # value_list ...
        - expose: # value_list ...
        - release: # value_list ...
        - event: # value ...
      metadata_1_name:
        - set: # value_list ...
        - share: # value_list ...
        - expose: # value_list ...
        - release: # value_list ...
        - event: # value ...
#! [grammar_on_data]
)PDIYAML";

const char* CONFIG_GRAMMAR_ON_FINALIZE = R"PDIYAML(
#! [grammar_on_finalize]
plugins:
  set_value:
    on_finalize:
      - set: # value_list ...
      - share: # value_list ...
      - expose: # value_list ...
      - release: # value_list ...
      - event: # value ...
#! [grammar_on_finalize]
)PDIYAML";

const char* CONFIG_EXAMPLE_1 = R"PDIYAML(
#! [example_1]
metadata:
  scalar_name: int
set_value:
  on_init:
    - expose:
      - scalar_name: 42
#! [example_1]
)PDIYAML";

const char* CONFIG_EXAMPLE_2 = R"PDIYAML(
#! [example_2]
metadata:
  array_name:
    type: array
    size: 4
    subtype: int

plugins:
  set_value:
    on_init:
      - expose:
        - array_name: [2, 3, 4, 5]
#! [example_2]
)PDIYAML";

const char* CONFIG_EXAMPLE_3 = R"PDIYAML(
#! [example_3]
metadata:
  record_name:
    type: record
    buffersize: 16
    members:
      member_1:
        disp: 0
        type: array
        size: 3
        subtype: int
      member_2:
        disp: 12
        type: int

set_value:
  on_init:
    - expose:
      - record_name:
          member_2: 3 # int member
          member_1: [1, 2, 3] # array of ints member
#! [example_3]
)PDIYAML";

const char* CONFIG_EXAMPLE_4 = R"PDIYAML(
#! [example_4]
metadata:
  array_size: int64

data:
  record_data:
    type: record
    buffersize: 36
    members:
      scalar_data: 
        disp: 0
        type: int
      array_data:
        disp: 4
        type: array
        size: $array_size
        subtype: int

plugins:
  set_value:
    on_init:
      - expose:
        - array_size: 3
      - share:
        - record_data:
            scalar_data: 0
            array_data: [0, 0, 0]
    on_event:
      event_1_name:
        - set:
          - record_data:
              scalar_data: 3
              array_data: [1, 2, 3]
    on_finalize:
      - release: [record_data]
#! [example_4]
)PDIYAML";

const char* CONFIG_EXAMPLE_5 = R"PDIYAML(
#! [example_5]
data:
  value_int:
    type: int
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - share:
          - value_int: 0
          - int_array: [1, 2, 3]
      increment:
        - set:
          - value_int: "$value_int + 1"
          - int_array: ["$int_array[0] + 1", "$int_array[1] + 1", "$int_array[2] + 1"]
    on_finalize:
      - release: [value_int, int_array]
#! [example_5]
)PDIYAML";

const char* CONFIG_EXAMPLE_6 = R"PDIYAML(
#! [example_6]
metadata:
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - expose:
          - int_array: [0, 0, 0]
      increment:
        - expose:
          - int_array: ["$int_array[0] + 1", "$int_array[0] + 1", "$int_array[1] + 1"]
#! [example_6]
)PDIYAML";

const char* CONFIG_EXAMPLE_7 = R"PDIYAML(
#! [example_7]
metadata:
  int_scalar: int
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - expose:
          - int_scalar: 0
          - int_array: [0, 0, 0]
      increment:
        - expose:
          - int_scalar: $int_scalar+1
          - int_array: ["$int_scalar", "$int_scalar", "$int_scalar"]
#! [example_7]
)PDIYAML";

/// Checks a tree whose plugin configuration only makes sense once the events it
/// describes have been triggered.
void check_with_events(const char* config, const std::vector<const char*>& events)
{
	PDI_init(PC_parse_string(config));
	for (auto&& event: events) {
		PDI_event(event);
	}
	PDI_finalize();
}

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct SetValueDoc: public ::PDI::PdiTest {};

TEST_F(SetValueDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_GRAMMAR_ON_INIT));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_GRAMMAR_ON_EVENT));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_GRAMMAR_ON_DATA));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_GRAMMAR_ON_FINALIZE));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_1));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_2));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_3));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_4));
	FinalizePdi();
	check_with_events(CONFIG_EXAMPLE_5, {"init", "increment"});
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_6));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_7));
	FinalizePdi();
}
