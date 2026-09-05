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
/// The specification trees of the decl_netcdf documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_OVERVIEW_1 = R"PDIYAML(
#! [overview_1]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    write: # ...
#! [overview_1]
)PDIYAML";

const char* CONFIG_OVERVIEW_2 = R"PDIYAML(
#! [overview_2]
plugins:
  decl_netcdf:
    - file: "file_name.nc"
      write: # ...
    - file: "file_name_2.nc"
      write: # ...
    - file: "file_name.nc"
      read: # ...
#! [overview_2]
)PDIYAML";

const char* CONFIG_EXAMPLE_1 = R"PDIYAML(
#! [example_1]
plugins:
  decl_netcdf:
    file: "file_name_${i}.nc"
#! [example_1]
)PDIYAML";

const char* CONFIG_EXAMPLE_2 = R"PDIYAML(
#! [example_2]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    communicator: $MPI_COMM_WORLD
#! [example_2]
)PDIYAML";

const char* CONFIG_EXAMPLE_3 = R"PDIYAML(
#! [example_3]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    on_event: "event"
#! [example_3]
)PDIYAML";

const char* CONFIG_EXAMPLE_4 = R"PDIYAML(
#! [example_4]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    on_event: ["event_1", "event_2"]
#! [example_4]
)PDIYAML";

const char* CONFIG_EXAMPLE_5 = R"PDIYAML(
#! [example_5]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    when: "$i < 10"
#! [example_5]
)PDIYAML";

const char* CONFIG_EXAMPLE_6 = R"PDIYAML(
#! [example_6]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    groups:
      group1:
        attributes:
          attr1: $value1
          attr2: $value2
      group1/group2:
        attributes:
          attr1: $value3
          attr2: $value4
#! [example_6]
)PDIYAML";

const char* CONFIG_EXAMPLE_7 = R"PDIYAML(
#! [example_7]
plugins:
  decl_netcdf:
    - file: "compressed_file.nc"
      deflate: 6
#! [example_7]
)PDIYAML";

const char* CONFIG_EXAMPLE_8 = R"PDIYAML(
#! [example_8]
plugins:
  decl_netcdf:
    file: "file_name.nc"
    variables:
      group1/group2/variable_name:
        type: array
        subtype: double
        size: [0, $value, $value] # 0 -> UNLIMITED dimension
        dimensions: ["time", "height", "width"]
        deflate: 6
        chunking: [10, 100, 100]
        attributes:
          attr1: $value
#! [example_8]
)PDIYAML";

const char* CONFIG_EXAMPLE_9 = R"PDIYAML(
#! [example_9]
metadata:
  var_attr: float
  group1_attr: float
  group1_data_attr: float
data:
  int_submatrix_top:
    type: array
    subtype: int
    size: [4, 8]
  int_submatrix_bottom:
    type: array
    subtype: int
    size: [4, 8]
plugins:
  decl_netcdf:
    - file: "example.nc"
      on_event: "write"
      groups:
        group_1:
          attributes:
            some_attr: $group1_attr
        group_1/data:
          attributes:
            some_attr: $group1_data_attr
      variables:
        group_1/data/int_matrix:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ["height", "width"]
          attributes:
            custom_attr: $var_attr
      write: 
        int_submatrix_top:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [0, 0]
            subsize: [4, 8]
        int_submatrix_bottom:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [4, 0]
            subsize: [4, 8]
    - file: "example.nc"
      on_event: "read"
      groups:
        group_1/data:
          attributes:
            some_attr: $custom_attr
      variables:
        group_1/data/int_matrix:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ["height", "width"]
          attributes:
            custom_attr: $var_attr
      read: 
        int_submatrix_top:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [0, 0]
            subsize: [4, 8]
        int_submatrix_bottom:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [4, 0]
            subsize: [4, 8]
#! [example_9]
)PDIYAML";

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct DeclNetcdfDoc: public ::PDI::PdiTest {};

TEST_F(DeclNetcdfDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_OVERVIEW_1));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_OVERVIEW_2));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_1));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_2));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_3));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_4));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_5));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_6));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_7));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_8));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_EXAMPLE_9));
	FinalizePdi();
}
