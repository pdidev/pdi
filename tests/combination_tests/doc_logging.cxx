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
/// The logging examples of \ref logging_map_node.
/// The pattern they show uses the rank of the MPI communicator, which only the
/// MPI plugin provides, so they are checked here rather than beside the
/// reference itself.

#include <fstream>

#include <mpi.h>

#include <pdi.h>
#include <pdi/testing.h>

struct DocLogging: public ::PDI::PdiTest {};

/// The logger pattern that names the rank of the process.
TEST_F(DocLogging, pattern_with_rank)
{
	// the rank is a data the MPI plugin defines, so the pattern that names it
	// belongs to the logging of that plugin, which sets it up once it is loaded
	InitPdi(PC_parse_string(R"==(
plugins:
  mpi:
)=="
	                        R"==(
#! [logging_map]
    logging:
      level: "debug"
      pattern: "[%{MPI_COMM_WORLD_rank:04d}][%n][%l]"
#! [logging_map]
)=="));
}

/// Logging to a file rather than to the console.
TEST_F(DocLogging, output_map)
{
	InitPdi(PC_parse_string(
		R"==(
#! [logging_output_map]
logging:
  level: "debug"
  output:
    file: "test.log"
    console: "on"
#! [logging_output_map]
)=="
	));
}

/// The complete specification tree the reference opens with.
/// It `include`s another file, so the test writes that file next to itself
/// before loading the tree, the way the include tests do.
TEST_F(DocLogging, root_tree)
{
	{
		std::ofstream("my_other_configuration_file.yml") << R"==(
types:
  my_other_type: int
)==";
	}

	InitPdi(PC_parse_string(R"==(
#! [root_tree]
include: my_other_configuration_file.yml
logging: trace
types:
  metadata_t: int
metadata:
  my_metadata: metadata_t
data:
  my_data:
    type: array
    subtype: double
    size: $my_metadata
plugin_path:
  - /usr/lib/pdi
plugins:
  trace: #...
  mpi: #...
#! [root_tree]
)=="));

	// the tree really declares what it says it does
	int my_metadata = 5;
	PDI_expose("my_metadata", &my_metadata, PDI_OUT);
	double my_data[5] = {0., 1., 2., 3., 4.};
	PDI_expose("my_data", my_data, PDI_OUT);
}

int main(int argc, char** argv)
{
	::testing::InitGoogleTest(&argc, argv);
	MPI_Init(&argc, &argv);
	int result = RUN_ALL_TESTS();
	MPI_Finalize();
	return result;
}
