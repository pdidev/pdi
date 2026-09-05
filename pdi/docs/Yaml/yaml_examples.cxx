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
/// The examples of \ref YAML.
/// The specification trees are fed to %PDI, so that they are checked to be
/// valid specification trees and not merely valid YAML; the plain YAML trees
/// are only parsed, as YAML is all they illustrate.

#include <pdi.h>

namespace {

/// A whole file passed to %PDI.
const char* CONFIG_WHOLE_FILE = R"PDIYAML(
#! [whole_file]
metadata:
  iteration: int
data:
  main_field: double
plugins:
  trace:
       # ...
#! [whole_file]
)PDIYAML";

/// A file where only a subtree is passed to %PDI.
const char* CONFIG_SUBTREE = R"PDIYAML(
#! [subtree]
duration: 0.75
size: [64, 64]
parallelism: { height: 4, width: 4 }

# only the following config will be passed to PDI
pdi_subtree:
  metadata:
    iteration: int
  data:
    main_field: double
  plugins:
    trace:
       # ...
#! [subtree]
)PDIYAML";

/// Plain YAML, illustrating the format itself rather than a %PDI tree: paraconf
/// is all that is needed to check these.
const char* YAML_TREE = R"PDIYAML(
#! [yaml_tree]
tree_1:
  array_1:
    - scalar_1
    - scalar_2
  array_2: [1, 2, 3]
tree_2: {subtree_1: scalar_1, subtree_2: scalar_2}
#! [yaml_tree]
)PDIYAML";

const char* YAML_SEQUENCE = R"PDIYAML(
#! [sequence]
- 1
- 2
- 3
- hello
- world
#! [sequence]
)PDIYAML";

const char* YAML_MAPPING = R"PDIYAML(
#! [mapping]
1: one
2: two
"three": 3
#! [mapping]
)PDIYAML";

const char* YAML_ORDERED_MAPPING = R"PDIYAML(
#! [ordered_mapping]
- 1: one
- 2: two
- "three": 3
#! [ordered_mapping]
)PDIYAML";

} // namespace

int main(int argc, char* argv[])
{
	//! [init_whole_file]
	PDI_init(PC_parse_path("example.yaml"));
	//! [init_whole_file]
	PDI_finalize();

	// the same, from the string this file embeds rather than from a path
	PDI_init(PC_parse_string(CONFIG_WHOLE_FILE));
	PDI_finalize();

	//! [init_subtree]
	PDI_init(PC_get(PC_parse_path("example.yaml"), ".pdi_subtree"));
	//! [init_subtree]
	PDI_finalize();

	PDI_init(PC_get(PC_parse_string(CONFIG_SUBTREE), ".pdi_subtree"));
	PDI_finalize();

	// the plain YAML trees only have to be valid YAML, paraconf is enough
	PC_parse_string(YAML_TREE);
	PC_parse_string(YAML_SEQUENCE);
	PC_parse_string(YAML_MAPPING);
	PC_parse_string(YAML_ORDERED_MAPPING);

	return 0;
}
