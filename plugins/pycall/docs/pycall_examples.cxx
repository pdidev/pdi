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
/// The examples of the Pycall documentation.
/// Every specification tree the documentation shows lives here as a snippet and
/// is initialized by %PDI below, so that it is checked to be a valid %PDI
/// specification tree and not merely valid YAML.

#include <cstdio>

#include <pdi.h>

/// The plugin tree, with a comment on every key.
static const char* CONFIG_OVERVIEW =
	R"PDIYAML(
#! [overview]
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:           # name of the plugin
    on_event:       # run a script on an event
      testing:      # name of the event
        with: {}    # data aliases
        exec: pass  # python code executed when the event is triggered
    on_data:        # run a script on a data share
      a: pass       # python code executed when the data is shared
#! [overview]
)PDIYAML";

/// Running a script on an event.
static const char* CONFIG_ON_EVENT =
	R"PDIYAML(
#! [on_event]
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        with: { a_python: $a }
        exec: "print(' * [P] I received    $a =',a_python); a_python[1]=7; print(' * [P] changed it to $a =',a_python);"
#! [on_event]
)PDIYAML";

/// A list of executions on a single event.
static const char* CONFIG_ON_EVENT_LIST =
	R"PDIYAML(
#! [on_event_list]
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        - exec: "pass" # some python script
          with: { a: $a }
        - exec: "pass" # some other python script
          with: { b: $a }
#! [on_event_list]
)PDIYAML";

/// Running a script when a data is shared.
static const char* CONFIG_ON_DATA =
	R"PDIYAML(
#! [on_data]
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_data:
      a: "print(' * [P] I received    $a =',a); a[1]=7; print(' * [P] changed it to $a =',a);"
#! [on_data]
)PDIYAML";

/// The full example, the one whose output the documentation shows.
static const char* CONFIG_FULL =
	R"PDIYAML(
#! [full]
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        with: { a_python: $a }
        exec: "print(' * [P] I received    $a =',a_python, flush=True); a_python[1]=7; print(' * [P] changed it to $a =',a_python, flush=True);"
#! [full]
)PDIYAML";

/// Checks that a specification tree of the documentation is one %PDI accepts.
static void check(const char* config)
{
	PDI_init(PC_parse_string(config));
	PDI_finalize();
}

int main(int argc, char* argv[])
{
	check(CONFIG_OVERVIEW);
	check(CONFIG_ON_EVENT);
	check(CONFIG_ON_EVENT_LIST);
	check(CONFIG_ON_DATA);

	// the full example, run for real: the documentation shows its output
	PDI_init(PC_parse_string(CONFIG_FULL));

	//! [example]
	int a[3] = {1, 2, 3};

	printf(" * [C] starting with $a = [%d %d %d]\n", a[0], a[1], a[2]);
	fflush(stdout); // so that the C and python outputs interleave in order

	PDI_share("a", a, PDI_INOUT);
	PDI_event("testing");
	PDI_reclaim("a");

	printf(" * [C] now I see     $a = [%d %d %d]\n", a[0], a[1], a[2]);
	//! [example]

	PDI_finalize();
	return 0;
}
