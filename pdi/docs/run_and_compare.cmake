#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

# Runs an example and checks its output against the one shown in the
# documentation.
#
# The documentation shows the log lines with the timestamp that %PDI really
# prints, but that timestamp obviously differs on every run: it is replaced by
# a fixed `HH:MM:SS' on both sides before comparing, so that everything else,
# the order of the lines included, is still compared exactly.
#
# EXPECTED is the file holding the expected output, the very same one the
# documentation pulls the listing from with \snippet.
#
# PLUGIN_PATH, when set, is prepended to PDI_PLUGIN_PATH so that an example can
# load a plugin that is built beside it.
#
# WORKDIR, when set, is the directory the example is run from, for the examples
# that read their specification tree from a file next to them.

if (NOT DEFINED EXAMPLE OR NOT DEFINED EXPECTED)
	message(FATAL_ERROR "EXAMPLE and EXPECTED must be set, this script is meant to be run by ctest")
endif ()

if (DEFINED PLUGIN_PATH)
	set(ENV{PDI_PLUGIN_PATH} "${PLUGIN_PATH}:$ENV{PDI_PLUGIN_PATH}")
endif ()

if (NOT DEFINED WORKDIR)
	set(WORKDIR "${CMAKE_CURRENT_BINARY_DIR}")
endif ()

execute_process(COMMAND "${EXAMPLE}"
		WORKING_DIRECTORY "${WORKDIR}"
		OUTPUT_VARIABLE ACTUAL
		ERROR_VARIABLE ACTUAL_ERR
		RESULT_VARIABLE STATUS)
if (NOT STATUS EQUAL 0)
	message(FATAL_ERROR "${EXAMPLE} failed with status ${STATUS}:\n${ACTUAL}${ACTUAL_ERR}")
endif ()
string(APPEND ACTUAL "${ACTUAL_ERR}")

file(READ "${EXPECTED}" EXPECTED_CONTENT)

# keep only what the doxygen snippet holds, that is to say the lines between
# the two `#! [output]' markers
string(REPLACE "\n" ";" EXPECTED_LINES "${EXPECTED_CONTENT}")
set(EXPECTED_CONTENT "")
set(IN_SNIPPET FALSE)
foreach (LINE IN LISTS EXPECTED_LINES)
	if (LINE MATCHES "^#! \\[output\\]$")
		if (IN_SNIPPET)
			break()
		endif ()
		set(IN_SNIPPET TRUE)
	elseif (IN_SNIPPET)
		string(APPEND EXPECTED_CONTENT "${LINE}\n")
	endif ()
endforeach ()

# the timestamp is the only part of the output that is not reproducible
string(REGEX REPLACE "\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]" "[HH:MM:SS]" ACTUAL "${ACTUAL}")
string(REGEX REPLACE "\\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\]" "[HH:MM:SS]" EXPECTED_CONTENT "${EXPECTED_CONTENT}")

string(STRIP "${ACTUAL}" ACTUAL)
string(STRIP "${EXPECTED_CONTENT}" EXPECTED_CONTENT)

if (NOT ACTUAL STREQUAL EXPECTED_CONTENT)
	message(FATAL_ERROR
			"the output of ${EXAMPLE} differs from the one shown in the documentation.\n"
			"--- documented ---\n${EXPECTED_CONTENT}\n"
			"--- actual ---\n${ACTUAL}\n")
endif ()
