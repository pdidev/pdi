#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

# The tests under tests/cmake_tests/ only configure: they build nothing, and a failed assert() is what fails them.

include_guard(GLOBAL)

### Fail the configuration unless a condition holds
#
# \param ARGN the condition, in the syntax of if()
#
# The condition is evaluated after its arguments have been expanded, so a value that happens to name a variable would be resolved in turn: only
# pass constants, TARGET tests, and quoted variables holding boolean constants.  Compare strings beforehand, with string(COMPARE).
###
function(assert)
	string(REPLACE ";" " " _ASSERT_TEXT "${ARGN}")
	message(STATUS "assert(${_ASSERT_TEXT})")
	if(NOT (${ARGN}))
		message(FATAL_ERROR "Assertion failed: ${_ASSERT_TEXT}")
	endif()
endfunction()
