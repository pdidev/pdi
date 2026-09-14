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

# Offers a `runtest_dir` executable target that runs a test in a fresh temporary directory, deleted afterwards, so that tests writing files neither
# collide when run in parallel nor leave anything behind:
#
#   add_test(NAME <name> COMMAND "$<TARGET_FILE:runtest_dir>" [--runtest-dir-copy-file <file>]... <command> [<arg>...])
#
# Each `--runtest-dir-copy-file <file>` copies <file> into that directory before <command> runs there.
#
# The target is imported and global, so every directory that includes this module shares the one definition.

include_guard(GLOBAL)

add_executable(runtest_dir IMPORTED GLOBAL)
set_target_properties(runtest_dir PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_LIST_DIR}/runtest-dir")
