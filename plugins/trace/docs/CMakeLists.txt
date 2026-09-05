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

cmake_minimum_required(VERSION 3.22...4.2)

# The specification trees shown in the documentation are fed to %PDI here:
# paraconf alone would only tell that they are valid YAML, initializing %PDI
# checks that they are valid specification trees.

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()

add_executable(trace_doc_examples trace_examples.cxx)
target_link_libraries(trace_doc_examples PDI::PDI_C GTest::gtest GTest::gtest_main GTest::gmock)
add_test(NAME trace_doc_examples COMMAND PDI::pdirun "$<TARGET_FILE:trace_doc_examples>")

# The trace shown in the documentation, produced for real and compared to it.
add_executable(trace_doc_output trace_output.cxx)
target_link_libraries(trace_doc_output PDI::PDI_C GTest::gtest GTest::gtest_main GTest::gmock)
add_test(NAME trace_doc_output
		COMMAND "${CMAKE_COMMAND}"
			"-DEXAMPLE=$<TARGET_FILE:trace_doc_output>"
			"-DEXPECTED=${CMAKE_CURRENT_SOURCE_DIR}/trace_examples.output"
			-P "${CMAKE_CURRENT_SOURCE_DIR}/../../../pdi/docs/run_and_compare.cmake")
