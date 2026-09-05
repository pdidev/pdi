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

# The examples of the documentation are run, not just compiled: what they
# illustrate is the mapping from the specification tree to the functions of the
# application, and only executing them checks that this mapping is correct.
# Their output is compared to the one the documentation shows, so that it can
# not go stale either.
# Their symbols must be exported for the plugin to find them.

# the comparison script lives with the PDI documentation, shared by every
# component that checks the output shown in its own documentation
set(PDI_DOCS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../../pdi/docs")

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()

# each example reads the specification tree the documentation shows beside it,
# hence the working directory
foreach(DOC_EXAMPLE IN ITEMS adding_to_number calculate hello_world print_number print_number_on_data)
	add_executable(user_code_doc_${DOC_EXAMPLE} "${DOC_EXAMPLE}.c")
	target_link_libraries(user_code_doc_${DOC_EXAMPLE} PDI::PDI_C)
	set_target_properties(user_code_doc_${DOC_EXAMPLE} PROPERTIES ENABLE_EXPORTS TRUE)
	add_test(NAME user_code_doc_${DOC_EXAMPLE}
			COMMAND "${CMAKE_COMMAND}"
				"-DEXAMPLE=$<TARGET_FILE:user_code_doc_${DOC_EXAMPLE}>"
				"-DEXPECTED=${CMAKE_CURRENT_SOURCE_DIR}/${DOC_EXAMPLE}.output"
				"-DWORKDIR=${CMAKE_CURRENT_SOURCE_DIR}"
				-P "${PDI_DOCS_DIR}/run_and_compare.cmake")
endforeach()

# The specification trees of the configuration reference: they only have to be
# accepted by %PDI, the functions they name do nothing.
add_executable(user_code_doc_trees user_code_examples.cxx)
target_link_libraries(user_code_doc_trees PDI::PDI_C GTest::gtest GTest::gtest_main GTest::gmock)
set_target_properties(user_code_doc_trees PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_doc_trees COMMAND PDI::pdirun "$<TARGET_FILE:user_code_doc_trees>")
