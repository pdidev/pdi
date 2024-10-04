#=============================================================================
# Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.10...3.25)
enable_language(Fortran)

# Includes
include(CTest)

function(build_test_compare_fortran TEST_NAME)
    # File locations
    set(EXPECTED_OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${TEST_NAME}_expected.json")
    set(ACTUAL_OUTPUT_PATH "${CMAKE_BINARY_DIR}/tests/fortran/${TEST_NAME}.json")
    set(YAML_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${TEST_NAME}.yml")

    # Test
    add_executable(${TEST_NAME} "${TEST_NAME}.F90")
    target_link_libraries(${TEST_NAME} PDI::PDI_f90)
    add_test(NAME ${TEST_NAME} COMMAND "$<TARGET_FILE:${TEST_NAME}>" "${YAML_PATH}" PRE_BUILD)

    # Compare content
    add_test(NAME verify_content_${TEST_NAME}
        COMMAND ${CMAKE_COMMAND} -DTEST_NAME=${TEST_NAME}
                                  -DEXPECTED_OUTPUT_PATH=${EXPECTED_OUTPUT_PATH}
                                  -DACTUAL_OUTPUT_PATH=${ACTUAL_OUTPUT_PATH}
                                  -DYAML_PATH=${YAML_PATH}
                                  -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/compare_content.cmake)

    set_tests_properties(verify_content_${TEST_NAME} PROPERTIES FIXTURES_REQUIRED ${TEST_NAME})
endfunction()

# add_executable(json_01_check_datatypes_F json_01_check_datatypes.F90)
# target_link_libraries(json_01_check_datatypes_F PDI::PDI_f90)
# add_test(NAME json_01_check_datatypes_F COMMAND "$<TARGET_FILE:json_01_check_datatypes_F>"  "${CMAKE_CURRENT_SOURCE_DIR}/json_01_check_datatypes.yml")

build_test_compare_fortran(json_01_check_datatypes_F)