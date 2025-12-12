#=============================================================================
# Copyright (C) 2023-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLEq FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.22...4.2)

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_json_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_json_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

function(build_test_compare TEST_NAME)
    # File locations
    set(EXPECTED_OUTPUT_PATH "${CMAKE_CURRENT_SOURCE_DIR}/${TEST_NAME}_expected.json")
    set(ACTUAL_OUTPUT_PATH "${CMAKE_BINARY_DIR}/tests/${TEST_NAME}.json")

    # Test
    add_executable(${TEST_NAME} "${TEST_NAME}.c")
    target_link_libraries(${TEST_NAME} PDI::PDI_C)
    add_test(NAME ${TEST_NAME} COMMAND "$<TARGET_FILE:${TEST_NAME}>" PRE_BUILD)

    # Compare content
    add_test(NAME verify_content_${TEST_NAME}
             COMMAND ${CMAKE_COMMAND} -DTEST_NAME=${TEST_NAME}
                                      -DEXPECTED_OUTPUT_PATH=${EXPECTED_OUTPUT_PATH}
                                      -DACTUAL_OUTPUT_PATH=${ACTUAL_OUTPUT_PATH}
                                      -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/compare_content.cmake)
    set_tests_properties(verify_content_${TEST_NAME} PROPERTIES FIXTURES_REQUIRED ${TEST_NAME})
endfunction()


build_test_compare(json_01_datatypes)
build_test_compare(json_02_multi_datatypes)
build_test_compare(json_03_arrays_of_datatypes)
build_test_compare(json_031_arrays_of_arrays_of_datatypes)
build_test_compare(json_04_arrays_of_multi_datatypes)
build_test_compare(json_05_mixing_datatypes)
build_test_compare(json_06_pointers)
build_test_compare(json_07_nested_datatypes)

