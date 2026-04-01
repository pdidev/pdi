#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_include_yaml LANGUAGES C)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

include(CTest)

find_package(paraconf REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_simple_yaml_include test_07.c)
target_link_libraries(test_simple_yaml_include paraconf::paraconf PDI::PDI_C)
add_test(NAME test_simple_yaml_include COMMAND "$<TARGET_FILE:test_simple_yaml_include>" "${CMAKE_CURRENT_SOURCE_DIR}/test_07.yml")
# set_tests_properties(test_simple_yaml_include PROPERTIES DISABLED TRUE) # Disabled as missing absolute paths on online CI

add_executable(test_duplicate_yaml_include test_07.c)
target_link_libraries(test_duplicate_yaml_include paraconf::paraconf PDI::PDI_C)
add_test(NAME test_duplicate_yaml_include COMMAND "$<TARGET_FILE:test_duplicate_yaml_include>" "${CMAKE_CURRENT_SOURCE_DIR}/test_08.yml")
# set_tests_properties(test_duplicate_yaml_include PROPERTIES DISABLED TRUE) # Disabled as expected to fail on correct behaviour (and also missing absolute paths on online CI)

add_executable(test_diamond_yaml_include test_07.c)
target_link_libraries(test_diamond_yaml_include paraconf::paraconf PDI::PDI_C)
add_test(NAME test_diamond_yaml_include COMMAND "$<TARGET_FILE:test_diamond_yaml_include>" "${CMAKE_CURRENT_SOURCE_DIR}/test_09.yml")
# set_tests_properties(test_diamond_yaml_include PROPERTIES DISABLED TRUE) # Disabled as missing absolute paths on online CI

add_executable(test_circular_yaml_include test_07.c)
target_link_libraries(test_circular_yaml_include paraconf::paraconf PDI::PDI_C)
add_test(NAME test_circular_yaml_include COMMAND "$<TARGET_FILE:test_circular_yaml_include>" "${CMAKE_CURRENT_SOURCE_DIR}/test_10.yml")
# set_tests_properties(test_circular_yaml_include PROPERTIES DISABLED TRUE) # Disabled as expected to fail on correct behaviour (and also missing absolute paths on online CI)

add_executable(test_plugins_yaml_include test_07.c)
target_link_libraries(test_plugins_yaml_include paraconf::paraconf PDI::PDI_C)
add_test(NAME test_plugins_yaml_include COMMAND "$<TARGET_FILE:test_plugins_yaml_include>" "${CMAKE_CURRENT_SOURCE_DIR}/test_11.yml")
# set_tests_properties(test_plugins_yaml_include PROPERTIES DISABLED TRUE) # Disabled as missing absolute paths on online CI

endif("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
