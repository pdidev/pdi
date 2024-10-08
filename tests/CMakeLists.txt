#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_tests LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

option(BUILD_DECL_HDF5_PLUGIN "Build Decl'HDF5 plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN "Build Serialize plug-in" ON)

# Includes
include(CTest)

# PDI
find_package(PDI REQUIRED COMPONENTS C)

if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_01_C test_01.c)
target_link_libraries(test_01_C PDI::PDI_C)
add_test(NAME test_01_C COMMAND "$<TARGET_FILE:test_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_01.yml")

add_executable(test_02_C test_02.c)
target_link_libraries(test_02_C PDI::PDI_C)
add_test(NAME test_02_C COMMAND "$<TARGET_FILE:test_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_02.yml")

add_executable(test_03_C test_03.c)
target_link_libraries(test_03_C PDI::PDI_C)
add_test(NAME test_03_C COMMAND "$<TARGET_FILE:test_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_03.yml")

add_executable(test_04_C test_04.c)
target_link_libraries(test_04_C PDI::PDI_C)
add_test(NAME test_04_C COMMAND "$<TARGET_FILE:test_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_04.yml")

endif("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
if("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_05_C test_05.c)
target_link_libraries(test_05_C PDI::PDI_C)
add_test(NAME test_05_C COMMAND "$<TARGET_FILE:test_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_05.yml")

endif("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
