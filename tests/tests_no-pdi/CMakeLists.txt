#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
project(pdi_tests LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

option(BUILD_DECL_HDF5_PLUGIN "Build Decl'HDF5 plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN "Build Serialize plug-in" ON)
option(EXAMPLE_NO_PDI		  "Disable PDI using PDIConfig" OFF)

include(CTest)

find_package(paraconf 1.0.0 REQUIRED COMPONENTS C)
message(WARNING "PDI is disabled on test 07 for no pdi.")
if(PDI_ROOT)
	message(WARNING "Using include through PDI_ROOT")
	include(${PDI_ROOT}/PDIConfig.cmake)
else() #if(EXAMPLE_NO_PDI)
	message(WARNING "Using include through CMAKE_CURRENT_LIST_DIR")
	include(${CMAKE_CURRENT_LIST_DIR}/../../no-pdi/cmake/PDIConfig.cmake)
endif()
include_directories(${CMAKE_CURRENT_LIST_DIR})

add_executable(test_07_main_C test_07_main.c)
set(BUILD_FLAGS_FOR_C "-Wall")
target_compile_options(test_07_main_C PRIVATE
		$<$<COMPILE_LANGUAGE:C>:${BUILD_FLAGS_FOR_C}>)
target_link_libraries(test_07_main_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_07_main_C COMMAND "$<TARGET_FILE:test_07_main_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_07_main.yml")

add_executable(test_07_main_CXX test_07_main.cpp)
set(BUILD_FLAGS_FOR_CXX "-Wall")
target_compile_options(test_07_main_CXX PRIVATE
		$<$<COMPILE_LANGUAGE:CXX>:${BUILD_FLAGS_FOR_CXX}>)
set_target_properties(test_07_main_CXX PROPERTIES CXX_STANDARD 11)
set_target_properties(test_07_main_CXX PROPERTIES LANGUAGE CXX)
set_target_properties(test_07_main_CXX PROPERTIES LINKER_LANGUAGE CXX)
target_link_libraries(test_07_main_CXX paraconf::paraconf PDI::PDI_C)
add_test(NAME test_07_main_CXX COMMAND "$<TARGET_FILE:test_07_main_CXX>" "${CMAKE_CURRENT_SOURCE_DIR}/test_07_main.yml")
