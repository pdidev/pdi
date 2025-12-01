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
project(pdi_test_api LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

include(CTest)

find_package(paraconf COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

add_executable(test_api_C test_api.c)
if("${paraconf_FOUND}")
	target_link_libraries(test_api_C PRIVATE paraconf::paraconf PDI::PDI_C)
	target_compile_definitions(test_api_C PRIVATE PARACONF_FOUND)
else()
	target_link_libraries(test_api_C PRIVATE PDI::PDI_C)
endif()
add_test(NAME test_api_C COMMAND "$<TARGET_FILE:test_api_C>")

add_executable(test_api_CXX test_api.cpp)
if("${paraconf_FOUND}")
	target_link_libraries(test_api_CXX PRIVATE paraconf::paraconf PDI::PDI_C)
	target_compile_definitions(test_api_CXX PRIVATE PARACONF_FOUND)
else()
	target_link_libraries(test_api_CXX PRIVATE PDI::PDI_C)
endif()
add_test(NAME test_api_CXX COMMAND "$<TARGET_FILE:test_api_CXX>")
