#=============================================================================
# Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_test_api LANGUAGES C CXX)

set(API_TESTS_PDI "FIND" CACHE STRING "How to get PDI: FIND it (installed, or the mock through PDI_ROOT), or MOCK it with mock_pdi as a subdirectory, Paraconf's target included")
set_property(CACHE API_TESTS_PDI PROPERTY STRINGS FIND MOCK)

include(CTest)


if("FIND" STREQUAL "${API_TESTS_PDI}")
	# TODO: drop the guards once CMake 3.24 is the minimum; the embedding build can then use
	#       FetchContent_Declare(... OVERRIDE_FIND_PACKAGE) instead.
	if(NOT TARGET paraconf::paraconf)
		find_package(paraconf REQUIRED COMPONENTS C)
	endif()
	if(NOT TARGET PDI::PDI_C)
		find_package(PDI REQUIRED COMPONENTS C)
	endif()
elseif("MOCK" STREQUAL "${API_TESTS_PDI}")
	set(PDI_MOCK_PARACONF_TARGET TRUE)
	add_subdirectory(../../mock_pdi mock_pdi)
else()
	message(FATAL_ERROR "API_TESTS_PDI should be set to one of: FIND, MOCK")
endif()


add_executable(test_api_C test_api.c)
target_link_libraries(test_api_C PRIVATE PDI::PDI_C paraconf::paraconf)
add_test(NAME test_api_C COMMAND "$<TARGET_FILE:test_api_C>")


add_executable(test_api_CXX test_api.cpp)
target_link_libraries(test_api_CXX PRIVATE PDI::PDI_C paraconf::paraconf)
add_test(NAME test_api_CXX COMMAND "$<TARGET_FILE:test_api_CXX>")
