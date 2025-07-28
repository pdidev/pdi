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

include(CTest)

# option(BUILD_WITHOUT_PDI "Use a mock PDI instead of the real one through " OFF)
# option(BUILD_WITH_PARACONF_FOR_NO_PDI "Build with the Paraconf embedded in local no-PDI" OFF)
# message(WARNING "BUILD_WITHOUT_PDI: ${BUILD_WITHOUT_PDI}")
# message(WARNING "BUILD_WITH_PARACONF_FOR_NO_PDI: ${BUILD_WITH_PARACONF_FOR_NO_PDI}")
# if(BUILD_WITHOUT_PDI)
# 	add_library(PDI_C INTERFACE)
# 	message(WARNING "Using include of NO-PDI through CMAKE_CURRENT_LIST_DIR: ${CMAKE_CURRENT_LIST_DIR}/../no-pdi/include")
# 	if(NOT BUILD_WITH_PARACONF_FOR_NO_PDI) 	# This case represents the use of NO-PDI together with its embedded NO-PARACONF.
# 		message(WARNING "Using NO-PARACONF, for NO-PDI through include_directories")
# 		target_include_directories(PDI_C INTERFACE BEFORE "${CMAKE_CURRENT_LIST_DIR}/../no-paraconf/include")
# 		add_library(paraconf INTERFACE)
# 		add_library(paraconf::paraconf ALIAS paraconf)
# 		target_include_directories(paraconf INTERFACE BEFORE "${CMAKE_CURRENT_LIST_DIR}/../no-paraconf/include")
# 		target_link_libraries(PDI_C INTERFACE paraconf::paraconf)
# 	else() 									# This case is never reached in the tests, as this method of using NO-PDI is intended to be used when PARACONF is missing.
# 		message(WARNING "Using PARACONF, for NO-PDI through include_directories")
# 		find_package(paraconf 1.0.0 REQUIRED COMPONENTS C)
# 	endif()
# 	include_directories("${CMAKE_CURRENT_LIST_DIR}/../no-pdi/include")
#     include_directories(PDI_C INTERFACE "${CMAKE_CURRENT_LIST_DIR}/../no-pdi/include")
#     add_library(PDI::PDI_C ALIAS PDI_C)
# else() 										# This case covers boths PDI and NO-PDI (through find_package), when using PARACONF.
# 	message(WARNING "Using find_package for PDI (whether PDI or NO-PDI)")
# 	find_package(PDI REQUIRED COMPONENTS C)
# 	message(WARNING "Using PARACONF, for PDI or NO-PDI through find_package")
# 	find_package(paraconf 1.0.0 REQUIRED COMPONENTS C)
# 	target_link_libraries(PDI_C INTERFACE paraconf::paraconf)
# endif()

message(WARNING "test-api")
# if(BUILD_WITHOUT_PDI)
# 	add_subdirectory({CMAKE_CURRENT_LIST_DIR}/../no-pdi no-pdi)
# else()
# 	find_package(PDI REQUIRED COMPONENTS C)
# endif()
find_package(PDI REQUIRED COMPONENTS C)
find_package(paraconf REQUIRED COMPONENTS C)

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

add_executable(test_07_C test_07_main.c)
set(BUILD_FLAGS_FOR_C "-Wall")
target_compile_options(test_07_C PRIVATE
		$<$<COMPILE_LANGUAGE:C>:${BUILD_FLAGS_FOR_C}>)
target_link_libraries(test_07_C PDI::PDI_C)
add_test(NAME test_07_C COMMAND "$<TARGET_FILE:test_07_C>")

add_executable(test_07_CXX test_07_main.cpp)
set(BUILD_FLAGS_FOR_CXX "-Wall")
target_compile_options(test_07_CXX PRIVATE
		$<$<COMPILE_LANGUAGE:CXX>:${BUILD_FLAGS_FOR_CXX}>)
set_target_properties(test_07_CXX PROPERTIES CXX_STANDARD 11)
set_target_properties(test_07_CXX PROPERTIES LANGUAGE CXX)
set_target_properties(test_07_CXX PROPERTIES LINKER_LANGUAGE CXX)
target_link_libraries(test_07_CXX PDI::PDI_C)
add_test(NAME test_07_CXX COMMAND "$<TARGET_FILE:test_07_CXX>")
