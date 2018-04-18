#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

cmake_minimum_required(VERSION 3.10)

###############
#   TEST 01   #
###############
add_executable(PDI_test_01 PDI_test_01.c)
target_link_libraries(PDI_test_01 pdi)
add_test(NAME PDI_test_01_run COMMAND PDI_test_01 "${CMAKE_CURRENT_SOURCE_DIR}/PDI_test_01.yml")

###############
#   TEST 02   #
###############
add_executable(PDI_test_02 PDI_test_02.c)
target_link_libraries(PDI_test_02 pdi)
set_target_properties(PDI_test_02 PROPERTIES ENABLE_EXPORTS ON)
add_test(NAME PDI_test_02_run COMMAND PDI_test_02 "${CMAKE_CURRENT_SOURCE_DIR}/PDI_test_02.yml")
set_property(TEST PDI_test_02_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/user_code/")

###############
# UNIT TESTS  #
###############
if("${USE_SYSTEM_GTEST}")
	find_package(GTest REQUIRED)
	find_package(GMock REQUIRED)
else()
	include(External_GTest)
	include(GoogleTest)
endif()

add_executable(PDI_unit_tests
		PDI_array_datatype.cxx
		PDI_error.cxx
		PDI_member.cxx
		PDI_record_datatype_empty.cxx
		PDI_ref_any.cxx
		PDI_scalar_datatype.cxx)

target_include_directories(PDI_unit_tests PRIVATE
		./mocks/)

target_link_libraries(PDI_unit_tests pdi pthread dl GTest::GTest GTest::Main GMock::GMock GMock::Main)

gtest_discover_tests(PDI_unit_tests)
