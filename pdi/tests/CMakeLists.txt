#=============================================================================
# Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

find_package(Threads REQUIRED)
find_package(GTest 1.8.0 REQUIRED)
find_package(GMock 1.8.0 REQUIRED)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
set(CMAKE_INCLUDE_CURRENT_DIR TRUE)


add_executable(PDI_test_01 PDI_test_01.c)
target_link_libraries(PDI_test_01 PDI::PDI_C)
add_test(NAME PDI_test_01 COMMAND PDI_test_01)


add_executable(PDI_test_02 PDI_test_02.c)
target_link_libraries(PDI_test_02 PDI::PDI_C)
set_target_properties(PDI_test_02 PROPERTIES ENABLE_EXPORTS ON)
add_test(NAME PDI_test_02 COMMAND PDI_test_02)

if("${BUILD_FORTRAN}")
	add_subdirectory(fortran/)
endif()

if("${BUILD_PYTHON}")
	add_test(NAME PDI_test_03 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/PDI_test_03.py")
	add_test(NAME PDI_test_04 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/PDI_test_04.py")
	add_test(NAME PDI_test_05 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/PDI_test_05.py")
endif()


set(PDI_unit_tests_SRC
		PDI_array_datatype.cxx
		PDI_callbacks.cxx
		PDI_context.cxx
		PDI_data_descriptor.cxx
		PDI_error.cxx
		PDI_expression.cxx
# 		PDI_initialize_plugins.cxx
		PDI_member.cxx
		PDI_pointer_datatype.cxx
		PDI_record_datatype_empty.cxx
		PDI_record_datatype.cxx
		PDI_ref_any.cxx
		PDI_scalar_datatype.cxx
		PDI_test_parse_types.cxx)
add_executable(PDI_unit_tests ${PDI_unit_tests_SRC})
target_link_libraries(PDI_unit_tests PDI::PDI_plugins Threads::Threads GTest::GTest GTest::Main GMock::GMock GMock::Main)
target_include_directories(PDI_unit_tests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../src")
gtest_discover_tests(PDI_unit_tests)


if("${Astyle_FOUND}")
	Astyle_add_indent(PDI_tests_indent
			OPTIONS_FILE "${PDI_SOURCE_DIR}/docs/formating.astyle"
			TEST
			RECURSIVE
			"${CMAKE_CURRENT_SOURCE_DIR}/**/*.c"
			"${CMAKE_CURRENT_SOURCE_DIR}/**/*.cxx"
			"${CMAKE_CURRENT_SOURCE_DIR}/**/*.h")
	add_dependencies(indent PDI_tests_indent)
endif()
