#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.22...4.2)

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

set(CMAKE_INCLUDE_CURRENT_DIR TRUE)


add_executable(PDI_unit_tests
		paraconf_wrapper.cxx
		parsed_modulo.cxx
		array_datatype.cxx
		C_API.cxx
		callbacks.cxx
		context.cxx
		data_descriptor.cxx
		datatype_attributes.cxx
		error.cxx
		expression.cxx
# 		initialize_plugins.cxx
		member.cxx
		pointer_datatype.cxx
		record_datatype_empty.cxx
		record_datatype.cxx
		ref_any.cxx
		scalar_datatype.cxx
		test_parse_types.cxx
		tuple_datatype.cxx
)
target_compile_features(PDI_unit_tests PRIVATE cxx_std_20)
target_link_libraries(PDI_unit_tests PDI::PDI_plugins GTest::gtest GTest::gtest_main GTest::gmock GTest::gmock_main)
target_include_directories(PDI_unit_tests PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../src")

if("${BUILD_PYTHON}")
	target_sources(PDI_unit_tests PRIVATE python_tools.cxx)
	target_link_libraries(PDI_unit_tests PDI::PDI_pysupport pybind11::embed Python3::Python)
endif()

set(OPERAND_TYPES uint8_t int64_t float double)
foreach(OPERAND1_TYPE IN LISTS OPERAND_TYPES)
	foreach(OPERAND2_TYPE IN LISTS OPERAND_TYPES)
		string(REPLACE "_" "U" CASE_NAME "${OPERAND1_TYPE}U${OPERAND2_TYPE}")
		configure_file(expression_operations.cxx "${CMAKE_CURRENT_BINARY_DIR}/expression_operations_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx" @ONLY)
		target_sources(PDI_unit_tests PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/expression_operations_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx")
	endforeach()
endforeach()

set(PARSEABLE_OPERAND_TYPES long double)
foreach(OPERAND1_TYPE IN LISTS PARSEABLE_OPERAND_TYPES)
	foreach(OPERAND2_TYPE IN LISTS PARSEABLE_OPERAND_TYPES)
		string(REPLACE "_" "U" CASE_NAME "${OPERAND1_TYPE}U${OPERAND2_TYPE}")
		configure_file(parsed_expression.cxx "${CMAKE_CURRENT_BINARY_DIR}/parsed_expression_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx" @ONLY)
		target_sources(PDI_unit_tests PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/parsed_expression_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx")
	endforeach()
endforeach()

set(MODULABLE_OPERAND_TYPES uint8_t int64_t)
foreach(OPERAND1_TYPE IN LISTS MODULABLE_OPERAND_TYPES)
	foreach(OPERAND2_TYPE IN LISTS MODULABLE_OPERAND_TYPES)
		string(REPLACE "_" "U" CASE_NAME "${OPERAND1_TYPE}U${OPERAND2_TYPE}")
		configure_file(modulo.cxx "${CMAKE_CURRENT_BINARY_DIR}/modulo_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx" @ONLY)
		target_sources(PDI_unit_tests PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/modulo_${OPERAND1_TYPE}_${OPERAND2_TYPE}.cxx")
	endforeach()
endforeach()

gtest_discover_tests(PDI_unit_tests)

find_program(ZSH_PROGRAM zsh)
if(EXISTS "${ZSH_PROGRAM}")
	set(ZSH_PROGRAM_FOUND ON)
else()
	set(ZSH_PROGRAM_FOUND OFF)
endif()
option(ENABLE_ZSH "Enable test of ZSH for env script" "${ZSH_PROGRAM_FOUND}")
if("${ENABLE_ZSH}" STREQUAL "AUTO")
	set(ENABLE_ZSH "${ZSH_PROGRAM_FOUND}")
endif()

add_test(NAME PDI_test_env COMMAND sh "${CMAKE_CURRENT_SOURCE_DIR}/test_env_sh" "${PDI_BINARY_DIR}/env.sh")
