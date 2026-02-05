# SPDX-FileCopyrightText: 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_test_api LANGUAGES C CXX)

option(DISABLE_PDI "Disable the use of both PDI and Paraconf in the project" OFF)

include(CTest)


if("${DISABLE_PDI}")
	set(PDI_MOCK_PARACONF_TARGET TRUE)
	add_subdirectory(../mock_pdi mock_pdi)
else()
	find_package(paraconf REQUIRED COMPONENTS C)
	find_package(PDI REQUIRED COMPONENTS C)
endif()


add_executable(test_api_C test_api.c)
target_link_libraries(test_api_C PRIVATE PDI::PDI_C paraconf::paraconf)
add_test(NAME test_api_C COMMAND "$<TARGET_FILE:test_api_C>")


add_executable(test_api_CXX test_api.cpp)
target_link_libraries(test_api_CXX PRIVATE PDI::PDI_C paraconf::paraconf)
add_test(NAME test_api_CXX COMMAND "$<TARGET_FILE:test_api_CXX>")
