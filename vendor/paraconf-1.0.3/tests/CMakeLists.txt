# Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
#               root of the project or at https://github.com/pdidev/paraconf
#
# SPDX-License-Identifier: MIT

cmake_minimum_required(VERSION 3.22...4.2)

add_executable(test1 test1.c)
target_link_libraries(test1 paraconf::paraconf)
set_target_properties(test1 PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED TRUE)
add_test(NAME test1 COMMAND test1 "${CMAKE_CURRENT_SOURCE_DIR}/test_data.yml")

if("${BUILD_FORTRAN}")
	add_executable(test2 test2.f90)
	target_link_libraries(test2 paraconf::paraconf_f90)
	add_test(NAME test2 COMMAND test2 "${CMAKE_CURRENT_SOURCE_DIR}/test_data.yml")
endif()
