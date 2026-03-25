# Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
#               root of the project or at https://github.com/pdidev/paraconf
#
# SPDX-License-Identifier: MIT

cmake_minimum_required(VERSION 3.22...4.2)
project(paraconf_examples LANGUAGES C)

option(BUILD_FORTRAN "Enable Fortran support" ON)
option(PARACONF_BUILD_FORTRAN "Enable Fortran support in Paraconf" "${BUILD_FORTRAN}")
option(PARACONF_EXAMPLE_BUILD_FORTRAN "Enable Fortran support in Paraconf" "${PARACONF_BUILD_FORTRAN}")

if("${PARACONF_EXAMPLE_BUILD_FORTRAN}")
	enable_language(Fortran)
endif()

if(NOT TARGET paraconf::paraconf)
	if("${PARACONF_EXAMPLE_BUILD_FORTRAN}")
		find_package(paraconf REQUIRED COMPONENTS C)
	else()
		find_package(paraconf REQUIRED COMPONENTS C f90)
	endif()
endif()

add_executable(paraconf_example example.c)
target_link_libraries(paraconf_example paraconf::paraconf)
add_test(NAME paraconf_example COMMAND paraconf_example "${CMAKE_CURRENT_SOURCE_DIR}/example.yml")

if("${PARACONF_EXAMPLE_BUILD_FORTRAN}")
	add_executable(paraconf_example_f90 example.f90)
	target_link_libraries(paraconf_example_f90 paraconf::paraconf_f90)
	add_test(NAME paraconf_example_f90 COMMAND paraconf_example_f90 "${CMAKE_CURRENT_SOURCE_DIR}/example.yml")
endif()
