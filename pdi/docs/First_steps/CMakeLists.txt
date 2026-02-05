# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

if("${BUILD_FORTRAN}")
	add_executable(hello_access.f90 hello_access.f90)
	target_link_libraries(hello_access.f90 PDI::PDI_Fortran)

	add_executable(hello_data.f90 hello_data.f90)
	target_link_libraries(hello_data.f90 PDI::PDI_Fortran)

	add_executable(hello_event.f90 hello_event.f90)
	target_link_libraries(hello_event.f90 PDI::PDI_Fortran)

	add_executable(hello_expose.f90 hello_expose.f90)
	target_link_libraries(hello_expose.f90 PDI::PDI_Fortran)

	add_executable(hello_multi_expose.f90 hello_multi_expose.f90)
	target_link_libraries(hello_multi_expose.f90 PDI::PDI_Fortran)
endif()

add_executable(hello_access_C hello_access.c)
target_link_libraries(hello_access_C PDI::PDI_C)

add_executable(hello_data_C hello_data.c)
target_link_libraries(hello_data_C PDI::PDI_C)

add_executable(hello_event_C hello_event.c)
target_link_libraries(hello_event_C PDI::PDI_C)

add_executable(hello_expose_C hello_expose.c)
target_link_libraries(hello_expose_C PDI::PDI_C)

add_executable(hello_multi_expose_C hello_multi_expose.c)
target_link_libraries(hello_multi_expose_C PDI::PDI_C)
