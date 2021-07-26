#=============================================================================
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
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
