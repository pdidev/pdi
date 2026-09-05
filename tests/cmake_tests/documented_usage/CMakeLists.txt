#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_documented_usage LANGUAGES C)

if("${BUILD_FORTRAN}")
	enable_language(Fortran)
endif()

# The cmake that \ref PDI_usage tells a user to write, configured and built here
# so that it cannot go stale.
# `<path/to>' is spelled `@MOCK_PDI_DIR@' in the snippets that the documentation
# shows with a placeholder, and substituted below.

#! [c_usage]
find_package(paraconf REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

add_executable(exec_file source_files.c)
target_link_libraries(exec_file PRIVATE PDI::PDI_C paraconf::paraconf)
#! [c_usage]

add_test(NAME documented_usage_c COMMAND "$<TARGET_FILE:exec_file>")

if("${BUILD_FORTRAN}")
#! [f90_usage]
find_package(paraconf REQUIRED COMPONENTS f90)
find_package(PDI REQUIRED COMPONENTS f90)

add_executable(exec_file_f90 source_files.f90)
target_link_libraries(exec_file_f90 PRIVATE PDI::PDI_f90 paraconf::paraconf_f90)
#! [f90_usage]

add_test(NAME documented_usage_f90
		COMMAND "$<TARGET_FILE:exec_file_f90>"
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endif()

# The "SubdirMock" approach: the snippet the documentation shows keeps the
# `<path/to>' placeholder, so it is replaced by the real location here before
# the result is included.
file(READ subdirmock.cmake.in SUBDIRMOCK_CMAKE)
string(REPLACE "<path/to>" "${CMAKE_CURRENT_SOURCE_DIR}/../../.." SUBDIRMOCK_CMAKE "${SUBDIRMOCK_CMAKE}")
# keep only the first variant: both add the same `mock_pdi' subdirectory, which
# cmake refuses to do twice, and they differ only by the Paraconf target the
# second one also asks the mock to provide.
string(REGEX REPLACE "#! \\[subdirmock_without_paraconf\\].*" "" SUBDIRMOCK_CMAKE "${SUBDIRMOCK_CMAKE}")
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/subdirmock.cmake" "${SUBDIRMOCK_CMAKE}")

set(DISABLE_PDI ON)
include("${CMAKE_CURRENT_BINARY_DIR}/subdirmock.cmake")
unset(DISABLE_PDI)

# the mock really provides the targets the documentation promises
if(NOT TARGET PDI::PDI_C)
	message(FATAL_ERROR "the SubdirMock example did not provide the PDI::PDI_C target")
endif()
