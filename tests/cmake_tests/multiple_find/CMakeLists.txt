#=============================================================================
# Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
project(pdi_cmake_tests_multiple_find)

# Configure-only: find the installed PDI from several directory scopes, and several times in the same one.
include("${CMAKE_CURRENT_SOURCE_DIR}/../assert.cmake")

# The components to check, which have to match those the distribution installed.
option(BUILD_FORTRAN "Check the Fortran component" ON)
option(BUILD_PYTHON  "Check the Python components" ON)


# check subdirectory scopes
message(STATUS "checking subdirectory scopes")

message("++ add_subdirectory(c_component)")
add_subdirectory(c_component)

message("++ add_subdirectory(all_components)")
add_subdirectory(all_components)

message(STATUS "Testing multiple find_package(PDI)")

message("++ 1st find_package")
find_package(PDI REQUIRED COMPONENTS C)
assert("${PDI_FOUND}")
assert(TARGET PDI::PDI_C)

message("++ 2nd find_package")
find_package(PDI REQUIRED COMPONENTS C)
assert("${PDI_FOUND}")
assert(TARGET PDI::PDI_C)

if ("${BUILD_FORTRAN}")
    message(STATUS "Testing Fortran component")
    
    message("++ find_package with component C and Fortran")
    find_package(PDI REQUIRED COMPONENTS f90)
    assert("${PDI_FOUND}")
    assert(TARGET PDI::PDI_C)
    assert(TARGET PDI::PDI_f90)

    # Only C component
    message("++ find_package with component C. Previous components should remain.")
    find_package(PDI REQUIRED COMPONENTS C)
    assert("${PDI_FOUND}")
    assert(TARGET PDI::PDI_C)
    assert(TARGET PDI::PDI_f90)
endif()

if ("${BUILD_PYTHON}")
    message(STATUS "Testing pysupport component")
    
    message("++ find_package with component C and python")
    find_package(PDI REQUIRED COMPONENTS pysupport)
    assert("${PDI_FOUND}")
    assert(TARGET PDI::PDI_C)
endif()
