#=============================================================================
# Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
project(pdi_cmake_tests)

include(CTest)

## Global options

set(DIST_PROFILE "User" CACHE STRING "Profile to use for PDI distribution build. Options are: User, Devel")
set_property(CACHE DIST_PROFILE PROPERTY STRINGS User Devel)

if(User STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" OFF)
	option(BUILD_TESTING       "Build tests" OFF)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" OFF)
elseif(Devel STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" ON)
	option(BUILD_TESTING       "Build tests" ON)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" ON)
else()
	message(FATAL_ERROR "DIST_PROFILE should be set to one of: User, Devel")
endif()

option(BUILD_BENCHMARKING       "Build PDI benchmarks" ON)
option(BUILD_DECL_HDF5_PLUGIN   "Build Decl'HDF5 plug-in" ON)
option(BUILD_DECL_NETCDF_PLUGIN "Build Decl'NetCDF plug-in" ON)
option(BUILD_FORTRAN            "Build with Fortran support" ON)
option(BUILD_HDF5_PARALLEL      "Build Decl'HDF5 in parallel mode" ON)
option(BUILD_MPI_PLUGIN         "Build MPI plug-in" ON)
option(BUILD_NETCDF_PARALLEL    "Build Decl'NetCDF in parallel mode" ON)
option(BUILD_PYCALL_PLUGIN      "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON             "Build with Python support" "${BUILD_UNSTABLE}")
option(BUILD_SET_VALUE_PLUGIN   "Build Set_value plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN   "Build Serialize plug-in" ON)
option(BUILD_TRACE_PLUGIN       "Build Trace plugin" ON)
option(BUILD_USER_CODE_PLUGIN   "Build User-code plugin" ON)
option(BUILD_JSON_PLUGIN        "Build JSON plugin" OFF)



function(assert)
    string(REPLACE ";" " " ASSERT_TEXT "${ARGN}")
    message(STATUS "assert(${ASSERT_TEXT})")
    if(NOT ( ${ARGN} ))
        message(FATAL_ERROR "Assertion failed: ${ASSERT_TEXT}")
    endif ()
endfunction()


# check subdirectory scope
message(STATUS "checking subdirectory scope")

message("++ add_subdirectory(example)")
add_subdirectory(../example example)

message("++ add_subdirectory(test_api)")
add_subdirectory(../test_api/ test_api)

message("++ add_subdirectory(tests)")
add_subdirectory(../tests/ tests)

message("++ add_subdirectory(subdir)")
add_subdirectory(subdir)

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
