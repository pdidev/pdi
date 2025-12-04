#=============================================================================
# Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_cmake_tests)

option(TEST_FORTRAN "Test Fortran component" ON)
option(TEST_PYTHON "Test Python component" ON)

function(assert)
    message(STATUS "assert ${ARGV}")
    if (NOT (${ARGV}))
        message(FATAL_ERROR "Assertion failed: ${ARGV}")
    endif ()
endfunction()


# check subdirectory scope
# Note: temporary deactivate due to cmake version <3.18 bug with target alias
#message(STATUS "+ add_subdirectory(PDI)")
#add_subdirectory(subdir)
#assert(NOT DEFINED PDI_FOUND)
#assert(NOT TARGET PDI_C)
#assert(NOT TARGET PDI_f90)
#assert(NOT TARGET PDI_pysupport)


# 2 consecutive find_package(PDI)
message(STATUS "Testing multiple find_package(PDI)")
message(STATUS "+ 1st find_package")
find_package(PDI REQUIRED COMPONENTS C)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(NOT TARGET PDI_f90)
assert(NOT TARGET PDI_pysupport)

message(STATUS "+ 2nd find_package")
find_package(PDI REQUIRED COMPONENTS C)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(NOT TARGET PDI_f90)
assert(NOT TARGET PDI_pysupport)

# Add Fortran component
if ("${TEST_FORTRAN}")
    message(STATUS "+ find_package with component C and Fortran")
    find_package(PDI REQUIRED COMPONENTS Fortran)
    assert(${PDI_FOUND} EQUAL 1)
    assert(TARGET PDI_C)
    assert(TARGET PDI_f90)
    assert(NOT TARGET PDI_pysupport)
endif()

# Only C component
message(STATUS "+ find_package with component C. Previous components should remain.")
find_package(PDI REQUIRED COMPONENTS C)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(TARGET PDI_f90)
assert(NOT TARGET PDI_pysupport)

# Add pysupport component
if ("${TEST_PYTHON}")
    message(STATUS "+ find_package with component C and python")
    find_package(PDI REQUIRED COMPONENTS pysupport)
    assert(${PDI_FOUND} EQUAL 1)
    assert(TARGET PDI_C)
    assert(TARGET PDI_f90)
    #assert(TARGET PDI_pysupport) # no python target defined
endif()
