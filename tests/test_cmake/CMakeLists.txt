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


function(assert)
    message(STATUS "assert ${ARGV}")
    if(NOT (${ARGV}))
        message(FATAL_ERROR "Assertion failed: ${ARGV}")
    endif()
endfunction()


message(STATUS "Testing multiple find_package(PDI)")

# 2 consecutive find_package(PDI)
message(STATUS "+ 1st find_package")
find_package(PDI REQUIRED)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(NOT TARGET PDI_python)
assert(NOT TARGET PDI_f90)


message(STATUS "+ 2nd find_package")
find_package(PDI REQUIRED)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(NOT TARGET PDI_python)
assert(NOT TARGET PDI_f90)

# find_package(PDI) with component
message(STATUS "+ find_package with component C")
find_package(PDI REQUIRED COMPONENTS C)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(NOT TARGET PDI_python)
assert(NOT TARGET PDI_f90)

message(STATUS "+ find_package with component C")
find_package(PDI REQUIRED COMPONENTS C Python)
assert(${PDI_FOUND} EQUAL 1)
assert(TARGET PDI_C)
assert(TARGET PDI_python)
assert(NOT TARGET PDI_f90)


get_cmake_property(_variableNames VARIABLES)
list (SORT _variableNames)
foreach (_variableName ${_variableNames})
    message(STATUS "${_variableName}=${${_variableName}}")
endforeach()
