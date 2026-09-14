#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

# Configure-only: embed the distribution the way another project would, as a subdirectory, and check how it behaves there.

cmake_minimum_required(VERSION 3.22...4.2)
# C and CXX both: PDI is a C++ library, and the C++ compile features of PDI::PDI_C propagate to whatever links it, so an enclosing project has to
# have CXX enabled even to use the C API.
project(pdi_cmake_tests_embedded LANGUAGES C CXX)

include("${CMAKE_CURRENT_SOURCE_DIR}/../assert.cmake")

set(PDI_DISTRIBUTION_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../.." CACHE PATH "The root of the PDI distribution to embed")



# # What an enclosing project may have done before embedding PDI

# The unprefixed names belong to the enclosing project: PDI must neither change them nor read them.  BUILD_FORTRAN is set to the opposite of the
# default of PDI_BUILD_FORTRAN, so that PDI reading it would show.
set(BUILD_TESTING ON)
set(BUILD_FORTRAN OFF)

# The same dependencies may already have been found, and with fewer components than PDI needs: MPI is found for C alone here, while the MPI plugin
# wants CXX.  The distribution has to notice the difference rather than skip the lookup on the strength of one target being present.
find_package(MPI REQUIRED COMPONENTS C)
find_package(Threads REQUIRED)

set(BUILD_TYPE_BEFORE "${CMAKE_BUILD_TYPE}")



# # Embedding PDI

set(PDI_BUILD_DOCUMENTATION OFF)
# The distribution would otherwise add its test projects.
set(PDI_BUILD_TESTING OFF)
add_subdirectory("${PDI_DISTRIBUTION_SOURCE_DIR}" pdi_dist)



# # The checks

# Not being the top-level project, PDI must have chosen the direct mode by itself.
assert(NOT "${PDI_SUPERBUILD}")

# The unprefixed variables must have survived untouched, and PDI must have ignored them.
assert("${BUILD_TESTING}")
assert(NOT "${BUILD_FORTRAN}")
assert("${PDI_BUILD_FORTRAN}")

# The build type is the enclosing project's to choose.
string(COMPARE EQUAL "${BUILD_TYPE_BEFORE}" "${CMAKE_BUILD_TYPE}" BUILD_TYPE_UNCHANGED)
assert("${BUILD_TYPE_UNCHANGED}")

assert(TARGET PDI::PDI_C)
assert(TARGET PDI::PDI_plugins)
assert(TARGET PDI::pdirun)

# The component PDI needed and the enclosing project had not found must have been picked up all the same.  MPI::MPI_CXX itself cannot be asserted
# from here: find_package() creates imported targets in the directory that calls it, so the one PDI found lives in pdi_dist and is invisible here.
# The plugin that links it is a normal target, and therefore global, so its presence is what proves the component was found.
if("${PDI_BUILD_MPI_PLUGIN}")
	assert(TARGET pdi_mpi_plugin)
endif()
