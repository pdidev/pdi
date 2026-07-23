#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/runtest-dir")

# Catalyst
find_package(catalyst REQUIRED)
if("${BUILD_CATALYST_PARALLEL}" AND NOT "${CATALYST_USE_MPI}")
	message(FATAL_ERROR "Catalyst with MPI support required, sequential catalyst only found. Please set -DBUILD_CATALYST_PARALLEL=OFF to disable parallel Catalyst")
endif()

# MPI
if("${CATALYST_USE_MPI}")
	find_package(MPI REQUIRED COMPONENTS C CXX)
	set(CATALYST_DEPS catalyst::catalyst MPI::MPI_C MPI::MPI_CXX)
endif()



# Creation of executable
add_executable(TestPDICatalyst_rectilinear rectilinear_mesh.cpp)

# MPI
find_package(MPI COMPONENTS C CXX REQUIRED)

target_link_libraries(TestPDICatalyst_rectilinear
  PRIVATE PDI::PDI_C catalyst::catalyst GTest::gmock GTest::gmock_main GTest::gtest GTest::gtest_main)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/tests_rectilinear)
configure_file(catalyst_serial_rectilinear.yml.in pdi.yml)

find_package(Python3 COMPONENTS Interpreter)

add_test(NAME TestPDICatalystRectilinear COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:TestPDICatalyst_rectilinear>")
set_property(TEST TestPDICatalystRectilinear PROPERTY ENVIRONMENT CATALYST_IMPLEMENTATION_NAME=stub  CATALYST_DATA_DUMP_DIRECTORY=./catalyst_output)