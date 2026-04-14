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

# Creation of executable
add_executable(TestPDICatalystGhost
 example.c)

# MPI
find_package(MPI COMPONENTS C CXX REQUIRED)

target_link_libraries(TestPDICatalystGhost
  PRIVATE PDI::PDI_C MPI::MPI_C)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/tests_ghost)
configure_file(catalyst_serial.yml.in pdi.yml)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/tests_ghost)
configure_file(catalyst_serial_structured.yml.in pdi_structured.yml)

find_package(Python3 COMPONENTS Interpreter)
add_test(NAME TestPDICatalystGhost_uniform COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/tests_ghost/run_test.py" "${CMAKE_BINARY_DIR}/tests_ghost/" "${CMAKE_SOURCE_DIR}/tests_ghost/" "true_uniform") 
add_test(NAME TestPDICatalystGhost_structured COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/tests_ghost/run_test.py" "${CMAKE_BINARY_DIR}/tests_ghost/" "${CMAKE_SOURCE_DIR}/tests_ghost/" "true_structured")