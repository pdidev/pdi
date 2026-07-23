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

if(${CATALYST_USE_MPI})
  set(RUNTEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake/runtest-dir")
  # MPI
  find_package(MPI COMPONENTS C CXX REQUIRED)

  # Creation of executable
  add_executable(TestPDICatalystComm
  example.c)

  target_link_libraries(TestPDICatalystComm
    PRIVATE PDI::PDI_C MPI::MPI_C MPI::MPI_CXX)

  set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/tests/tests_comm)
  configure_file(catalyst_comm.yml.in pdi.yml)

  find_package(Python3 COMPONENTS Interpreter)

  add_test(NAME TestPDICatalystComm COMMAND "${RUNTEST_DIR}" ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/tests/tests_comm/run_test_mpi.py" "${CMAKE_BINARY_DIR}/tests/tests_comm/" "${CMAKE_SOURCE_DIR}/tests/tests_comm/" "${MPIEXEC_EXECUTABLE}") 
  set_property(TEST TestPDICatalystComm PROPERTY TIMEOUT 3)

endif()