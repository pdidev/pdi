#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

set(RUNTEST_DIR "${CMAKE_SOURCE_DIR}/cmake/runtest-dir")

###############
#   TEST 01   #
###############
add_executable(decl_sion_01_c decl_sion_test_01.c)
target_link_libraries(decl_sion_01_c pdi MPI::MPI_C)
add_test(NAME decl_sion_01_c COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_sion_01_c>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
set_property(TEST decl_sion_01_c APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_01_F decl_sion_test_01.F90)
target_link_libraries(decl_sion_01_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_01_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_sion_01_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
set_property(TEST decl_sion_01_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")

###############
#   TEST 02   #
###############
add_executable(decl_sion_02_c decl_sion_test_02.c)
target_link_libraries(decl_sion_02_c pdi MPI::MPI_C)
add_test(NAME decl_sion_02_c COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_02_c>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02_c.yml")
set_property(TEST decl_sion_02_c APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_02_F decl_sion_test_02.F90)
target_link_libraries(decl_sion_02_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_02_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_02_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02_F90.yml")
set_property(TEST decl_sion_02_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")

###############
#   TEST 03   #
###############
add_executable(decl_sion_03_c decl_sion_test_03.c)
target_link_libraries(decl_sion_03_c pdi MPI::MPI_C)
add_test(NAME decl_sion_03_c COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_03_c>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03_c.yml")
set_property(TEST decl_sion_03_c APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_03_F decl_sion_test_03.F90)
target_link_libraries(decl_sion_03_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_03_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_03_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_03_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03_F90.yml")
set_property(TEST decl_sion_03_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")
