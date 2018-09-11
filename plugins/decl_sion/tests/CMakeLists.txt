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

cmake_minimum_required(VERSION 3.10)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")


# Testing export of Metatadata and files creation

add_executable(decl_sion_01_c decl_sion_test_01.c)
target_link_libraries(decl_sion_01_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_01_c COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_sion_01_c>")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_01_F decl_sion_test_01.F90)
target_link_libraries(decl_sion_01_F PDI::pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_01_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_sion_01_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
endif("${ENABLE_FORTRAN}")


# Testing writer/reader with one file per variable

add_executable(decl_sion_02_c decl_sion_test_02.c)
target_link_libraries(decl_sion_02_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_02_c COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_02_c>")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_02_F decl_sion_test_02.F90)
target_link_libraries(decl_sion_02_F PDI::pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_02_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_02_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02.yml")
endif("${ENABLE_FORTRAN}")


# Testing writer/reader using the event based mechanism 

add_executable(decl_sion_03_c decl_sion_test_03.c)
target_link_libraries(decl_sion_03_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_03_c COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_03_c>")

if("${ENABLE_FORTRAN}")
add_executable(decl_sion_03_F decl_sion_test_03.F90)
target_link_libraries(decl_sion_03_F PDI::pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_03_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_03_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_sion_03_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03.yml")
endif("${ENABLE_FORTRAN}")
