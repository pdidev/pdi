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

##########
# TEST 01: Metatadata export using filename

add_executable(decl_hdf5_01_C decl_hdf5_test_01.c)
target_link_libraries(decl_hdf5_01_C pdi MPI::MPI_C)
add_test(NAME decl_hdf5_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")

if("${ENABLE_FORTRAN}")
add_executable(decl_hdf5_01_F decl_hdf5_test_01.F90)
target_link_libraries(decl_hdf5_01_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_hdf5_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_hdf5_01_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_01_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")

##########
# TEST 02: PDI_import/PDI_export

add_executable(decl_hdf5_02_C decl_hdf5_test_02.c)
target_link_libraries(decl_hdf5_02_C pdi MPI::MPI_C)
add_test(NAME decl_hdf5_02_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_hdf5_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_02_C.yml")
set_property(TEST decl_hdf5_02_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")

if("${ENABLE_FORTRAN}")
add_executable(decl_hdf5_02_F decl_hdf5_test_02.F90)
target_link_libraries(decl_hdf5_02_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_hdf5_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_hdf5_02_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_hdf5_02_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_02_F.yml")
set_property(TEST decl_hdf5_02_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")

##########
# TEST 03: slicing mechanism

if("${ENABLE_FORTRAN}")
add_test(NAME decl_hdf5_03_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 2 "$<TARGET_FILE:decl_hdf5_02_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_03_F.yml" "test_03_on")
set_property(TEST decl_hdf5_03_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
endif("${ENABLE_FORTRAN}")

##########
# TEST 04: parallel read/write

add_executable(decl_hdf5_04_C decl_hdf5_test_04.c)
target_link_libraries(decl_hdf5_04_C pdi MPI::MPI_C)
add_test(NAME decl_hdf5_04_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 4 "$<TARGET_FILE:decl_hdf5_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04_C.yml")
set_property(TEST decl_hdf5_04_C APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../:${CMAKE_CURRENT_BINARY_DIR}/../../mpi/")

if("${ENABLE_FORTRAN}")
add_executable(decl_hdf5_04_F decl_hdf5_test_04.F90)
target_link_libraries(decl_hdf5_04_F pdi_f90 MPI::MPI_Fortran)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_hdf5_04_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_hdf5_04_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 4 "$<TARGET_FILE:decl_hdf5_04_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04_F.yml")
set_property(TEST decl_hdf5_04_F APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../:${CMAKE_CURRENT_BINARY_DIR}/../../mpi/")
endif("${ENABLE_FORTRAN}")

##########
# TEST 05: scalars default options

add_executable(decl_hdf5_05_C decl_hdf5_test_05.c)
target_link_libraries(decl_hdf5_05_C pdi MPI::MPI_C)
add_test(NAME decl_hdf5_05_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 4 "$<TARGET_FILE:decl_hdf5_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_05_C.yml")
set_property(TEST decl_hdf5_05_C APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../:${CMAKE_CURRENT_BINARY_DIR}/../../mpi/")

##########
# TEST 06: 

add_executable(decl_hdf5_06_C decl_hdf5_test_06.c)
target_link_libraries(decl_hdf5_06_C pdi MPI::MPI_C)
add_test(NAME decl_hdf5_06_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 4 "$<TARGET_FILE:decl_hdf5_06_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_06_C.yml")
set_property(TEST decl_hdf5_06_C APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../:${CMAKE_CURRENT_BINARY_DIR}/../../mpi/")
