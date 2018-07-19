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

cmake_minimum_required(VERSION 3.5)
project(pdi_examples LANGUAGES C CXX)

if("${ENABLE_FORTRAN}")
	enable_language(Fortran)
endif()

# find_package(pdi REQUIRED)
find_library(LIB_M m DOC "The math library")

# Default standard in the project is C99 & C++11
set(CMAKE_CXX_STANDARD 11)
set(CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_C_STANDARD 99)
set(C_STANDARD_REQUIRED TRUE)

add_executable(PDI_example_C example.c)
target_link_libraries(PDI_example_C pdi MPI::MPI_C m)

if("${ENABLE_FORTRAN}")
add_executable(PDI_example_F example.F90)
target_link_libraries(PDI_example_F pdi_f90 MPI::MPI_Fortran m)
endif("${ENABLE_FORTRAN}")

set(RUNTEST_DIR "${CMAKE_SOURCE_DIR}/cmake/runtest-dir")

if("${ENABLE_DECL_HDF5}")
add_test(NAME PDI_example_decl_hdf5_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5.yml")
set_property(TEST PDI_example_decl_hdf5_C APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_hdf5/:${CMAKE_CURRENT_BINARY_DIR}/../plugins/mpi/")
set_property(TEST PDI_example_decl_hdf5_C PROPERTY TIMEOUT 5)

if("${ENABLE_FORTRAN}")
add_test(NAME PDI_example_decl_hdf5_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5.yml")
set_property(TEST PDI_example_decl_hdf5_F APPEND PROPERTY ENVIRONMENT
	"LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_hdf5/:${CMAKE_CURRENT_BINARY_DIR}/../plugins/mpi/")
set_property(TEST PDI_example_decl_hdf5_F PROPERTY TIMEOUT 5)
endif("${ENABLE_FORTRAN}")
endif("${ENABLE_DECL_HDF5}")

if("${ENABLE_DECL_SION}")
add_test(NAME PDI_example_decl_sion_variables_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_variables.yml")
set_property(TEST PDI_example_decl_sion_variables_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_sion/")
set_property(TEST PDI_example_decl_sion_variables_C PROPERTY TIMEOUT 5)

if("${ENABLE_FORTRAN}")
add_test(NAME PDI_example_decl_sion_variables_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_variables.yml")
set_property(TEST PDI_example_decl_sion_variables_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_sion/")
set_property(TEST PDI_example_decl_sion_variables_F PROPERTY TIMEOUT 5)
endif("${ENABLE_FORTRAN}")

add_test(NAME PDI_example_decl_sion_files_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_files.yml")
set_property(TEST PDI_example_decl_sion_files_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_sion/")
set_property(TEST PDI_example_decl_sion_files_C PROPERTY TIMEOUT 5)

if("${ENABLE_FORTRAN}")
add_test(NAME PDI_example_decl_sion_files_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_files.yml")
set_property(TEST PDI_example_decl_sion_files_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/decl_sion/")
set_property(TEST PDI_example_decl_sion_files_F PROPERTY TIMEOUT 5)
endif("${ENABLE_FORTRAN}")
endif("${ENABLE_DECL_SION}")

if("${ENABLE_FTI}")
add_test(NAME PDI_example_fti_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config.ini" "mpirun" -n 6 "$<TARGET_FILE:PDI_example_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti.yml")
set_property(TEST PDI_example_fti_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/fti/")
set_property(TEST PDI_example_fti_C PROPERTY TIMEOUT 180)

if("${ENABLE_FORTRAN}")
add_test(NAME PDI_example_fti_F COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config.ini" "mpirun" -n 6 "$<TARGET_FILE:PDI_example_F>" "${CMAKE_CURRENT_SOURCE_DIR}/fti.yml")
set_property(TEST PDI_example_fti_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/fti/")
set_property(TEST PDI_example_fti_F PROPERTY TIMEOUT 180)
endif("${ENABLE_FORTRAN}")
endif("${ENABLE_FTI}")

add_test(NAME PDI_example_test_C COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test.yml")
set_property(TEST PDI_example_test_C PROPERTY DEPENDS PDI_example_test_file)
set_property(TEST PDI_example_test_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/test/")
set_property(TEST PDI_example_test_C PROPERTY TIMEOUT 5)

if("${ENABLE_FORTRAN}")
add_test(NAME PDI_example_test_F COMMAND "${RUNTEST_DIR}" "mpirun" -n 3 "$<TARGET_FILE:PDI_example_F>" "${CMAKE_CURRENT_SOURCE_DIR}/test.yml")
set_property(TEST PDI_example_test_F PROPERTY DEPENDS PDI_example_test_file)
set_property(TEST PDI_example_test_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/test/")
set_property(TEST PDI_example_test_F PROPERTY TIMEOUT 5)
endif("${ENABLE_FORTRAN}")
