#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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

include(AddCCompilerFlags)

find_library(LIB_M m DOC "The math library")

add_executable(PDI_example EXCLUDE_FROM_ALL example.c)
set_target_properties(PDI_example PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED ON)
add_compiler_flags(PDI_example PRIVATE -Wall -Wextra -std=c99)
target_link_libraries(PDI_example pdi mpi)
add_test(NAME PDI_example_build
		COMMAND "${CMAKE_COMMAND}" --build . --target PDI_example
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_executable(PDI_example_f90 EXCLUDE_FROM_ALL example.f90)
target_link_libraries(PDI_example_f90 pdi_f90 mpi_f90)
add_test(NAME PDI_example_f90_build
		COMMAND "${CMAKE_COMMAND}" --build . --target PDI_example_f90
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME PDI_example_1_file COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_SOURCE_DIR}/example.yml" "${CMAKE_CURRENT_BINARY_DIR}/example.yml")

add_test(NAME PDI_example_1_C COMMAND "mpirun" -n 2 ./PDI_example "${CMAKE_CURRENT_SOURCE_DIR}/example.yml")
set_property(TEST PDI_example_1_C PROPERTY DEPENDS PDI_example_build PDI_example_1_file)
set_property(TEST PDI_example_1_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/HDF5_per_process/")
set_property(TEST PDI_example_1_C PROPERTY TIMEOUT 5)

add_test(NAME PDI_example_1_F COMMAND "mpirun" -n 2 ./PDI_example_f90 "${CMAKE_CURRENT_SOURCE_DIR}/example.yml")
set_property(TEST PDI_example_1_F PROPERTY DEPENDS PDI_example_f90_build PDI_example_1_file)
set_property(TEST PDI_example_1_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/HDF5_per_process/")
set_property(TEST PDI_example_1_F PROPERTY TIMEOUT 5)

add_test(NAME PDI_example_2_fti  COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_SOURCE_DIR}/example2.fti" "${CMAKE_CURRENT_BINARY_DIR}/example2.fti")
add_test(NAME PDI_example_2_file COMMAND "${CMAKE_COMMAND}" -E copy "${CMAKE_CURRENT_SOURCE_DIR}/example2.yml" "${CMAKE_CURRENT_BINARY_DIR}/example2.yml")
set_property(TEST PDI_example_2_file PROPERTY DEPENDS PDI_example_2_fti)

add_test(NAME PDI_example_2_C COMMAND "mpirun" -n 6 ./PDI_example "${CMAKE_CURRENT_SOURCE_DIR}/example2.yml")
set_property(TEST PDI_example_2_C PROPERTY DEPENDS PDI_example_build PDI_example_2_file)
set_property(TEST PDI_example_2_C APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/FTI/")
set_property(TEST PDI_example_2_C PROPERTY TIMEOUT 120)

add_test(NAME PDI_example_2_F COMMAND "mpirun" -n 6 ./PDI_example_f90 "${CMAKE_CURRENT_SOURCE_DIR}/example2.yml")
set_property(TEST PDI_example_2_F PROPERTY DEPENDS PDI_example_f90_build PDI_example_2_file)
set_property(TEST PDI_example_2_F APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../plugins/FTI/")
set_property(TEST PDI_example_2_F PROPERTY TIMEOUT 120)
