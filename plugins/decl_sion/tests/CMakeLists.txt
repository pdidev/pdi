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

###############
#   TEST 01   #
###############
add_executable(decl_sion_01_c EXCLUDE_FROM_ALL decl_sion_test_01.c)
target_link_libraries(decl_sion_01_c pdi mpi)
add_test(NAME decl_sion_01_c_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_01_c
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_01_c_run COMMAND decl_sion_01_c "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
set_property(TEST decl_sion_01_c_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_01_c_run PROPERTY DEPENDS decl_sion_01_c_build)
set_tests_properties(decl_sion_01_c_run PROPERTIES DEPENDS decl_sion_01_c_build)


add_executable(decl_sion_01_F EXCLUDE_FROM_ALL decl_sion_test_01.F90)
target_link_libraries(decl_sion_01_F pdi_f90 mpi_f90)
add_test(NAME decl_sion_01_F_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_01_F
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_01_F_run COMMAND decl_sion_01_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
set_property(TEST decl_sion_01_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_01_F_run PROPERTY DEPENDS decl_sion_01_F_build)
set_tests_properties(decl_sion_01_F_run PROPERTIES DEPENDS decl_sion_01_F_build)



###############
#   TEST 02   #
###############
add_executable(decl_sion_02_c EXCLUDE_FROM_ALL decl_sion_test_02.c)
target_link_libraries(decl_sion_02_c pdi mpi)
add_test(NAME decl_sion_02_c_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_02_c
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_02_c_run COMMAND "mpirun" -n 2 ./decl_sion_02_c "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02_c.yml")
set_property(TEST decl_sion_02_c_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_02_c_run PROPERTY DEPENDS decl_sion_02_c_build)
set_tests_properties(decl_sion_02_c_run PROPERTIES DEPENDS decl_sion_02_c_build)

add_executable(decl_sion_02_F EXCLUDE_FROM_ALL decl_sion_test_02.F90)
target_link_libraries(decl_sion_02_F pdi_f90 mpi_f90)
add_test(NAME decl_sion_02_F_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_02_F
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_02_F_run COMMAND "mpirun" -n 2 ./decl_sion_02_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02_F90.yml")
set_property(TEST decl_sion_02_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_02_F_run PROPERTY DEPENDS decl_sion_02_F_build)
set_tests_properties(decl_sion_02_F_run PROPERTIES DEPENDS decl_sion_02_F_build)


###############
#   TEST 03   #
###############
add_executable(decl_sion_03_c EXCLUDE_FROM_ALL decl_sion_test_03.c)
target_link_libraries(decl_sion_03_c pdi mpi)
add_test(NAME decl_sion_03_c_build
 		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_03_c
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_03_c_run COMMAND "mpirun" -n 2 ./decl_sion_03_c "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03_c.yml")
set_property(TEST decl_sion_03_c_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_03_c_run PROPERTY DEPENDS decl_sion_03_c_build)
set_tests_properties(decl_sion_03_c_run PROPERTIES DEPENDS decl_sion_03_c_build)

add_executable(decl_sion_03_F EXCLUDE_FROM_ALL decl_sion_test_03.F90)
target_link_libraries(decl_sion_03_F pdi_f90 mpi_f90)
add_test(NAME decl_sion_03_F_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_sion_03_F
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_sion_03_F_run COMMAND "mpirun" -n 2 ./decl_sion_03_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03_F90.yml")
set_property(TEST decl_sion_03_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_sion_03_F_run PROPERTY DEPENDS decl_sion_03_F_build)
set_tests_properties(decl_sion_03_F_run PROPERTIES DEPENDS decl_sion_03_F_build)
