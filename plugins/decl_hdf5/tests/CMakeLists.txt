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
add_executable(decl_hdf5_01_C EXCLUDE_FROM_ALL decl_hdf5_test_01.c)
target_link_libraries(decl_hdf5_01_C pdi mpi)
add_test(NAME decl_hdf5_01_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_01_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_01_C_run COMMAND decl_hdf5_01_C "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_01_C_run PROPERTY DEPENDS decl_hdf5_01_C_build)
set_tests_properties(decl_hdf5_01_C_run PROPERTIES DEPENDS decl_hdf5_01_C_build)


add_executable(decl_hdf5_01_F EXCLUDE_FROM_ALL decl_hdf5_test_01.F90)
target_link_libraries(decl_hdf5_01_F pdi_f90 mpi_f90)
add_test(NAME decl_hdf5_01_F_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_01_F
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_01_F_run COMMAND decl_hdf5_01_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_01_F_run PROPERTY DEPENDS decl_hdf5_01_F_build)
set_tests_properties(decl_hdf5_01_F_run PROPERTIES DEPENDS decl_hdf5_01_F_build)



###############
#   TEST 02   #
###############
add_executable(decl_hdf5_02_C EXCLUDE_FROM_ALL decl_hdf5_test_02.c)
target_link_libraries(decl_hdf5_02_C pdi mpi)
add_test(NAME decl_hdf5_02_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_02_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_02_C_run COMMAND "mpirun" -n 2 decl_hdf5_02_C "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_02_C.yml")
set_property(TEST decl_hdf5_02_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_02_C_run PROPERTY DEPENDS decl_hdf5_02_C_build)
set_tests_properties(decl_hdf5_02_C_run PROPERTIES DEPENDS decl_hdf5_02_C_build)

add_executable(decl_hdf5_02_F EXCLUDE_FROM_ALL decl_hdf5_test_02.F90)
target_link_libraries(decl_hdf5_02_F pdi_f90 mpi_f90)
add_test(NAME decl_hdf5_02_F_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_02_F
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_02_F_run COMMAND "mpirun" -n 2 decl_hdf5_02_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_02_F90.yml")
set_property(TEST decl_hdf5_02_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_02_F_run PROPERTY DEPENDS decl_hdf5_02_F_build)
set_tests_properties(decl_hdf5_02_F_run PROPERTIES DEPENDS decl_hdf5_02_F_build)


###############
#   TEST 03   #
###############
add_test(NAME decl_hdf5_03_F_run COMMAND "mpirun" -n 2 decl_hdf5_02_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_03_F90.yml" "test_03_on")
set_property(TEST decl_hdf5_03_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_03_F_run PROPERTY DEPENDS decl_hdf5_02_F_build)
set_tests_properties(decl_hdf5_03_F_run PROPERTIES DEPENDS decl_hdf5_02_F_build)


###############
#   TEST 04   #
###############

add_executable(decl_hdf5_04_C EXCLUDE_FROM_ALL decl_hdf5_test_04.c)
target_link_libraries(decl_hdf5_04_C pdi mpi)
add_test(NAME decl_hdf5_04_C_build
	COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_04_C
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_04_C_run COMMAND "mpirun" -n 4 decl_hdf5_04_C "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04_C.yml")
set_property(TEST decl_hdf5_04_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_04_C_run PROPERTY DEPENDS decl_hdf5_04_C_build)
set_tests_properties(decl_hdf5_04_C_run PROPERTIES DEPENDS decl_hdf5_04_C_build)

add_executable(decl_hdf5_04_F EXCLUDE_FROM_ALL decl_hdf5_test_04.F90)
target_link_libraries(decl_hdf5_04_F pdi_f90 mpi_f90)
add_test(NAME decl_hdf5_04_F_build
	COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_04_F
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_04_F_run COMMAND "mpirun" -n 4 decl_hdf5_04_F "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04_F90.yml")
set_property(TEST decl_hdf5_04_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_04_F_run PROPERTY DEPENDS decl_hdf5_04_F_build)
set_tests_properties(decl_hdf5_04_F_run PROPERTIES DEPENDS decl_hdf5_04_F_build) 


################ 
##   TEST 05   # 
################

add_executable(decl_hdf5_05_C EXCLUDE_FROM_ALL decl_hdf5_test_05.c)
target_link_libraries(decl_hdf5_05_C pdi mpi)
add_test(NAME decl_hdf5_05_C_build
	COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_05_C
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME decl_hdf5_05_C_run COMMAND "mpirun" -n 4 decl_hdf5_05_C "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_05_C.yml")
set_property(TEST decl_hdf5_05_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST decl_hdf5_05_C_run PROPERTY DEPENDS decl_hdf5_05_C_build)
set_tests_properties(decl_hdf5_05_C_run PROPERTIES DEPENDS decl_hdf5_05_C_build)


################ 
##   TEST 06   # 
################

# Sequential and Parallel HDF5
if( ${ENABLE_DECL_HDF5} ) 
	add_executable(decl_hdf5_06_C EXCLUDE_FROM_ALL decl_hdf5_test_06.c)
	target_link_libraries(decl_hdf5_06_C pdi mpi)
	add_test(NAME decl_hdf5_06_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target decl_hdf5_06_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
	add_test(NAME decl_hdf5_06_C_run COMMAND "mpirun" -n 4 decl_hdf5_06_C "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_06_C.yml")
	set_property(TEST decl_hdf5_06_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
	set_property(TEST decl_hdf5_06_C_run PROPERTY DEPENDS decl_hdf5_06_C_build)
	set_tests_properties(decl_hdf5_06_C_run PROPERTIES DEPENDS decl_hdf5_06_C_build)
endif()
