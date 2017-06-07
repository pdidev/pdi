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

###############
#   TEST 01   #
###############
add_executable(User_Code_01_C EXCLUDE_FROM_ALL User_Code_test_01.c)
add_compiler_flags(User_Code_01_C PRIVATE -Wall -Wextra)
target_link_libraries(User_Code_01_C pdi mpi)
add_test(NAME User_Code_01_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target User_Code_01_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME User_Code_01_C_run COMMAND User_Code_01_C "${CMAKE_CURRENT_SOURCE_DIR}/User_Code_test_01.yml")
set_property(TEST User_Code_01_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST User_Code_01_C_run PROPERTY DEPENDS User_Code_01_C_build)
set_tests_properties(User_Code_01_C_run PROPERTIES DEPENDS User_Code_01_C_build TIMEOUT 3)

# 
# add_executable(User_Code_01_F EXCLUDE_FROM_ALL User_Code_test_01.f90)
# add_compiler_flags(User_Code_01_F PRIVATE -Wall -Wextra)
# target_link_libraries(User_Code_01_F pdi_f90 mpi_f90)
# add_test(NAME User_Code_01_F_build
# 		COMMAND "${CMAKE_COMMAND}" --build . --target User_Code_01_F
# 		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
# 
# add_test(NAME User_Code_01_F_run COMMAND User_Code_01_F "${CMAKE_CURRENT_SOURCE_DIR}/User_Code_test_01.yml")
# set_property(TEST User_Code_01_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
# set_property(TEST User_Code_01_F_run PROPERTY DEPENDS User_Code_01_F_build)
# set_tests_properties(User_Code_01_F_run PROPERTIES DEPENDS User_Code_01_F_build TIMEOUT 3)
# 
# 
# 
# ###############
# #   TEST 02   #
# ###############
# add_executable(User_Code_02_C EXCLUDE_FROM_ALL User_Code_test_02.c)
# add_compiler_flags(User_Code_02_C PRIVATE -Wall -Wextra)
# target_link_libraries(User_Code_02_C pdi mpi)
# add_test(NAME User_Code_02_C_build
# 		COMMAND "${CMAKE_COMMAND}" --build . --target User_Code_02_C
# 		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
# 
# add_test(NAME User_Code_02_C_run COMMAND "mpirun" -n 2 User_Code_02_C "${CMAKE_CURRENT_SOURCE_DIR}/User_Code_test_02_C.yml")
# set_property(TEST User_Code_02_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
# set_property(TEST User_Code_02_C_run PROPERTY DEPENDS User_Code_02_C_build)
# set_tests_properties(User_Code_02_C_run PROPERTIES DEPENDS User_Code_02_C_build TIMEOUT 3)
# 
# add_executable(User_Code_02_F EXCLUDE_FROM_ALL User_Code_test_02.f90)
# add_compiler_flags(User_Code_02_F PRIVATE -Wall -Wextra)
# target_link_libraries(User_Code_02_F pdi_f90 mpi_f90)
# add_test(NAME User_Code_02_F_build
# 		COMMAND "${CMAKE_COMMAND}" --build . --target User_Code_02_F
# 		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
# 
# add_test(NAME User_Code_02_F_run COMMAND "mpirun" -n 2 User_Code_02_F "${CMAKE_CURRENT_SOURCE_DIR}/User_Code_test_02_f90.yml")
# set_property(TEST User_Code_02_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
# set_property(TEST User_Code_02_F_run PROPERTY DEPENDS User_Code_02_F_build)
# set_tests_properties(User_Code_02_F_run PROPERTIES DEPENDS User_Code_02_F_build TIMEOUT 3)
# 
# 
# ###############
# #   TEST 03   #
# ###############
# add_test(NAME User_Code_03_F_run COMMAND "mpirun" -n 2 User_Code_02_F "${CMAKE_CURRENT_SOURCE_DIR}/User_Code_test_03_f90.yml" "test_03_on")
# set_property(TEST User_Code_03_F_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
# set_property(TEST User_Code_03_F_run PROPERTY DEPENDS User_Code_02_F_build)
# set_tests_properties(User_Code_03_F_run PROPERTIES DEPENDS User_Code_02_F_build TIMEOUT 3)
# 
# 
