#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

##########
# FTI init by event test 

add_executable(fti_01_C fti_test_01.c)
target_link_libraries(fti_01_C PDI::pdi MPI::MPI_C fti.shared)

## without heads, implicit MPI_COMM_WORLD
add_test(NAME fti_01-1_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_01.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_01-1.yml")

## with heads, implicit MPI_COMM_WORLD
add_test(NAME fti_01-2_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_01-2.yml")

## without heads, explicit MPI_COMM_WORLD
add_test(NAME fti_01-3_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_01.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_01-3.yml")

## with heads, explicit MPI_COMM_WORLD
add_test(NAME fti_01-4_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_01-4.yml")

##########
# FTI init when loading plugin test

add_executable(fti_02_C fti_test_02.c)
target_link_libraries(fti_02_C PDI::pdi MPI::MPI_C fti.shared)

## without heads, implicit MPI_COMM_WORLD
add_test(NAME fti_02-1_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_01.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_02-1.yml")

## with heads, implicit MPI_COMM_WORLD
add_test(NAME fti_02-2_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_02-2.yml")

## without heads, explicit MPI_COMM_WORLD
add_test(NAME fti_02-3_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_01.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_02-3.yml")

## with heads, explicit MPI_COMM_WORLD
add_test(NAME fti_02-4_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_02-4.yml")

##########
# FTI init by communicator exposure test

add_executable(fti_03_C fti_test_03.c)
target_link_libraries(fti_03_C PDI::pdi MPI::MPI_C fti.shared)

## without heads
add_test(NAME fti_03-1_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_01.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_03-1.yml")

## with heads 
add_test(NAME fti_03-2_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_03-2.yml")

##########
# FTI checkpoint and recover test 

add_executable(fti_04_C fti_test_04.c)
target_link_libraries(fti_04_C PDI::pdi MPI::MPI_C)

## check L1 ckpt
add_test(NAME fti_04-1_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_04-1.yml")

## check L2 ckpt
add_test(NAME fti_04-2_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_04-2.yml")

## check L3 ckpt
add_test(NAME fti_04-3_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_04-3.yml")

## check L4 ckpt
add_test(NAME fti_04-4_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_04-4.yml")

##########
# FTI snapshot test 

add_executable(fti_05_C fti_test_05.c)
target_link_libraries(fti_05_C PDI::pdi MPI::MPI_C)

## without heads
add_test(NAME fti_05-1_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_05-1.yml")

## with heads - CURRENTLY NOT WORKING, because heads are killed by FTI after FTI_Finalize
add_test(NAME fti_05-2_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_04.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_05-2.yml")

##########
# FTI head status test

add_executable(fti_06_C fti_test_06.c)
target_link_libraries(fti_06_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_06_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_02.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_06_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_06.yml")

##########
# FTI stage dir test

add_executable(fti_07_C fti_test_07.c)
target_link_libraries(fti_07_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_07_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_05.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_07_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_07.yml")

##########
# FTI variable size test

add_executable(fti_08_C fti_test_08.c)
target_link_libraries(fti_08_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_08_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_08_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_08.yml")
set_property(TEST fti_08_C PROPERTY DISABLED TRUE) #TODO: this test currently fails

##########
# FTI dynamic array recover test

add_executable(fti_09_C fti_test_09.c)
target_link_libraries(fti_09_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_09_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_09_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_09.yml")

##########
# FTI recover_var test

add_executable(fti_10_C fti_test_10.c)
target_link_libraries(fti_10_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_10_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_03.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_10_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_10.yml")

##########
# FTI send_file test (probably bugged in current FTI version)

add_executable(fti_11_C fti_test_11.c)
target_link_libraries(fti_11_C PDI::pdi MPI::MPI_C)
add_test(NAME fti_11_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config_05.ini" "mpirun" -n 6 "$<TARGET_FILE:fti_11_C>" "${CMAKE_CURRENT_SOURCE_DIR}/fti_test_11.yml")
set_property(TEST fti_11_C PROPERTY DISABLED TRUE) #TODO: bugged in current FTI version
