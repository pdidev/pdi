#=============================================================================
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.10)
enable_language(Fortran)

# Includes
include(CTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

add_executable(01_init_finalize_F 01_init_finalize.F90)
target_link_libraries(01_init_finalize_F PDI::PDI_f90)
add_test(NAME 01_init_finalize_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:01_init_finalize_F>" "${CMAKE_CURRENT_SOURCE_DIR}/01-03_config.yml")
set_property(TEST 01_init_finalize_F PROPERTY TIMEOUT 15)

add_executable(02_share_F 02_share.F90)
target_link_libraries(02_share_F PDI::PDI_f90)
add_test(NAME 02_share_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:02_share_F>" "${CMAKE_CURRENT_SOURCE_DIR}/01-03_config.yml")
set_property(TEST 02_share_F PROPERTY TIMEOUT 15)

add_executable(03_access_F 03_access.F90)
target_link_libraries(03_access_F PDI::PDI_f90)
add_test(NAME 03_access_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:03_access_F>" "${CMAKE_CURRENT_SOURCE_DIR}/01-03_config.yml")
set_property(TEST 03_access_F PROPERTY TIMEOUT 15)
