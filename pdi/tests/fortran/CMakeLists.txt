# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
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
