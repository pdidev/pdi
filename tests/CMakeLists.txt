# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_tests LANGUAGES C)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

option(BUILD_DECL_HDF5_PLUGIN "Build Decl'HDF5 plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN "Build Serialize plug-in" ON)

include(CTest)

find_package(paraconf REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_01_C test_01.c)
target_link_libraries(test_01_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_01_C COMMAND "$<TARGET_FILE:test_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_01.yml")

add_executable(test_02_C test_02.c)
target_link_libraries(test_02_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_02_C COMMAND "$<TARGET_FILE:test_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_02.yml")

add_executable(test_03_C test_03.c)
target_link_libraries(test_03_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_03_C COMMAND "$<TARGET_FILE:test_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_03.yml")

add_executable(test_04_C test_04.c)
target_link_libraries(test_04_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_04_C COMMAND "$<TARGET_FILE:test_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_04.yml")

endif("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
if("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_05_C test_05.c)
target_link_libraries(test_05_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_05_C COMMAND "$<TARGET_FILE:test_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_05.yml")
#TODO: NetCDF error https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/433
set_tests_properties(test_05_C PROPERTIES DISABLED TRUE)

endif("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
