#=============================================================================
# Copyright (C) 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================


cmake_minimum_required(VERSION 3.22...4.2)

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_decl_netcdf_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_decl_netcdf_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)


add_executable(decl_netcdf_tests decl_netcdf_tests.cxx)
target_link_libraries(decl_netcdf_tests PDI::PDI_plugins Threads::Threads GTest::gtest GTest::gtest_main)
target_compile_features(decl_netcdf_tests PUBLIC cxx_std_17)
gtest_discover_tests(decl_netcdf_tests)

if("${BUILD_NETCDF_PARALLEL}")
     find_package(MPI REQUIRED COMPONENTS C)
     add_executable(netcdf_mpi_tests decl_netcdf_mpi_tests.c)
     target_link_libraries(netcdf_mpi_tests PDI::PDI_C MPI::MPI_C)
     target_compile_features(netcdf_mpi_tests PUBLIC cxx_std_17)
     add_test(NAME netcdf_mpi_tests COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS}  "$<TARGET_FILE:netcdf_mpi_tests>" ${MPIEXEC_POSTFLAGS})
     set_property(TEST netcdf_mpi_tests PROPERTY TIMEOUT 15)
     set_property(TEST netcdf_mpi_tests PROPERTY PROCESSORS 4)
endif()

add_executable(decl_netcdf_08_C decl_netcdf_test_08.c)
target_link_libraries(decl_netcdf_08_C PDI::PDI_C)
target_compile_features(decl_netcdf_08_C PUBLIC cxx_std_17)
add_test(NAME decl_netcdf_08_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_08_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_08.yml")
set_property(TEST decl_netcdf_08_C PROPERTY TIMEOUT 15)

add_executable(decl_netcdf_09_C decl_netcdf_test_09.c)
target_link_libraries(decl_netcdf_09_C PDI::PDI_C)
target_compile_features(decl_netcdf_09_C PUBLIC cxx_std_17)
add_test(NAME decl_netcdf_09_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_09_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_09.yml")
set_property(TEST decl_netcdf_09_C PROPERTY TIMEOUT 15)
