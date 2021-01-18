#=============================================================================
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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


cmake_minimum_required(VERSION 3.5)

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

add_executable(decl_netcdf_01_C decl_netcdf_test_01.c)
target_link_libraries(decl_netcdf_01_C PDI::PDI_C)
add_test(NAME decl_netcdf_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_01.yml")
set_property(TEST decl_netcdf_01_C PROPERTY TIMEOUT 5)

add_executable(decl_netcdf_02_C decl_netcdf_test_02.c)
target_link_libraries(decl_netcdf_02_C PDI::PDI_C)
add_test(NAME decl_netcdf_02_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_02.yml")
set_property(TEST decl_netcdf_02_C PROPERTY TIMEOUT 5)

add_executable(decl_netcdf_03_C decl_netcdf_test_03.c)
target_link_libraries(decl_netcdf_03_C PDI::PDI_C)
add_test(NAME decl_netcdf_03_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_03.yml")
set_property(TEST decl_netcdf_03_C PROPERTY TIMEOUT 5)

add_executable(decl_netcdf_04_C decl_netcdf_test_04.c)
target_link_libraries(decl_netcdf_04_C PDI::PDI_C)
add_test(NAME decl_netcdf_04_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_04.yml")
set_property(TEST decl_netcdf_04_C PROPERTY TIMEOUT 5)

add_executable(decl_netcdf_05_C decl_netcdf_test_05.c)
target_link_libraries(decl_netcdf_05_C PDI::PDI_C)
add_test(NAME decl_netcdf_05_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_05.yml")
set_property(TEST decl_netcdf_05_C PROPERTY TIMEOUT 5)

if("PARALLEL" IN_LIST NetCDF_FEATURES)
     find_package(MPI REQUIRED COMPONENTS C)
     add_executable(decl_netcdf_06_C decl_netcdf_test_06.c)
     target_link_libraries(decl_netcdf_06_C PDI::PDI_C MPI::MPI_C)
     add_test(NAME decl_netcdf_06_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS}  "$<TARGET_FILE:decl_netcdf_06_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_06.yml")
     set_property(TEST decl_netcdf_06_C PROPERTY TIMEOUT 5)
endif()

add_executable(decl_netcdf_07_C decl_netcdf_test_07.c)
target_link_libraries(decl_netcdf_07_C PDI::PDI_C)
add_test(NAME decl_netcdf_07_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_netcdf_07_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_test_07.yml")
set_property(TEST decl_netcdf_07_C PROPERTY TIMEOUT 5)
