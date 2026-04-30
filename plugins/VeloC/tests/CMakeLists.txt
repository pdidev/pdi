#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# Neither the name of CEA nor the names of its contributors may be used to
# endorse or promote products derived from this software without specific
# prior written permission.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.22...4.2)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_veloc_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_veloc_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

# HDF5
set(HDF5_USE_STATIC_LIBRARIES OFF)
set(HDF5_PREFER_PARALLEL "${BUILD_HDF5_PARALLEL}")
find_package(HDF5 REQUIRED COMPONENTS C)
if("${HDF5_VERSION}" VERSION_LESS 1.10)
    message(FATAL_ERROR "HDF5 version 1.10 at least required, HDF5 ${HDF5_VERSION} found.")
endif()
set(HDF5_DEPS hdf5::hdf5)

# Create persistent shared workdirs and pre-copy config at configure time
set(VELOC_TEST_01_WORKDIR "${CMAKE_CURRENT_BINARY_DIR}/workdir_01")
set(VELOC_TEST_03_WORKDIR "${CMAKE_CURRENT_BINARY_DIR}/workdir_03")
file(MAKE_DIRECTORY "${VELOC_TEST_01_WORKDIR}")
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/veloc_config.cfg" DESTINATION "${VELOC_TEST_01_WORKDIR}")
file(MAKE_DIRECTORY "${VELOC_TEST_03_WORKDIR}")
file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/veloc_config.cfg" DESTINATION "${VELOC_TEST_03_WORKDIR}")


# test_0 - standalone, uses runtest-dir for isolated temp dir
add_executable(veloc_test_0 veloc_test_0.cxx)
target_link_libraries(veloc_test_0 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_0
    COMMAND "${RUNTEST_DIR}"
        --runtest-dir-copy-file "${CMAKE_CURRENT_SOURCE_DIR}/veloc_config.cfg"
        "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_0>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_0 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_0 PROPERTY PROCESSORS 1)


# test_01 series - shared workdir so checkpoint files persist between tests
add_executable(veloc_test_01_1 veloc_test_01_1.cxx)
target_link_libraries(veloc_test_01_1 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_01_1
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_01_1>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_01_1 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_01_WORKDIR}")
set_property(TEST veloc_test_01_1 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_01_1 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_01_1 PROPERTY FIXTURES_SETUP fixture_01)


add_executable(veloc_test_01_2 veloc_test_01_2.cxx)
target_link_libraries(veloc_test_01_2 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_01_2
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_01_2>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_01_2 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_01_WORKDIR}")
set_property(TEST veloc_test_01_2 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_01_2 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_01_2 PROPERTY FIXTURES_REQUIRED fixture_01)


add_executable(veloc_test_01_3 veloc_test_01_3.cxx)
target_link_libraries(veloc_test_01_3 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_01_3
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_01_3>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_01_3 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_01_WORKDIR}")
set_property(TEST veloc_test_01_3 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_01_3 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_01_3 PROPERTY FIXTURES_REQUIRED fixture_01)


# test_02 - standalone, uses runtest-dir for isolated temp dir
add_executable(veloc_test_02 veloc_test_02.cxx)
target_link_libraries(veloc_test_02 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_02
    COMMAND "${RUNTEST_DIR}"
        --runtest-dir-copy-file "${CMAKE_CURRENT_SOURCE_DIR}/veloc_config.cfg"
        "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_02>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_02 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_02 PROPERTY PROCESSORS 1)


# test_03 series - shared workdir so HDF5 checkpoint files persist between tests
add_executable(veloc_test_03_1 veloc_test_03_1.cxx)
target_link_libraries(veloc_test_03_1 PDI::PDI_C MPI::MPI_C ${HDF5_DEPS})
add_test(NAME veloc_test_03_1
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_03_1>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_03_1 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_03_WORKDIR}")
set_property(TEST veloc_test_03_1 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_03_1 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_03_1 PROPERTY FIXTURES_SETUP fixture_03)


add_executable(veloc_test_03_2 veloc_test_03_2.cxx)
target_link_libraries(veloc_test_03_2 PDI::PDI_C MPI::MPI_C ${HDF5_DEPS})
add_test(NAME veloc_test_03_2
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_03_2>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_03_2 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_03_WORKDIR}")
set_property(TEST veloc_test_03_2 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_03_2 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_03_2 PROPERTY FIXTURES_REQUIRED fixture_03)


add_executable(veloc_test_03_3 veloc_test_03_3.cxx)
target_link_libraries(veloc_test_03_3 PDI::PDI_C MPI::MPI_C ${HDF5_DEPS})
add_test(NAME veloc_test_03_3
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_03_3>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_03_3 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_03_WORKDIR}")
set_property(TEST veloc_test_03_3 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_03_3 PROPERTY PROCESSORS 1)
set_property(TEST veloc_test_03_3 PROPERTY FIXTURES_REQUIRED fixture_03)
