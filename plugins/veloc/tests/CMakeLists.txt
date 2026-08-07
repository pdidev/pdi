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

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")
set(VELOC_CONFIG_FILE_NAME "veloc_config.cfg")
set(VELOC_TEST_NPROC 3)

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
find_package(HDF5 REQUIRED COMPONENTS C)
if("${HDF5_VERSION}" VERSION_LESS 1.10)
    message(FATAL_ERROR "HDF5 version 1.10 at least required, HDF5 ${HDF5_VERSION} found.")
endif()

set(VELOC_TEST_YAML_WORKDIR    "${CMAKE_CURRENT_BINARY_DIR}/workdir_yaml_tests")
set(VELOC_TEST_MANAGED_WORKDIR "${CMAKE_CURRENT_BINARY_DIR}/workdir_managed_tests")
set(VELOC_TEST_CUSTOM_WORKDIR  "${CMAKE_CURRENT_BINARY_DIR}/workdir_custom_tests")
foreach(workdir IN ITEMS
        "${VELOC_TEST_YAML_WORKDIR}"
        "${VELOC_TEST_MANAGED_WORKDIR}"
        "${VELOC_TEST_CUSTOM_WORKDIR}")
	file(MAKE_DIRECTORY "${workdir}")
	file(COPY "${CMAKE_CURRENT_SOURCE_DIR}/${VELOC_CONFIG_FILE_NAME}" DESTINATION "${workdir}")
endforeach()

# veloc_test_yaml_syntax
add_executable(veloc_test_yaml_syntax veloc_test_yaml_syntax.cxx)
target_link_libraries(veloc_test_yaml_syntax PDI::PDI_C MPI::MPI_C GTest::gmock GTest::gtest GTest::gtest_main)
target_compile_definitions(veloc_test_yaml_syntax PRIVATE
    VELOC_CONFIG_FILE="${VELOC_TEST_YAML_WORKDIR}/${VELOC_CONFIG_FILE_NAME}")
gtest_discover_tests(veloc_test_yaml_syntax
    WORKING_DIRECTORY "${VELOC_TEST_YAML_WORKDIR}"
    TEST_PREFIX veloc_test_yaml_syntax.
    PROPERTIES
        TIMEOUT 15
)

# Erases checkpoints from previous ctest runs
add_test(NAME veloc_managed_prepare
    COMMAND "${CMAKE_COMMAND}" -E rm -rf
        "${VELOC_TEST_MANAGED_WORKDIR}/scratchdir"
        "${VELOC_TEST_MANAGED_WORKDIR}/persdir")
set_property(TEST veloc_managed_prepare PROPERTY TIMEOUT 15)
set_property(TEST veloc_managed_prepare PROPERTY FIXTURES_SETUP managed_prepare)

# veloc_test_managed_1
add_executable(veloc_test_managed_1 veloc_test_managed_1.cxx)
target_link_libraries(veloc_test_managed_1 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_managed_1
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" ${VELOC_TEST_NPROC} ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_managed_1>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_managed_1 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_MANAGED_WORKDIR}")
set_property(TEST veloc_test_managed_1 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_managed_1 PROPERTY PROCESSORS ${VELOC_TEST_NPROC})
set_property(TEST veloc_test_managed_1 PROPERTY FIXTURES_REQUIRED managed_prepare)
set_property(TEST veloc_test_managed_1 PROPERTY FIXTURES_SETUP managed_ckpt)

# veloc_test_managed_2
add_executable(veloc_test_managed_2 veloc_test_managed_2.cxx)
target_link_libraries(veloc_test_managed_2 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_managed_2
    COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" ${VELOC_TEST_NPROC} ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_managed_2>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_managed_2 PROPERTY WORKING_DIRECTORY "${VELOC_TEST_MANAGED_WORKDIR}")
set_property(TEST veloc_test_managed_2 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_managed_2 PROPERTY PROCESSORS ${VELOC_TEST_NPROC})
set_property(TEST veloc_test_managed_2 PROPERTY FIXTURES_REQUIRED managed_ckpt)

add_test(NAME veloc_managed_cleanup
    COMMAND "${CMAKE_COMMAND}" -E rm -rf
        "${VELOC_TEST_MANAGED_WORKDIR}/scratchdir"
        "${VELOC_TEST_MANAGED_WORKDIR}/persdir")
set_property(TEST veloc_managed_cleanup PROPERTY TIMEOUT 15)
set_property(TEST veloc_managed_cleanup PROPERTY FIXTURES_CLEANUP managed_ckpt)

# veloc_test_managed_3 
add_executable(veloc_test_managed_3 veloc_test_managed_3.cxx)
target_link_libraries(veloc_test_managed_3 PDI::PDI_C MPI::MPI_C)
add_test(NAME veloc_test_managed_3
    COMMAND "${RUNTEST_DIR}"
        --runtest-dir-copy-file "${CMAKE_CURRENT_SOURCE_DIR}/${VELOC_CONFIG_FILE_NAME}"
        "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" ${VELOC_TEST_NPROC} ${MPIEXEC_PREFLAGS}
        "$<TARGET_FILE:veloc_test_managed_3>" ${MPIEXEC_POSTFLAGS})
set_property(TEST veloc_test_managed_3 PROPERTY TIMEOUT 15)
set_property(TEST veloc_test_managed_3 PROPERTY PROCESSORS ${VELOC_TEST_NPROC})

# veloc_test_custom
add_executable(veloc_test_custom veloc_test_custom.cxx)
target_link_libraries(veloc_test_custom PDI::PDI_C hdf5::hdf5 MPI::MPI_C GTest::gmock GTest::gtest GTest::gtest_main)
target_compile_definitions(veloc_test_custom PRIVATE
    VELOC_CONFIG_FILE="${VELOC_TEST_CUSTOM_WORKDIR}/${VELOC_CONFIG_FILE_NAME}")
gtest_discover_tests(veloc_test_custom
    WORKING_DIRECTORY "${VELOC_TEST_CUSTOM_WORKDIR}"
    TEST_PREFIX veloc_test_custom.
    PROPERTIES
        TIMEOUT 15
)