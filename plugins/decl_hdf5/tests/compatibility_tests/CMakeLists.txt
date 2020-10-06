#=============================================================================
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

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../../cmake/runtest-dir")

find_package(MPI REQUIRED)

add_executable(decl_hdf5_comp_01_C decl_hdf5_comp_test_01.c)
target_link_libraries(decl_hdf5_comp_01_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_01_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_01_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_01_C>")

add_executable(decl_hdf5_comp_02_C decl_hdf5_comp_test_02.c)
target_link_libraries(decl_hdf5_comp_02_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_02_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_02_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_02_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_02_C>")

add_executable(decl_hdf5_comp_03_C decl_hdf5_comp_test_03.c)
target_link_libraries(decl_hdf5_comp_03_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_03_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_03_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_03_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_03_C>")

add_executable(decl_hdf5_comp_04_C decl_hdf5_comp_test_04.c)
target_link_libraries(decl_hdf5_comp_04_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_04_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_04_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_04_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_04_C>")

add_executable(decl_hdf5_comp_05_C decl_hdf5_comp_test_05.c)
target_link_libraries(decl_hdf5_comp_05_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_05_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_05_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_05_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_05_C>")

add_executable(decl_hdf5_comp_06_C decl_hdf5_comp_test_06.c)
target_link_libraries(decl_hdf5_comp_06_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_comp_06_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_comp_06_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_comp_06_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_comp_06_C>")

add_executable(decl_hdf5_dataset_comp_01_C decl_hdf5_dataset_comp_test_01.c)
target_link_libraries(decl_hdf5_dataset_comp_01_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_dataset_comp_01_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_dataset_comp_01_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_dataset_comp_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_dataset_comp_01_C>")

add_executable(decl_hdf5_dataset_comp_02_C decl_hdf5_dataset_comp_test_02.c)
target_link_libraries(decl_hdf5_dataset_comp_02_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_dataset_comp_02_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_dataset_comp_02_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_dataset_comp_02_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_dataset_comp_02_C>")

add_executable(decl_hdf5_group_comp_01_C decl_hdf5_group_comp_test_01.c)
target_link_libraries(decl_hdf5_group_comp_01_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_group_comp_01_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_group_comp_01_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_group_comp_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_group_comp_01_C>")

add_executable(decl_hdf5_group_comp_02_C decl_hdf5_group_comp_test_02.c)
target_link_libraries(decl_hdf5_group_comp_02_C PDI::PDI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_group_comp_02_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_group_comp_02_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_group_comp_02_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_group_comp_02_C>")

add_executable(decl_hdf5_mpi_comp_01_C decl_hdf5_mpi_comp_test_01.c)
target_link_libraries(decl_hdf5_mpi_comp_01_C PDI::PDI_C MPI::MPI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_mpi_comp_01_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_mpi_comp_01_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_mpi_comp_01_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_comp_01_C>")

add_executable(decl_hdf5_mpi_comp_02_C decl_hdf5_mpi_comp_test_02.c)
target_link_libraries(decl_hdf5_mpi_comp_02_C PDI::PDI_C MPI::MPI_C ${HDF5_C_LIBRARIES})
target_include_directories(decl_hdf5_mpi_comp_02_C PRIVATE ${HDF5_INCLUDE_DIRS})
target_compile_definitions(decl_hdf5_mpi_comp_02_C PRIVATE ${HDF5_DEFINITIONS})
add_test(NAME decl_hdf5_mpi_comp_02_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_comp_02_C>")
