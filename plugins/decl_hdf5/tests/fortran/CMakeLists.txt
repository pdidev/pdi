# SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
enable_language(Fortran)

# Includes
include(CTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../../cmake/runtest-dir")

if("${BUILD_HDF5_PARALLEL}")
    if(NOT "${MPI_Fortran_HAVE_F90_MODULE}")
        if("${MPI_Fortran_HAVE_F77_HEADER}")
            file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90" [=[
    module mpi
    implicit none
    include "mpif.h"
    end module
    ]=])
            add_library(MPI_with_mod STATIC "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90")
            target_link_libraries(MPI_with_mod MPI::MPI_Fortran)
        else()
            message(FATAL_ERROR "Unable to compile a MPI program either with F90 module or F77 include")
        endif()
    else()
        add_library(MPI_with_mod INTERFACE)
        target_link_libraries(MPI_with_mod INTERFACE MPI::MPI_Fortran)
    endif()
endif()

add_executable(decl_hdf5_01_F decl_hdf5_test_01.F90)
target_link_libraries(decl_hdf5_01_F PDI::PDI_f90)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_hdf5_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_hdf5_01_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_01_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_F PROPERTY TIMEOUT 15)

if("${BUILD_HDF5_PARALLEL}")
    add_executable(decl_hdf5_02_F decl_hdf5_test_02.F90)
    target_link_libraries(decl_hdf5_02_F PDI::PDI_f90 MPI_with_mod)
    string(RANDOM TMP_MODULE_DIRECTORY)
    set_target_properties(decl_hdf5_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
    add_test(NAME decl_hdf5_02_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/../decl_hdf5_mpi_test_01.yml")
    set_property(TEST decl_hdf5_02_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_02_F PROPERTY PROCESSORS 2)

    add_test(NAME decl_hdf5_03_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/../decl_hdf5_test_03.yml" "test_03_on")
    set_property(TEST decl_hdf5_03_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_03_F PROPERTY PROCESSORS 2)

    add_executable(decl_hdf5_04_F decl_hdf5_test_04.F90)
    target_link_libraries(decl_hdf5_04_F PDI::PDI_f90 MPI_with_mod)
    string(RANDOM TMP_MODULE_DIRECTORY)
    set_target_properties(decl_hdf5_04_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
    add_test(NAME decl_hdf5_04_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_04_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04.yml")
    set_property(TEST decl_hdf5_04_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_04_F PROPERTY PROCESSORS 4)
endif("${BUILD_HDF5_PARALLEL}")

add_executable(decl_hdf5_05_F decl_hdf5_test_05.F90)
target_link_libraries(decl_hdf5_05_F PDI::PDI_f90)
add_test(NAME decl_hdf5_05_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_05_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_05.yml")
set_property(TEST decl_hdf5_05_F PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_06_F decl_hdf5_test_06.F90)
target_link_libraries(decl_hdf5_06_F PDI::PDI_f90)
add_test(NAME decl_hdf5_06_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_06_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_06.yml")
set_property(TEST decl_hdf5_06_F PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_07_F decl_hdf5_test_07.F90)
target_link_libraries(decl_hdf5_07_F PDI::PDI_f90)
add_test(NAME decl_hdf5_07_F COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_07_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_07.yml")
set_property(TEST decl_hdf5_07_F PROPERTY TIMEOUT 15)
