# SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../../cmake/runtest-dir")

set(ALL_TEST_NAMES array dataset group struct variables)
if("${BUILD_HDF5_PARALLEL}")
    list(APPEND ALL_TEST_NAMES mpi mpi_independent)
endif()

foreach(test_name ${ALL_TEST_NAMES})
    foreach(io_operation write read)
        add_executable("HDF5_C_${test_name}_${io_operation}_test" "HDF5_C/${test_name}_${io_operation}_test.c")
        target_link_libraries("HDF5_C_${test_name}_${io_operation}_test" ${HDF5_DEPS})

        add_executable("PDI_C_${test_name}_${io_operation}_test" "PDI_C/${test_name}_${io_operation}_test.c")
        target_link_libraries("PDI_C_${test_name}_${io_operation}_test" PDI::PDI_C)
        if ( ("${test_name}" STREQUAL "mpi") OR ("${test_name}" STREQUAL "mpi_independent") )
            target_link_libraries("HDF5_C_${test_name}_${io_operation}_test" MPI::MPI_C)
            target_link_libraries("PDI_C_${test_name}_${io_operation}_test"  MPI::MPI_C)
        endif()
    endforeach(io_operation)

    foreach(write_prefix HDF5_C_ PDI_C_)
        foreach(read_prefix HDF5_C_ PDI_C_)
            set(write_test_name "${write_prefix}${test_name}_write_test")
            set(read_test_name "${read_prefix}${test_name}_read_test")
            if ( ("${test_name}" STREQUAL "mpi") OR ("${test_name}" STREQUAL "mpi_independent") )
                set(MPI_CASE_CMD "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS}")
                set(MPI_POST_CMD " ${MPIEXEC_POSTFLAGS}")
            else()
                set(MPI_CASE_CMD)
                set(MPI_POST_CMD)
            endif()
            add_test(NAME "${write_test_name}__${read_test_name}" COMMAND "${RUNTEST_DIR}" "/bin/bash" "-exc" "${MPI_CASE_CMD}$<TARGET_FILE:${write_test_name}>${MPI_POST_CMD} && ${MPI_CASE_CMD}$<TARGET_FILE:${read_test_name}>${MPI_POST_CMD}")
        endforeach(read_prefix)
    endforeach(write_prefix)
endforeach(test_name)
