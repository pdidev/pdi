#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

set(ALL_TEST_NAMES array dataset group struct variables)
if("${BUILD_HDF5_PARALLEL}")
    list(APPEND ALL_TEST_NAMES mpi)
endif()

foreach(test_name )
    foreach(io_operation write read)
        add_executable("HDF5_C_${test_name}_${io_operation}_test" "HDF5_C/${test_name}_${io_operation}_test.c")
        target_link_libraries("HDF5_C_${test_name}_${io_operation}_test" ${HDF5_C_LIBRARIES})
        target_include_directories("HDF5_C_${test_name}_${io_operation}_test" PRIVATE ${HDF5_INCLUDE_DIRS})
        target_compile_definitions("HDF5_C_${test_name}_${io_operation}_test" PRIVATE ${HDF5_DEFINITIONS})

        add_executable("PDI_C_${test_name}_${io_operation}_test" "PDI_C/${test_name}_${io_operation}_test.c")
        target_link_libraries("PDI_C_${test_name}_${io_operation}_test" PDI::PDI_C)
        if ("${test_name}" STREQUAL "mpi")
            target_link_libraries("HDF5_C_${test_name}_${io_operation}_test" MPI::MPI_C)
            target_link_libraries("PDI_C_${test_name}_${io_operation}_test"  MPI::MPI_C)
        endif()
    endforeach(io_operation)

    foreach(write_prefix HDF5_C_ PDI_C_)
        foreach(read_prefix HDF5_C_ PDI_C_)
            set(write_test_name "${write_prefix}${test_name}_write_test")
            set(read_test_name "${read_prefix}${test_name}_read_test")
            if ("${test_name}" STREQUAL "mpi")
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
