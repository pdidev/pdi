#=============================================================================
# Copyright (C) 2021-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.22...4.2)

add_executable(decl_hdf5_01_F decl_hdf5_test_01.F90)
target_link_libraries(decl_hdf5_01_F PDI::PDI_f90)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_hdf5_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_hdf5_01_F COMMAND "$<TARGET_FILE:runtest_dir>" "$<TARGET_FILE:decl_hdf5_01_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_01.yml")
set_property(TEST decl_hdf5_01_F PROPERTY TIMEOUT 15)

if("${BUILD_HDF5_PARALLEL}")
    add_executable(decl_hdf5_02_F decl_hdf5_test_02.F90)
    target_link_libraries(decl_hdf5_02_F PDI::PDI_f90 MPI_with_mod)
    string(RANDOM TMP_MODULE_DIRECTORY)
    set_target_properties(decl_hdf5_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
    add_test(NAME decl_hdf5_02_F COMMAND "$<TARGET_FILE:runtest_dir>" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/../decl_hdf5_mpi_test_01.yml")
    set_property(TEST decl_hdf5_02_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_02_F PROPERTY PROCESSORS 2)

    add_test(NAME decl_hdf5_03_F COMMAND "$<TARGET_FILE:runtest_dir>" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/../decl_hdf5_test_03.yml" "test_03_on")
    set_property(TEST decl_hdf5_03_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_03_F PROPERTY PROCESSORS 2)

    add_executable(decl_hdf5_04_F decl_hdf5_test_04.F90)
    target_link_libraries(decl_hdf5_04_F PDI::PDI_f90 MPI_with_mod)
    string(RANDOM TMP_MODULE_DIRECTORY)
    set_target_properties(decl_hdf5_04_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
    add_test(NAME decl_hdf5_04_F COMMAND "$<TARGET_FILE:runtest_dir>" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_04_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_04.yml")
    set_property(TEST decl_hdf5_04_F PROPERTY TIMEOUT 15)
    set_property(TEST decl_hdf5_04_F PROPERTY PROCESSORS 4)
endif("${BUILD_HDF5_PARALLEL}")

add_executable(decl_hdf5_05_F decl_hdf5_test_05.F90)
target_link_libraries(decl_hdf5_05_F PDI::PDI_f90)
add_test(NAME decl_hdf5_05_F COMMAND "$<TARGET_FILE:runtest_dir>" "$<TARGET_FILE:decl_hdf5_05_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_05.yml")
set_property(TEST decl_hdf5_05_F PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_06_F decl_hdf5_test_06.F90)
target_link_libraries(decl_hdf5_06_F PDI::PDI_f90)
add_test(NAME decl_hdf5_06_F COMMAND "$<TARGET_FILE:runtest_dir>" "$<TARGET_FILE:decl_hdf5_06_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_06.yml")
set_property(TEST decl_hdf5_06_F PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_07_F decl_hdf5_test_07.F90)
target_link_libraries(decl_hdf5_07_F PDI::PDI_f90)
add_test(NAME decl_hdf5_07_F COMMAND "$<TARGET_FILE:runtest_dir>" "$<TARGET_FILE:decl_hdf5_07_F>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_07.yml")
set_property(TEST decl_hdf5_07_F PROPERTY TIMEOUT 15)
