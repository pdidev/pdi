#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2022-2026 Centre National de Recherche Scientifique (CNRS)
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
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
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
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_decl_hdf5_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_decl_hdf5_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

add_executable(decl_hdf5_tests decl_hdf5_tests.cxx)
target_link_libraries(decl_hdf5_tests PDI::PDI_C GTest::gtest GTest::gtest_main)
gtest_discover_tests(decl_hdf5_tests)

# compression test
add_executable(decl_hdf5_deflate decl_hdf5_test_deflate.cxx)
target_link_libraries(decl_hdf5_deflate PDI::PDI_C GTest::gtest GTest::gtest_main ${HDF5_DEPS})
gtest_discover_tests(decl_hdf5_deflate)

# PDI_import/PDI_export
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_01_C decl_hdf5_mpi_test_01.c)
	target_link_libraries(decl_hdf5_mpi_01_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_01_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_01_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_mpi_test_01.yml")
	set_property(TEST decl_hdf5_mpi_01_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_01_C PROPERTY PROCESSORS 2)
endif()

# parallel read/write
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_02_C decl_hdf5_mpi_test_02.c)
	target_link_libraries(decl_hdf5_mpi_02_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_02_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_02_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_02_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_02_C PROPERTY PROCESSORS 4)
endif()

# scalars default options
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_03_C decl_hdf5_mpi_test_03.c)
	target_link_libraries(decl_hdf5_mpi_03_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_03_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_03_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_03_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_03_C PROPERTY PROCESSORS 4)
endif()

if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_04_C decl_hdf5_mpi_test_04.c)
	target_link_libraries(decl_hdf5_mpi_04_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_04_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_04_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_04_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_04_C PROPERTY PROCESSORS 4)
endif()

# communicator as a reference
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_05_C decl_hdf5_mpi_test_05.c)
	target_link_libraries(decl_hdf5_mpi_05_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_05_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 8 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_05_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_05_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_05_C PROPERTY PROCESSORS 8)
endif()

# scalars default options
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_06_C decl_hdf5_mpi_test_06.c)
	target_link_libraries(decl_hdf5_mpi_06_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_06_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_06_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_06_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_06_C PROPERTY PROCESSORS 4)
endif()

# mpio dataset attribute
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_07_C decl_hdf5_mpi_test_07.c)
	target_link_libraries(decl_hdf5_mpi_07_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_07_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_07_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_07_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_07_C PROPERTY PROCESSORS 4)
endif()

# case datasets is defined as a regex.
# The data is writing in //. This data is defined with a dataset_selection and a dataset that depends on a regex
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_mpi_08_C decl_hdf5_mpi_test_08.c)
	target_link_libraries(decl_hdf5_mpi_08_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_mpi_08_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_mpi_08_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_mpi_08_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_mpi_08_C PROPERTY PROCESSORS 4)
endif()


add_executable(decl_hdf5_IO_options_C decl_hdf5_test_IO_options.c)
target_link_libraries(decl_hdf5_IO_options_C PDI::PDI_C  ${HDF5_DEPS})
add_test(NAME decl_hdf5_IO_options_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_IO_options_C>")
set_property(TEST decl_hdf5_IO_options_C PROPERTY TIMEOUT 15)

if("${BUILD_FORTRAN}")
	add_subdirectory(fortran/)
endif("${BUILD_FORTRAN}")


add_subdirectory("compatibility_tests/")
