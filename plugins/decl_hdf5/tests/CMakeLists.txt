#=============================================================================
# Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.5)

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

# Metatadata export using filename

add_executable(decl_hdf5_01_C decl_hdf5_test_01.c)
target_link_libraries(decl_hdf5_01_C PDI::PDI_C)
add_test(NAME decl_hdf5_01_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_01_C>")
set_property(TEST decl_hdf5_01_C PROPERTY TIMEOUT 15)

# PDI_import/PDI_export
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_02_C decl_hdf5_test_02.c)
	target_link_libraries(decl_hdf5_02_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_02_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_02_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_02.yml")
	set_property(TEST decl_hdf5_02_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_02_C PROPERTY PROCESSORS 2)
endif()
# slicing mechanism

# parallel read/write

if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_04_C decl_hdf5_test_04.c)
	target_link_libraries(decl_hdf5_04_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_04_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_04_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_04_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_04_C PROPERTY PROCESSORS 4)
endif()
# scalars default options

if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_05_C decl_hdf5_test_05.c)
	target_link_libraries(decl_hdf5_05_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_05_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_05_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_05_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_05_C PROPERTY PROCESSORS 4)
endif()
# 
if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_06_C decl_hdf5_test_06.c)
	target_link_libraries(decl_hdf5_06_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_06_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 4 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_06_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_06_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_06_C PROPERTY PROCESSORS 4)
endif()
# bcommunicator as a reference

if("${BUILD_HDF5_PARALLEL}")
	add_executable(decl_hdf5_07_C decl_hdf5_test_07.c)
	target_link_libraries(decl_hdf5_07_C PDI::PDI_C MPI::MPI_C)
	add_test(NAME decl_hdf5_07_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 8 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_hdf5_07_C>" ${MPIEXEC_POSTFLAGS})
	set_property(TEST decl_hdf5_07_C PROPERTY TIMEOUT 15)
	set_property(TEST decl_hdf5_07_C PROPERTY PROCESSORS 8)
endif()

add_executable(decl_hdf5_08_C decl_hdf5_test_08.c)
target_link_libraries(decl_hdf5_08_C PDI::PDI_C)
add_test(NAME decl_hdf5_08_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_08_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_08.yml")
set_property(TEST decl_hdf5_08_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_09_C decl_hdf5_test_09.c)
target_link_libraries(decl_hdf5_09_C PDI::PDI_C)
add_test(NAME decl_hdf5_09_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_09_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_09.yml")
set_property(TEST decl_hdf5_09_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_11_C decl_hdf5_test_11.c)
target_link_libraries(decl_hdf5_11_C PDI::PDI_C)
add_test(NAME decl_hdf5_11_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_11_C>")
set_property(TEST decl_hdf5_11_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_12_C decl_hdf5_test_12.c)
target_link_libraries(decl_hdf5_12_C PDI::PDI_C)
add_test(NAME decl_hdf5_12_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_12_C>")
set_property(TEST decl_hdf5_12_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_13_C decl_hdf5_test_13.c)
target_link_libraries(decl_hdf5_13_C PDI::PDI_C)
add_test(NAME decl_hdf5_13_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_13_C>")
set_property(TEST decl_hdf5_13_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_14_C decl_hdf5_test_14.c)
target_link_libraries(decl_hdf5_14_C PDI::PDI_C)
add_test(NAME decl_hdf5_14_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_14_C>" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5_test_14.yml")
set_property(TEST decl_hdf5_14_C PROPERTY TIMEOUT 15)

add_executable(decl_hdf5_15_C decl_hdf5_test_15.c)
target_link_libraries(decl_hdf5_15_C PDI::PDI_C)
add_test(NAME decl_hdf5_15_C COMMAND "${RUNTEST_DIR}" "$<TARGET_FILE:decl_hdf5_15_C>")
set_property(TEST decl_hdf5_15_C PROPERTY TIMEOUT 15)

if("${BUILD_FORTRAN}")
	add_subdirectory(fortran/)
endif("${BUILD_FORTRAN}")


# Config tests

if("${BUILD_CFG_VALIDATOR}")
find_package(PDIcfg REQUIRED)

# Positive tests
add_test(NAME pos_val_decl_hdf5_test_01 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_01.yml")
add_test(NAME pos_val_decl_hdf5_test_02 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_02.yml")
add_test(NAME pos_val_decl_hdf5_test_03 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_03.yml")
add_test(NAME pos_val_decl_hdf5_test_04 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_04.yml")
add_test(NAME pos_val_decl_hdf5_test_05 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_05.yml")
add_test(NAME pos_val_decl_hdf5_test_06 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_06.yml")
add_test(NAME pos_val_decl_hdf5_test_07 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_07.yml")
add_test(NAME pos_val_decl_hdf5_test_08 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_hdf5_test_08.yml")

# Negative config tests
add_test(NAME neg_val_decl_hdf5_test_01 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_01.yml")
add_test(NAME neg_val_decl_hdf5_test_02 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_02.yml")
add_test(NAME neg_val_decl_hdf5_test_03 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_03.yml")
add_test(NAME neg_val_decl_hdf5_test_04 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_04.yml")
add_test(NAME neg_val_decl_hdf5_test_05 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_05.yml")
add_test(NAME neg_val_decl_hdf5_test_06 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_06.yml")
add_test(NAME neg_val_decl_hdf5_test_07 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_07.yml")
add_test(NAME neg_val_decl_hdf5_test_08 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_hdf5_test_08.yml")
set_tests_properties( 
	neg_val_decl_hdf5_test_01
	neg_val_decl_hdf5_test_02
	neg_val_decl_hdf5_test_03
	neg_val_decl_hdf5_test_04
	neg_val_decl_hdf5_test_05
	neg_val_decl_hdf5_test_06
	neg_val_decl_hdf5_test_07
	neg_val_decl_hdf5_test_08
	PROPERTIES
	WILL_FAIL TRUE)

endif()

add_subdirectory("compatibility_tests/")
