#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 National Institute for Research in Digital Science and Technology (Inria)
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

cmake_minimum_required(VERSION 3.22...4.2)

if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

##
set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_damaris_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_damaris_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

## first test: write an integer array with damaris and with decl_hdf5 and compare the result
## The size of the array change between the two iterations

add_executable(damaris_write_int_array damaris_write_int_array.cxx)
target_link_libraries(damaris_write_int_array  PDI::PDI_C MPI::MPI_C)
target_compile_features(damaris_write_int_array  PUBLIC cxx_std_17)

add_executable(damaris_read_int_array damaris_read_int_array.cxx)
target_link_libraries(damaris_read_int_array PDI::PDI_C)
target_compile_features(damaris_read_int_array PUBLIC cxx_std_17)

add_test(NAME test_damaris_depends_on
     COMMAND "${RUNTEST_DIR}" "/bin/bash" "-exc"
     "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} $<TARGET_FILE:damaris_write_int_array> ${MPIEXEC_POSTFLAGS} && $<TARGET_FILE:damaris_read_int_array>")
set_property(TEST test_damaris_depends_on PROPERTY TIMEOUT 8)

add_executable(damaris_write_dataset damaris_write_dataset.cxx)
target_link_libraries(damaris_write_dataset PDI::PDI_C MPI::MPI_C)
add_executable(damaris_read_dataset damaris_read_dataset.cxx)
target_link_libraries(damaris_read_dataset PDI::PDI_C MPI::MPI_C)

add_test(NAME test_damaris_dataset_name
     COMMAND "${RUNTEST_DIR}" "/bin/bash" "-exc"
     "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} $<TARGET_FILE:damaris_write_dataset> ${MPIEXEC_POSTFLAGS} && $<TARGET_FILE:damaris_read_dataset>")

configure_file(test_write_1_file_2_data.yml test_write_1_file_2_data.yml COPYONLY)
configure_file(test_write_2_file_1_data.yml test_write_2_file_1_data.yml COPYONLY)

add_executable(damaris_write_multiple_data  test_write_multiple_data.cxx)
target_link_libraries(damaris_write_multiple_data PDI::PDI_C MPI::MPI_C)
target_compile_features(damaris_write_multiple_data  PUBLIC cxx_std_17)

add_executable(damaris_read_multiple_data test_read_multiple_data.cxx)
target_compile_features(damaris_read_multiple_data PUBLIC cxx_std_17)
target_link_libraries(damaris_read_multiple_data PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest)
gtest_discover_tests(damaris_read_multiple_data PROPERTIES TIMEOUT 3)

# generic executable to test damaris
add_executable(test_write_simple_array_int test_write_simple_array_int.c)
target_link_libraries(test_write_simple_array_int PDI::PDI_C MPI::MPI_C)

# yaml file using with the previous test
configure_file(test_write_multi_process_collective.yml test_write_multi_process_collective.yml COPYONLY)
configure_file(test_write_multi_process_file_per_core.yml test_write_multi_process_file_per_core.yml COPYONLY)
configure_file(test_write_multi_process_collective_2_servers.yml test_write_multi_process_collective_2_servers.yml COPYONLY)
configure_file(test_write_metadata.yml test_write_metadata.yml COPYONLY)

add_executable(test_read_simple_array_int test_read_simple_array_int.cxx)
target_link_libraries(test_read_simple_array_int PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest GTest::gtest_main)
gtest_discover_tests(test_read_simple_array_int PROPERTIES TIMEOUT 3)

#################################
# multidimensinal data
configure_file(test_write_2D_data.yml test_write_2D_data.yml COPYONLY)
configure_file(test_write_3D_data.yml test_write_3D_data.yml COPYONLY)

add_executable(damaris_write_2D_data  test_write_2D_data.cxx)
target_link_libraries(damaris_write_2D_data PDI::PDI_C MPI::MPI_CXX)
target_compile_features(damaris_write_2D_data  PUBLIC cxx_std_17)

add_executable(damaris_write_3D_data  test_write_3D_data.cxx)
target_link_libraries(damaris_write_3D_data PDI::PDI_C MPI::MPI_CXX)
target_compile_features(damaris_write_3D_data  PUBLIC cxx_std_17)

add_executable(damaris_read_multidimensional_data test_read_multidimensional_data.cxx)
target_compile_features(damaris_read_multidimensional_data PUBLIC cxx_std_17)
target_link_libraries(damaris_read_multidimensional_data PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest)
gtest_discover_tests(damaris_read_multidimensional_data PROPERTIES TIMEOUT 3)
# end multidimensional data
##################################
#########################
# test for when condition
configure_file(test_damaris_when_condition.yml test_damaris_when_condition.yml COPYONLY)

add_executable(damaris_write_when_condition  test_write_damaris_when_condition.cxx)
target_link_libraries(damaris_write_when_condition PDI::PDI_C MPI::MPI_CXX)
target_compile_features(damaris_write_when_condition  PUBLIC cxx_std_17)

add_executable(damaris_read_when_condition  test_read_damaris_when_condition.cxx)
target_link_libraries(damaris_read_when_condition PDI::PDI_C  GTest::gmock GTest::gmock_main GTest::gtest)
target_compile_features(damaris_read_when_condition  PUBLIC cxx_std_17)

gtest_discover_tests(damaris_read_when_condition PROPERTY TIMEOUT 4)
# end test for when condition
#############################

#########################
# tests for the Damaris `pyscript` (Python in situ analytics)
# They need Damaris built with Python support.
file(STRINGS "${Damaris_INCLUDE_DIRS}/Damaris.h" DAMARIS_HAVE_PYTHON REGEX "^#define HAVE_PYTHON_ENABLED")
if(DAMARIS_HAVE_PYTHON)
	add_executable(test_write_pyscript test_write_pyscript.c)
	target_link_libraries(test_write_pyscript PDI::PDI_C MPI::MPI_C)

	foreach(PYSCRIPT_TEST_FILE
	        test_pyscript.py
	        test_damaris_pyscript.yml
	        test_damaris_pyscript_unknown_key.yml
	        test_damaris_pyscript_missing_file.yml)
		configure_file(${PYSCRIPT_TEST_FILE} ${PYSCRIPT_TEST_FILE} COPYONLY)
	endforeach()

	add_executable(test_read_pyscript test_read_pyscript.cxx)
	target_compile_features(test_read_pyscript PUBLIC cxx_std_20)
	target_link_libraries(test_read_pyscript PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest)
	gtest_discover_tests(test_read_pyscript PROPERTIES TIMEOUT 60)

	# Python + Dask distributed: needs a Dask scheduler executable, and the Python run by the
	# Damaris servers must import `dask.distributed' and `damaris4py' (Damaris Python module).
	option(DAMARIS_PLUGIN_TEST_DASK "Test the Damaris pyscript with Dask distributed" OFF)
	if(DAMARIS_PLUGIN_TEST_DASK)
		find_program(DAMARIS_DASK_SCHEDULER_EXECUTABLE dask-scheduler REQUIRED)
		# Damaris starts the workers with `dask worker', taken from PATH: use the scheduler's one
		get_filename_component(DAMARIS_DASK_BIN_DIR "${DAMARIS_DASK_SCHEDULER_EXECUTABLE}" DIRECTORY)
		set(DAMARIS_DASK_PYTHONPATH "" CACHE STRING
		    "Extra PYTHONPATH for the Python run by the Damaris servers (e.g. damaris4py and dask site-packages)")

		foreach(PYSCRIPT_TEST_FILE test_pyscript_dask.py test_damaris_pyscript_dask.yml)
			configure_file(${PYSCRIPT_TEST_FILE} ${PYSCRIPT_TEST_FILE} COPYONLY)
		endforeach()

		add_executable(test_read_pyscript_dask test_read_pyscript_dask.cxx)
		target_compile_features(test_read_pyscript_dask PUBLIC cxx_std_20)
		string(REPLACE ";" ":" DAMARIS_DASK_PYTHONPATH_STR "${DAMARIS_DASK_PYTHONPATH}")
		target_compile_definitions(test_read_pyscript_dask PRIVATE
		                           DASK_SCHEDULER_EXECUTABLE="${DAMARIS_DASK_SCHEDULER_EXECUTABLE}"
		                           DASK_BIN_DIR="${DAMARIS_DASK_BIN_DIR}"
		                           DASK_PYTHONPATH="${DAMARIS_DASK_PYTHONPATH_STR}")
		target_link_libraries(test_read_pyscript_dask PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest)
		gtest_discover_tests(test_read_pyscript_dask PROPERTIES TIMEOUT 120)
	endif()
else()
	message(STATUS "Damaris built without Python support: pyscript tests disabled")
endif()
# end tests for the Damaris `pyscript`
#########################
