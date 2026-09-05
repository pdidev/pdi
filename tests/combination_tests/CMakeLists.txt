#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_tests LANGUAGES C CXX)

find_package(Threads REQUIRED)
if(NOT TARGET GTest::gtest)
	option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
	add_subdirectory("../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
include(GoogleTest)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

option(BUILD_DECL_HDF5_PLUGIN "Build Decl'HDF5 plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN "Build Serialize plug-in" ON)

include(CTest)

find_package(paraconf REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

add_executable(integration_tests include.cxx)
target_link_libraries(integration_tests PDI::PDI_C GTest::gmock GTest::gmock_main GTest::gtest GTest::gtest_main)
gtest_discover_tests(integration_tests)

if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_01_C test_01.c)
target_link_libraries(test_01_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_01_C COMMAND "$<TARGET_FILE:test_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_01.yml")

add_executable(test_02_C test_02.c)
target_link_libraries(test_02_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_02_C COMMAND "$<TARGET_FILE:test_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_02.yml")

add_executable(test_03_C test_03.c)
target_link_libraries(test_03_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_03_C COMMAND "$<TARGET_FILE:test_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_03.yml")

add_executable(test_04_C test_04.c)
target_link_libraries(test_04_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_04_C COMMAND "$<TARGET_FILE:test_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_04.yml")

endif("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")
if("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")

add_executable(test_05_C test_05.c)
target_link_libraries(test_05_C paraconf::paraconf PDI::PDI_C)
add_test(NAME test_05_C COMMAND "$<TARGET_FILE:test_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/test_05.yml")
#TODO: NetCDF error https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/issues/433
set_tests_properties(test_05_C PROPERTIES DISABLED TRUE)

endif("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_SERIALIZE_PLUGIN}")


# The First steps examples of the documentation.
# They are built and run here rather than beside the documentation because they
# load the Trace plugin, which is only available once the plugins have been
# built.
# Each of them reads its specification tree from its own directory.

option(BUILD_TRACE_PLUGIN "Build Trace plugin" ON)

if("${BUILD_TRACE_PLUGIN}")

set(FIRST_STEPS_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../../pdi/docs/First_steps")

foreach(FIRST_STEPS_EXAMPLE IN ITEMS hello_data hello_event hello_expose hello_multi_expose)
	add_executable(First_steps_${FIRST_STEPS_EXAMPLE}_C "${FIRST_STEPS_DIR}/${FIRST_STEPS_EXAMPLE}.c")
	target_link_libraries(First_steps_${FIRST_STEPS_EXAMPLE}_C paraconf::paraconf PDI::PDI_C)
	if(EXISTS "${FIRST_STEPS_DIR}/${FIRST_STEPS_EXAMPLE}.output")
		# the documentation shows the output of this one, check it did not go stale
		add_test(NAME First_steps_${FIRST_STEPS_EXAMPLE}_C
				COMMAND "${CMAKE_COMMAND}"
					"-DEXAMPLE=$<TARGET_FILE:First_steps_${FIRST_STEPS_EXAMPLE}_C>"
					"-DEXPECTED=${FIRST_STEPS_DIR}/${FIRST_STEPS_EXAMPLE}.output"
					"-DWORKDIR=${FIRST_STEPS_DIR}"
					-P "${FIRST_STEPS_DIR}/../run_and_compare.cmake")
	else()
		add_test(NAME First_steps_${FIRST_STEPS_EXAMPLE}_C
				COMMAND "$<TARGET_FILE:First_steps_${FIRST_STEPS_EXAMPLE}_C>"
				WORKING_DIRECTORY "${FIRST_STEPS_DIR}")
	endif()
endforeach()

endif("${BUILD_TRACE_PLUGIN}")

# The logging examples of the specification tree reference: their pattern names
# the rank of the MPI communicator, that only the MPI plugin provides.
find_package(MPI REQUIRED COMPONENTS C)
add_executable(doc_logging doc_logging.cxx)
target_link_libraries(doc_logging MPI::MPI_C PDI::PDI_C GTest::gtest GTest::gmock)
add_test(NAME doc_logging COMMAND "$<TARGET_FILE:doc_logging>")
