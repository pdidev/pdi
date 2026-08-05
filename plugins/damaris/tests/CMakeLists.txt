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
gtest_add_tests(TARGET damaris_read_multiple_data TEST_LIST GDamaris_multidata)

set_tests_properties(${GDamaris_multidata} PROPERTIES TIMEOUT 8)
