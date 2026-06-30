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

cmake_minimum_required(VERSION 3.16...3.29)

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

# copy yaml file, to binary folder, for example_damaris_plugin_with_is_client.c
configure_file(test_damaris_scalar_type.yml test_damaris_scalar_type.yml COPYONLY)
configure_file(run_launch.sh.in run_launch.sh)
#configure_file(run_launch_mpi.sh.in run_launch_mpi.sh)

add_executable(test_damaris_scalar_type test_damaris_scalar_type.c)
add_executable(read_damaris_scalar_type read_damaris_scalar_type.c)
target_link_libraries(test_damaris_scalar_type PDI::PDI_C MPI::MPI_C)
target_link_libraries(read_damaris_scalar_type PDI::PDI_C)

# option 1:
add_test(NAME test_damaris_scalar_type_one_cmd
     COMMAND "${RUNTEST_DIR}"  "/bin/bash" "-exc" "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} $<TARGET_FILE:test_damaris_scalar_type> ${MPIEXEC_POSTFLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/test_damaris_scalar_type.yml && $<TARGET_FILE:read_damaris_scalar_type>" )
set_property(TEST test_damaris_scalar_type_one_cmd PROPERTY TIMEOUT 10)


# option 2:
set(YAML_FILE1 ${CMAKE_CURRENT_SOURCE_DIR}/test_damaris_scalar_type.yml)
set(NB_PROC1 2)
set(BASH_SCRIPT1 ${CMAKE_CURRENT_BINARY_DIR}/run_launch.sh)
set(COMBINED_SCRIPT "${BASH_SCRIPT1} ${NB_PROC1} $<TARGET_FILE:test_damaris_scalar_type> ${YAML_FILE1} && $<TARGET_FILE:read_damaris_scalar_type>")
add_test(NAME test_damaris_scalar_type_bash
     COMMAND "${RUNTEST_DIR}"  "/bin/bash" "-exc" "${COMBINED_SCRIPT}" )

set_property(TEST test_damaris_scalar_type_bash PROPERTY TIMEOUT 10)