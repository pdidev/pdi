#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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

include(AddCCompilerFlags)

add_executable(PDI_example
	example.c
)
add_compiler_flags(PDI_example PRIVATE -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors -pedantic-errors)
target_link_libraries(PDI_example pdi hdf5_per_process test_plugin mpi)
add_test(NAME PDI_example COMMAND "${MPIEXEC}" -n 2 PDI_example WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

add_executable(PDI_example_f90
	example.f90
)
target_link_libraries(PDI_example_f90 pdi_f90 hdf5_per_process test_plugin mpi_f90)
add_test(NAME PDI_example_f90 COMMAND "${MPIEXEC}" -n 2 PDI_example_f90 WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
