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

add_executable(Utilities_01_C EXCLUDE_FROM_ALL Utilities_test_01.c)
add_compiler_flags(Utilities_01_C PRIVATE -Wall -Wextra)
target_link_libraries(Utilities_01_C pdi mpi)
add_test(NAME Utilities_01_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target Utilities_01_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME Utilities_01_C_run COMMAND Utilities_01_C "${CMAKE_CURRENT_SOURCE_DIR}/Utilities_test_01.yml")
set_property(TEST Utilities_01_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST Utilities_01_C_run PROPERTY DEPENDS Utilities_01_C_build)
set_tests_properties(Utilities_01_C_run PROPERTIES DEPENDS Utilities_01_C_build TIMEOUT 3)

add_executable(Utilities_02_C EXCLUDE_FROM_ALL Utilities_test_02.c)
add_compiler_flags(Utilities_02_C PRIVATE -Wall -Wextra)
target_link_libraries(Utilities_02_C pdi mpi)
add_test(NAME Utilities_02_C_build
		COMMAND "${CMAKE_COMMAND}" --build . --target Utilities_02_C
		WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

add_test(NAME Utilities_02_C_run COMMAND Utilities_02_C "${CMAKE_CURRENT_SOURCE_DIR}/Utilities_test_02.yml")
set_property(TEST Utilities_02_C_run APPEND PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}/../")
set_property(TEST Utilities_02_C_run PROPERTY DEPENDS Utilities_02_C_build)
set_tests_properties(Utilities_02_C_run PROPERTIES DEPENDS Utilities_02_C_build TIMEOUT 3)


