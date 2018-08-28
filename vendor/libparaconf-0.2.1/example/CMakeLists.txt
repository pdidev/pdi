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

add_executable(PC_example EXCLUDE_FROM_ALL example.c)
target_link_libraries(PC_example paraconf yaml)
set_property(TARGET PC_example PROPERTY C_STANDARD 99)
set_property(TARGET PC_example PROPERTY C_STANDARD_REQUIRED TRUE)
add_test(NAME PC_example_build COMMAND "${CMAKE_COMMAND}" --build . --target PC_example WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
add_test(NAME PC_example COMMAND PC_example WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
set_tests_properties(PC_example PROPERTIES DEPENDS PC_example_build)

if("${ENABLE_FORTRAN}")
	add_executable(PC_example_f90 EXCLUDE_FROM_ALL example.f90)
	target_link_libraries(PC_example_f90 paraconf_f90)
	add_test(NAME PC_example_f90_build COMMAND "${CMAKE_COMMAND}" --build . --target PC_example_f90 WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")
	add_test(NAME PC_example_f90 COMMAND PC_example_f90 WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
	set_tests_properties(PC_example_f90 PROPERTIES DEPENDS PC_example_f90_build)
endif()
