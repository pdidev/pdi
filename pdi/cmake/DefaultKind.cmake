################################################################################
# Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#     * Neither the name of the <organization> nor the
#     names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

cmake_minimum_required(VERSION 3.22...3.28)

list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}")

function(get_default_kind TYPE DEFAULT_KIND_VAR)
	if(DEFINED "${DEFAULT_KIND_VAR}")
		return()
	endif()
	message(STATUS "Checking default Fortran kind for ${TYPE}")
	set(TEST_FILE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_test_kind${TYPE}.f90")
	file(WRITE "${TEST_FILE}" "
program test_kind${TYPE}
  ${TYPE} :: def_knd_var
  print *, 'KINDOF ', kind(def_knd_var), ' ENDKINDOF'
end program test_kind${TYPE}
")
	try_run(RUN_RES CMP_RES "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${TEST_FILE}"
			COMPILE_OUTPUT_VARIABLE COMPILE_OUTPUT
			RUN_OUTPUT_VARIABLE RUN_OUTPUT
	)
	if("${CMP_RES}" AND "${RUN_RES}" EQUAL 0)
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
				"Kind detection program for ${TYPE} successfully compiled and run:\n"
				"   ===> Compilation\n"
				"${COMPILE_OUTPUT}\n"
				"   ===> Execution\n"
				"${RUN_OUTPUT}\n\n"
		)
		string(REGEX MATCH "KINDOF *[0-9]* *ENDKINDOF" "${DEFAULT_KIND_VAR}" "${RUN_OUTPUT}")
		string(REGEX MATCH "[0-9]+" "${DEFAULT_KIND_VAR}" "${${DEFAULT_KIND_VAR}}")
	else()
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
				"Kind detection program for ${TYPE} failed to compile or run:\n"
				"   ===> Compilation (${CMP_RES})\n"
				"${COMPILE_OUTPUT}\n"
				"   ===> Execution (${RUN_RES})\n"
				"${RUN_OUTPUT}\n\n"
		)
		message(FATAL_ERROR "Checking default Fortran kind -- Failed")
	endif()
	set("${DEFAULT_KIND_VAR}" "${${DEFAULT_KIND_VAR}}" CACHE STRING "Default Fortran kind for ${TYPE} variables")
	mark_as_advanced("${DEFAULT_KIND_VAR}")
	message(STATUS "Checking default Fortran kind for ${TYPE} -- ${${DEFAULT_KIND_VAR}}")
endfunction()
