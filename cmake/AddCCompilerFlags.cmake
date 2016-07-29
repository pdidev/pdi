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

cmake_minimum_required(VERSION 3.0)

include(CheckCCompilerFlag)

 function(testff_arr_cloc RESULT_VAR)
 	if(DEFINED "${RESULT_VAR}")
 		return()
 	endif()
 	message(STATUS "Checking whether Fortran supports the C_loc call on assumed shape array elements")
 	set(TEST_FILE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_test_${RESULT_VAR}.f90")
 	file(WRITE "${TEST_FILE}" "
 program test${RESULT_VAR}

 use, intrinsic :: ISO_C_binding
 implicit none

 interface
   subroutine free(v) bind(C)
    use, intrinsic :: ISO_C_binding
    type(C_ptr), value :: v
   endsubroutine
 endinterface

 real(C_float), dimension(:,:), pointer :: v

 call free(c_loc(v))

 endprogram
 ")
 	try_compile(COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${TEST_FILE}"
 		OUTPUT_VARIABLE COMPILE_OUTPUT
 	)
 	if(COMPILE_RESULT)
 		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
 		"${TYPE}(KIND=${KIND}) type successfully compiled with the following output:\n"
 		"${COMPILE_OUTPUT}\n")
 		message(STATUS "Checking whether Fortran supports the C_loc call on assumed shape array elements -- yes")
 	else()
 		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
 		"${TYPE}(KIND=${KIND}) type failed to compile with the following output:\n"
 		"${COMPILE_OUTPUT}\n")
 		message(STATUS "Checking whether Fortran supports the C_loc call on assumed shape array elements -- no")
 	endif()
 	set("${RESULT_VAR}" "${COMPILE_RESULT}" CACHE BOOL "Whether Fortran supports the C_loc call on assumed shape array elements")
 	mark_as_advanced("${RESULT_VAR}")
 endfunction()


function(add_compiler_flags TARGET VISIBILITY FLAGS)
	foreach(FLAG "${FLAGS}" ${ARGN})
		set(FLAG_WORKS)
		check_c_compiler_flag("${FLAG}" FLAG_WORKS)
		if("${FLAG_WORKS}")
			target_compile_options("${TARGET}" "${VISIBILITY}" "${FLAG}")
		endif()
	endforeach()
	set(CLOC_WORKS)
	testff_arr_cloc(CLOC_WORKS)
	if(CLOC_WORKS)
		target_compile_options("${TARGET}" "${VISIBILITY}" "-d CLOC_SUPPORTED")
	endif()
endfunction()
