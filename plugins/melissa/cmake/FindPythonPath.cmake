################################################################################
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)

include(FindPackageHandleStandardArgs)

set(_PythonPath_QUIET)
if("${PythonPath_FIND_QUIETLY}")
	set(_PythonPath_QUIET QUIET)
endif()

set(_PythonPath_REQUIRED)
set(_PythonPath_ERROR_LEVEL)
if("${PythonPath_FIND_REQUIRED}")
	set(_PythonPath_REQUIRED REQUIRED)
	set(_PythonPath_ERROR_LEVEL SEND_ERROR)
endif()

find_package(PythonInterp ${_PythonPath_QUIET} ${_PythonPath_REQUIRED})

if("${PYTHONINTERP_FOUND}")
	if(NOT "${PythonPath_INSTALL_STDLIBDIR}")
		execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
				"from distutils import sysconfig;print(sysconfig.get_python_lib(prefix='',plat_specific=False,standard_lib=True))"
				OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE _PythonPath_PC_SUCCESS
				OUTPUT_VARIABLE PythonPath_INSTALL_STDLIBDIR
				ERROR_VARIABLE  _PythonPath_PC_ERROR)
		if(NOT "${_PythonPath_PC_SUCCESS}" EQUAL 0 AND NOT "${PythonInterp_FIND_QUIETLY}")
			message("${_PythonPath_ERROR_LEVEL}" "Error while locating python STDLIB location:\n${_PythonPath_PC_ERROR}")
		else()
			set(PythonPath_INSTALL_STDLIBDIR "${PythonPath_INSTALL_STDLIBDIR}" CACHE STRING "python STDLIB location" FORCE)
			mark_as_advanced(PythonPath_INSTALL_STDLIBDIR)
		endif()
	endif()
	if(NOT "${PythonPath_INSTALL_STDARCHDIR}")
		execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
				"from distutils import sysconfig;print(sysconfig.get_python_lib(prefix='',plat_specific=True,standard_lib=True))"
				OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE _PythonPath_PC_SUCCESS
				OUTPUT_VARIABLE PythonPath_INSTALL_STDARCHDIR
				ERROR_VARIABLE  _PythonPath_PC_ERROR)
		if(NOT "${_PythonPath_PC_SUCCESS}" EQUAL 0 AND NOT "${PythonInterp_FIND_QUIETLY}")
			message("${_PythonPath_ERROR_LEVEL}" "Error while locating python STDARCH location:\n${_PythonPath_PC_ERROR}")
		else()
			set(PythonPath_INSTALL_STDARCHDIR "${PythonPath_INSTALL_STDARCHDIR}" CACHE STRING "python STDARCH location" FORCE)
			mark_as_advanced(PythonPath_INSTALL_STDARCHDIR)
		endif()
	endif()
	if(NOT "${PythonPath_INSTALL_SITELIBDIR}")
		execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
				"from distutils import sysconfig;print(sysconfig.get_python_lib(prefix='',plat_specific=False,standard_lib=False))"
				OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE _PythonPath_PC_SUCCESS
				OUTPUT_VARIABLE PythonPath_INSTALL_SITELIBDIR
				ERROR_VARIABLE  _PythonPath_PC_ERROR)
		if(NOT "${_PythonPath_PC_SUCCESS}" EQUAL 0 AND NOT "${PythonInterp_FIND_QUIETLY}")
			message("${_PythonPath_ERROR_LEVEL}" "Error while locating python SITELIB location:\n${_PythonPath_PC_ERROR}")
		else()
			set(PythonPath_INSTALL_SITELIBDIR "${PythonPath_INSTALL_SITELIBDIR}" CACHE STRING "python SITELIB location" FORCE)
			mark_as_advanced(PythonPath_INSTALL_SITELIBDIR)
		endif()
	endif()
	if(NOT "${PythonPath_INSTALL_SITEARCHDIR}")
		execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
				"from distutils import sysconfig;print(sysconfig.get_python_lib(prefix='',plat_specific=True,standard_lib=False))"
				OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE _PythonPath_PC_SUCCESS
				OUTPUT_VARIABLE PythonPath_INSTALL_SITEARCHDIR
				ERROR_VARIABLE  _PythonPath_PC_ERROR)
		if(NOT "${_PythonPath_PC_SUCCESS}" EQUAL 0 AND NOT "${PythonInterp_FIND_QUIETLY}")
			message("${_PythonPath_ERROR_LEVEL}" "Error while locating python SITEARCH location:\n${_PythonPath_PC_ERROR}")
		else()
			set(PythonPath_INSTALL_SITEARCHDIR "${PythonPath_INSTALL_SITEARCHDIR}" CACHE STRING "python SITEARCH location" FORCE)
			mark_as_advanced(PythonPath_INSTALL_SITEARCHDIR)
		endif()
	endif()
endif()

find_package_handle_standard_args(PythonPath REQUIRED_VARS PythonPath_INSTALL_STDLIBDIR PythonPath_INSTALL_STDARCHDIR PythonPath_INSTALL_SITELIBDIR PythonPath_INSTALL_SITEARCHDIR)
