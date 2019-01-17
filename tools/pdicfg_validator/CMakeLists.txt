#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)
project(pdicfg_validator LANGUAGES)


# Includes

include(GNUInstallDirs)


# Dependencies

set(Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.4)
find_package(PythonInterp REQUIRED) #< Same as pybind11 to get a coherent version of python with the rest of PDI


# Libraries

## Path detection

execute_process(COMMAND "${PYTHON_EXECUTABLE}" "-c"
		"from distutils import sysconfig;print(sysconfig.get_python_lib(prefix='',plat_specific=True))"
		OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_STRIP_TRAILING_WHITESPACE
		RESULT_VARIABLE PDP_SUCCESS
		OUTPUT_VARIABLE INSTALL_PYDSTPKGDIR
		ERROR_VARIABLE  PDP_ERROR)
if(NOT "${PDP_SUCCESS}" EQUAL 0)
	message(FATAL_ERROR "Error while locating python DIST_PACKAGE location:\n${PDP_ERROR}")
endif()


## Actual installation

install(DIRECTORY pdicfg_validator
		DESTINATION "${INSTALL_PYDSTPKGDIR}/"
)

install(PROGRAMS pdicfg_validate
		DESTINATION "${CMAKE_INSTALL_BINDIR}/"
)


# Tests

if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
