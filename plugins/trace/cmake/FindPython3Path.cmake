################################################################################
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies
# alternatives (CEA)
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

set(_Python3Path_QUIET)
if("${Python3Path_FIND_QUIETLY}")
	set(_Python3Path_QUIET QUIET)
endif()

set(_Python3Path_REQUIRED)
set(_Python3Path_ERROR_LEVEL)
if("${Python3Path_FIND_REQUIRED}")
	set(_Python3Path_REQUIRED REQUIRED)
	set(_Python3Path_ERROR_LEVEL SEND_ERROR)
endif()

set(Python3Path_COMPONENTS_PARAM)
set(Python3Path_OPTIONAL_COMPONENTS)
foreach(_Python3Path_COMPONENT ${Python3Path_FIND_COMPONENTS})
	if("${Python3Path_FIND_REQUIRED_${_Python3Path_COMPONENT}}")
		list(APPEND Python3Path_COMPONENTS_PARAM "${_Python3Path_COMPONENT}")
	else()
		list(APPEND Python3Path_OPTIONAL_COMPONENTS "${_Python3Path_COMPONENT}")
	endif()
endforeach()
if(NOT "xx" STREQUAL "x${Python3Path_COMPONENTS_PARAM}x")
	set(Python3Path_COMPONENTS_PARAM COMPONENTS ${Python3Path_COMPONENTS_PARAM})
endif()
if(NOT "xx" STREQUAL "x${Python3Path_OPTIONAL_COMPONENTS}x")
	list(APPEND Python3Path_COMPONENTS_PARAM OPTIONAL_COMPONENTS ${Python3Path_OPTIONAL_COMPONENTS})
endif()

find_package(Python3 ${Python3Path_FIND_VERSION} ${_Python3Path_QUIET} ${_Python3Path_REQUIRED} ${Python3Path_COMPONENTS_PARAM})

if("${Python3_FOUND}")
	# retrieve various package installation directories
	execute_process (COMMAND "${Python3_EXECUTABLE}" -c "import sys; from distutils import sysconfig;sys.stdout.write(';'.join([sysconfig.get_python_lib(prefix='',plat_specific=False,standard_lib=True),sysconfig.get_python_lib(prefix='',plat_specific=True,standard_lib=True),sysconfig.get_python_lib(prefix='',plat_specific=False,standard_lib=False),sysconfig.get_python_lib(prefix='',plat_specific=True,standard_lib=False)]))"
			RESULT_VARIABLE _Python3Path_RESULT
			OUTPUT_VARIABLE _Python3Path_LIBPATHS
			ERROR_QUIET)
	if (NOT _Python3Path_RESULT)
		list (GET _Python3Path_LIBPATHS 0 Python3Path_INSTALL_STDLIBDIR)
		list (GET _Python3Path_LIBPATHS 1 Python3Path_INSTALL_STDARCHDIR)
		list (GET _Python3Path_LIBPATHS 2 Python3Path_INSTALL_SITELIBDIR)
		list (GET _Python3Path_LIBPATHS 3 Python3Path_INSTALL_SITEARCHDIR)
	else()
		unset (Python3Path_INSTALL_STDLIBDIR)
		unset (Python3Path_INSTALL_STDARCHDIR)
		unset (Python3Path_INSTALL_SITELIBDIR)
		unset (Python3Path_INSTALL_SITEARCHDIR)
	endif()
	unset(_Python3Path_LIBPATHS)
endif()

find_package_handle_standard_args(Python3Path REQUIRED_VARS Python3Path_INSTALL_SITELIBDIR Python3Path_INSTALL_SITEARCHDIR Python3Path_INSTALL_STDLIBDIR Python3Path_INSTALL_STDARCHDIR)
