##############################################################################
# Copyright (c) Julien Bigot - CEA (julien.bigot@cea.fr)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
##############################################################################

cmake_minimum_required(VERSION 3.5)

set(Python_ADDITIONAL_VERSIONS 3 2)
find_package(PythonInterp REQUIRED)

# if we are added by add_subdirectory(), behave as with find_package
if(NOT "${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
	configure_file(bin/zpp.in zpp @ONLY)
	set(ZPP_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/zpp" PARENT_SCOPE)
	include("${CMAKE_CURRENT_SOURCE_DIR}/zpp/cmake/Zpp.cmake")
	return()
endif()

# Behave as a wrapper for the setuptools installer

project(Zpp NONE)

message(WARNING "The cmake build-system is a simple wrapper for the setup.py, it will be removed in future versions, please switch to a direct use of setup.py")

file(COPY
	AUTHORS
	zpp
	LICENSE.txt
	MANIFEST.in
	README.md
	setup.py
	DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/pypkg"
	USE_SOURCE_PERMISSIONS)

install(CODE "execute_process(
		COMMAND [===[${PYTHON_EXECUTABLE}]===] setup.py install --prefix [===[${CMAKE_STAGING_PREFIX}]===]
		WORKING_DIRECTORY [===[${CMAKE_CURRENT_BINARY_DIR}/pypkg]===])")
