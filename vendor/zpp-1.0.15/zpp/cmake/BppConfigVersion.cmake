################################################################################
# Copyright (c) Julien Bigot - CEA (julien.bigot@cea.fr)
# All rights reserved.
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
################################################################################

# Compute the installation prefix relative to this file.
get_filename_component(_CURRENT_LIST_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
include("${_CURRENT_LIST_DIR}/ZppConfig.cmake")

execute_process(COMMAND "${ZPP_EXECUTABLE}" "--version"
	RESULT_VARIABLE _ZPP_VERSION_RESULT
	OUTPUT_VARIABLE PACKAGE_VERSION
	ERROR_VARIABLE _ZPP_VERSION_ERROR
	OUTPUT_STRIP_TRAILING_WHITESPACE
)
if(NOT "0${_ZPP_VERSION_RESULT}" EQUAL 0)
	message("${ZPP_EXECUTABLE} --version\n${PACKAGE_VERSION}\n${_ZPP_VERSION_ERROR}\n\$?=${_ZPP_VERSION_RESULT}")
endif()

# Check whether the requested PACKAGE_FIND_VERSION is compatible
if("${PACKAGE_VERSION}" VERSION_LESS "${PACKAGE_FIND_VERSION}")
  set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()
  set(PACKAGE_VERSION_COMPATIBLE TRUE)
  if ("${PACKAGE_VERSION}" VERSION_EQUAL "${PACKAGE_FIND_VERSION}")
    set(PACKAGE_VERSION_EXACT TRUE)
  endif()
endif()
