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

cmake_minimum_required(VERSION 2.8)
cmake_policy(PUSH)

# Compute the installation prefix relative to this file.
get_filename_component(_CURRENT_LIST_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)

# Hook for python to define the real relative path
#@PYTHON_INSERT_ZPP_EXECUTABLE@

# Compute the installation prefix relative to this file.
if(NOT DEFINED "ZPP_EXECUTABLE")
	get_filename_component(_ZPP_BIN_DIR "${_CURRENT_LIST_DIR}" PATH)
	get_filename_component(_ZPP_BIN_DIR "${_ZPP_BIN_DIR}" PATH)
	get_filename_component(_ZPP_BIN_DIR "${_ZPP_BIN_DIR}" PATH)
	if(_ZPP_BIN_DIR STREQUAL "/")
		set(_ZPP_BIN_DIR "")
	endif()
	set(ZPP_EXECUTABLE "${_ZPP_BIN_DIR}/bin/zpp")
endif()

# For compatibility
set(BPP_EXECUTABLE "${ZPP_EXECUTABLE}")

include("${_CURRENT_LIST_DIR}/Zpp.cmake")

cmake_policy(POP)
