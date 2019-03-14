##############################################################################
# Copyright (c) 2013-2014, Julien Bigot - CEA (julien.bigot@cea.fr)
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

cmake_minimum_required(VERSION 2.8)
# protect the project call to workaround a cmake bug
if("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
	project(Bpp Fortran)
endif()

if(POLICY CMP0054)
	cmake_policy(SET CMP0054 NEW)
endif()

include(cmake/Bpp.cmake)

file(READ VERSION BPP_VERSION)
string(REGEX REPLACE "\n.*" "" BPP_VERSION "${BPP_VERSION}")


# The executable

configure_file(scripts/bpp "${CMAKE_CURRENT_BINARY_DIR}/bpp" @ONLY)


# Installation

configure_file(cmake/BppConfig.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/BppConfig.cmake" @ONLY)
configure_file(cmake/BppConfigVersion.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/BppConfigVersion.cmake" @ONLY)
configure_file(scripts/bpp.mk.in "${CMAKE_CURRENT_BINARY_DIR}/bpp.mk" @ONLY)

install(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/bpp
	DESTINATION bin
)
install(DIRECTORY include/
	DESTINATION share/bpp/include/
	FILES_MATCHING PATTERN "*.bpp.sh"
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/bpp.mk"
	DESTINATION share/bpp/
)
install(FILES
		cmake/Bpp.cmake
		cmake/TestFortType.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/BppConfig.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/BppConfigVersion.cmake"
	DESTINATION "share/bpp-${BPP_VERSION}/cmake/"
)
