#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(CTest)
include(GNUInstallDirs)


find_package(Python3Path REQUIRED COMPONENTS Interpreter)
find_package(PyYAML REQUIRED)


install(DIRECTORY pdicfg_validator
		DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/"
)

file(RELATIVE_PATH PDICFG_MODULE_PATH "/invalid/${CMAKE_INSTALL_BINDIR}" "/invalid/${Python3Path_INSTALL_SITELIBDIR}/")
configure_file("pdicfg_validate" "pdicfg_validate" @ONLY)
install(PROGRAMS "${CMAKE_BINARY_DIR}/pdicfg_validate"
	DESTINATION "${CMAKE_INSTALL_BINDIR}/"
)
add_executable(pdicfg_validate IMPORTED GLOBAL)
set_target_properties(pdicfg_validate PROPERTIES IMPORTED_LOCATION "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validate")


if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()


configure_file(cmake/PDIcfgConfig.cmake.in PDIcfgConfig.cmake @ONLY)
install(FILES "${CMAKE_BINARY_DIR}/PDIcfgConfig.cmake"
		DESTINATION "${CMAKE_INSTALL_DATADIR}/pdicfg/cmake"
		COMPONENT Development
)
