#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
project(pdi_mpi_plugin LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
if("${BUILD_TESTING}")
	enable_language(C)
	if("${ENABLE_FORTRAN}")
		enable_language(Fortran)
	endif()
endif()

# Includes
include(CTest)
include(GNUInstallDirs)

# Astyle
find_package(Astyle)

# MPI
set(MPI_COMPONENTS CXX)
if("${BUILD_TESTING}")
	list(APPEND MPI_COMPONENTS C)
	if("${ENABLE_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})

# PDI
find_package(PDI REQUIRED)

# The plugin
add_library(pdi_mpi_plugin MODULE mpi.cxx)
target_link_libraries(pdi_mpi_plugin PUBLIC PDI::pdi MPI::MPI_CXX)
set_target_properties(pdi_mpi_plugin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${Astyle_FOUND}")
	Astyle_add_indent(indent TEST
			OPTIONS_FILE ../../docs/formating.astyle
			mpi.cxx tests/*.c)
endif()

# Installation
install(TARGETS pdi_mpi_plugin
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	set(Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.4)
	find_package(PythonPath REQUIRED)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${PythonPath_INSTALL_SITELIBDIR}/")
endif()

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
