#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10)
project(pdi_mpi_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_CFG_VALIDATOR "Build config validation script" OFF)
option(BUILD_FORTRAN "Enable Fortran support" ON)
option(BUILD_INDENT  "Enable automatic code indentation" OFF)

include(CTest)

if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	enable_language(Fortran)
	set(PDI_COMPONENTS f90)
endif()

include(GNUInstallDirs)

# Astyle
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
endif()

# MPI
set(MPI_COMPONENTS CXX)
if("${BUILD_TESTING}")
	list(APPEND MPI_COMPONENTS C)
	if("${BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})

# PDI
find_package(PDI REQUIRED COMPONENTS plugins ${PDI_COMPONENTS})

# The plugin
add_library(pdi_mpi_plugin MODULE mpi.cxx)
target_link_libraries(pdi_mpi_plugin PUBLIC PDI::PDI_plugins MPI::MPI_CXX)
set_target_properties(pdi_mpi_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${BUILD_INDENT}")
	Astyle_add_indent(indent TEST
		OPTIONS_FILE formating.astyle
		*.cxx *.h
		tests/*.c)
endif()

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_mpi_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/")
endif()

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
