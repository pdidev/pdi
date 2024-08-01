#=============================================================================
# Copyright 2015 FZJ, Benedikt Steinbusch <b.steinbusch@fz-juelich.de>
# Copyright (C) 2020-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.10...3.25)
project(pdi_decl_sion_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_DEPRECATED "Build deprecated modules" OFF)
if("${BUILD_DEPRECATED}")
	message(WARNING "Sionlib is unmaintained and as such, the Decl'Sion plugin is deprecated, unmaintained and unsupported")
else()
	message(FATAL_ERROR "Sionlib is unmaintained and as such, the Decl'Sion plugin is deprecated, unmaintained and unsupported, set -DBUILD_DEPRECATED=ON to build anyway")
endif()

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
set(MPI_LANGS CXX)
if("${BUILD_TESTING}")
	list(APPEND MPI_LANGS C)
	if("${BUILD_FORTRAN}")
		list(APPEND MPI_LANGS Fortran)
	endif()
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_LANGS})

# SIONlib
find_package(SIONlib 1.7.6 REQUIRED)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins ${PDI_COMPONENTS})

# The plugin
add_library(pdi_decl_sion_plugin MODULE decl_sion.cxx)
set_property(SOURCE decl_sion.cxx APPEND_STRING PROPERTY COMPILE_FLAGS " ${SIONlib_MPI_C_COMPILE_FLAGS} ")
target_link_libraries(pdi_decl_sion_plugin PUBLIC PDI::PDI_plugins MPI::MPI_CXX ${SIONlib_MPI_C_LIBRARIES})
target_include_directories(pdi_decl_sion_plugin PUBLIC ${SIONlib_MPI_C_INCLUDE_DIRS})
set_target_properties(pdi_decl_sion_plugin PROPERTIES
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${BUILD_INDENT}")
	Astyle_add_indent(indent TEST
		OPTIONS_FILE formating.astyle
		decl_sion.cxx tests/*.c)
endif()

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_decl_sion_plugin
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
