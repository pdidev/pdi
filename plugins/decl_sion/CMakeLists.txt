#=============================================================================
# Copyright 2015 FZJ, Benedikt Steinbusch <b.steinbusch@fz-juelich.de>
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

cmake_minimum_required(VERSION 3.5)
project(pdi_decl_sion_plugin LANGUAGES CXX)
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
set(MPI_LANGS CXX)
if("${BUILD_TESTING}")
	list(APPEND MPI_LANGS C)
	if("${ENABLE_FORTRAN}")
		list(APPEND MPI_LANGS Fortran)
	endif()
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_LANGS})

# SIONlib
find_package(SIONlib REQUIRED)

# PDI
find_package(PDI REQUIRED)

# The plugin
add_library(pdi_decl_sion_plugin MODULE decl_sion.cxx)
set_property(SOURCE decl_sion.cxx APPEND_STRING PROPERTY COMPILE_FLAGS " ${SIONlib_MPI_C_COMPILE_FLAGS} ")
target_link_libraries(pdi_decl_sion_plugin PUBLIC PDI::pdi MPI::MPI_CXX ${SIONlib_MPI_C_LIBRARIES})
target_include_directories(pdi_decl_sion_plugin PUBLIC ${SIONlib_MPI_C_INCLUDE_DIRS})
set_target_properties(pdi_decl_sion_plugin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${Astyle_FOUND}")
	Astyle_add_indent(indent TEST
			OPTIONS_FILE ../../docs/formating.astyle
			decl_sion.cxx tests/*.c)
endif()

# Installation
install(TARGETS pdi_decl_sion_plugin
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
