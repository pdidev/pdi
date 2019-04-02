#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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
project(paraconf LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../cmake")


# Build options

option(BUILD_SHARED_LIBS "Build shared libraries rather than static ones" ON)
option(ENABLE_FORTRAN    "Enable Fortran support" ON)

## Options handling

if("${ENABLE_FORTRAN}")
	enable_language(Fortran)
endif()


# Includes

include(GenerateExportHeader)
include(CMakePackageConfigHelpers)
include(GNUInstallDirs)


# Dependencies

find_package(Threads REQUIRED)
find_package(yaml REQUIRED)


# Version

file(READ ../VERSION paraconf_VERSION)
string(STRIP "${paraconf_VERSION}" paraconf_VERSION)
string(REPLACE "." ";" paraconf_VERSION_list ${paraconf_VERSION})
list(GET paraconf_VERSION_list 0 paraconf_VERSION_MAJOR)
list(GET paraconf_VERSION_list 1 paraconf_VERSION_MINOR)
list(GET paraconf_VERSION_list 2 paraconf_VERSION_PATCH)


# Libraries

## C version

add_library(paraconf
	api.c
	status.c
	ypath.c
)
generate_export_header(paraconf)
target_link_libraries(paraconf Threads::Threads yaml)
target_include_directories(paraconf PUBLIC
	"$<BUILD_INTERFACE:${paraconf_SOURCE_DIR}/../include/>"
	"$<BUILD_INTERFACE:${paraconf_BINARY_DIR}/>"
)
set_property(TARGET paraconf PROPERTY C_STANDARD 99)
set_property(TARGET paraconf PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET paraconf PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET paraconf PROPERTY CXX_STANDARD 14)
set_property(TARGET paraconf PROPERTY CXX_STANDARD_REQUIRED TRUE)
set_property(TARGET paraconf PROPERTY CXX_VISIBILITY_PRESET hidden)
set_property(TARGET paraconf PROPERTY VERSION ${paraconf_VERSION})
set_property(TARGET paraconf PROPERTY SOVERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf PROPERTY paraconf_MAJOR_VERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf APPEND PROPERTY COMPATIBLE_INTERFACE_STRING paraconf_MAJOR_VERSION)
install(TARGETS paraconf EXPORT PC_export
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(FILES ../include/paraconf.h "${paraconf_BINARY_DIR}/paraconf_export.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)


## Fortran version

if("${ENABLE_FORTRAN}")
	add_library(paraconf_f90 fortran/paraconf.f90)
	target_link_libraries(paraconf_f90 INTERFACE paraconf)
	target_include_directories(paraconf_f90 INTERFACE
		"$<BUILD_INTERFACE:${paraconf_SOURCE_DIR}/include>")
	set_property(TARGET paraconf_f90 PROPERTY VERSION ${paraconf_VERSION})
	set_property(TARGET paraconf_f90 PROPERTY SOVERSION ${paraconf_VERSION_MAJOR})
	set_property(TARGET paraconf_f90 PROPERTY paraconf_MAJOR_VERSION ${paraconf_VERSION_MAJOR})
	set_property(TARGET paraconf_f90 PROPERTY Fortran_MODULE_DIRECTORY "${paraconf_BINARY_DIR}/fortran_inc/")
	if(GNU STREQUAL "${CMAKE_Fortran_COMPILER_ID}")
		set(COMPILER_FID "gcc")
		string(REGEX REPLACE "\\..*" "" COMPILER_FVER "${CMAKE_Fortran_COMPILER_VERSION}")
	else()
		set(COMPILER_FID "${CMAKE_Fortran_COMPILER_ID}")
		set(COMPILER_FVER "${CMAKE_Fortran_COMPILER_VERSION}")
	endif()
	set(INSTALL_FINCLUDEDIR "${CMAKE_INSTALL_LIBDIR}/${COMPILER_FID}/${CMAKE_Fortran_LIBRARY_ARCHITECTURE}/${COMPILER_FVER}/finclude" CACHE PATH "Fortran module files (LIBDIR/${COMPILER_FID}/${CMAKE_Fortran_LIBRARY_ARCHITECTURE}/${COMPILER_FVER}/finclude)")
	install(TARGETS paraconf_f90 EXPORT PC_export
			ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
			LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
			INCLUDES DESTINATION "${INSTALL_FINCLUDEDIR}"
	)
	install(DIRECTORY "${paraconf_BINARY_DIR}/fortran_inc/"
			DESTINATION "${INSTALL_FINCLUDEDIR}"
	)
	install(FILES
			"${paraconf_SOURCE_DIR}/fortran/paraconff_types.h"
			"${paraconf_SOURCE_DIR}/fortran/paraconff_consts.h"
			"${paraconf_SOURCE_DIR}/fortran/paraconff.h"
			DESTINATION "${INSTALL_FINCLUDEDIR}"
	)
	install(FILES
			"${paraconf_SOURCE_DIR}/fortran/invalid_include.F90"
			DESTINATION "${INSTALL_FINCLUDEDIR}"
			RENAME "paraconf.F90"
	)
endif()


# Installable config

set(INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}-${paraconf_VERSION}/cmake")
write_basic_package_version_file("${paraconf_BINARY_DIR}/paraconf/paraconfConfigVersion.cmake"
	VERSION ${paraconf_VERSION}
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT PC_export NAMESPACE "paraconf::" DESTINATION "${INSTALL_CMAKEDIR}" FILE paraconf.cmake)
install(FILES
		../cmake/paraconfConfig.cmake
		"${paraconf_BINARY_DIR}/paraconf/paraconfConfigVersion.cmake"
		DESTINATION "${INSTALL_CMAKEDIR}"
)
