#=============================================================================
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10)
project(paraconf LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${PDI_SOURCE_DIR}/cmake")


# Build options

option(BUILD_SHARED_LIBS "Build shared libraries rather than static ones" ON)
option(BUILD_FORTRAN     "Enable Fortran support" ON)

if("${BUILD_FORTRAN}")
	enable_language(Fortran)
endif()


# A little compatibility macro

if("${CMAKE_VERSION}" VERSION_LESS "3.12")
	set(LIBRARY_COMPONENT "COMPONENT" "Runtime")
else()
	set(LIBRARY_COMPONENT "COMPONENT" "Runtime" "NAMELINK_COMPONENT" "Development")
endif()


# Includes

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(CTest)
include(CMakePackageConfigHelpers)
include(GenerateExportHeader)
include(GNUInstallDirs)
set(INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATADIR}/paraconf/cmake") # not provided by GNUInstallDirs


# Dependencies

find_package(Threads REQUIRED)
find_package(yaml REQUIRED)


# Version

file(READ VERSION paraconf_VERSION)
string(STRIP "${paraconf_VERSION}" paraconf_VERSION)
string(REPLACE "." ";" paraconf_VERSION_list ${paraconf_VERSION})
list(GET paraconf_VERSION_list 0 paraconf_VERSION_MAJOR)
list(GET paraconf_VERSION_list 1 paraconf_VERSION_MINOR)
list(GET paraconf_VERSION_list 2 paraconf_VERSION_PATCH)


# Libraries

## C version

add_library(paraconf
	src/api.c
	src/status.c
	src/ypath.c
)
generate_export_header(paraconf)
target_link_libraries(paraconf Threads::Threads yaml)
target_include_directories(paraconf PUBLIC
	"$<BUILD_INTERFACE:${paraconf_SOURCE_DIR}/include/>"
	"$<BUILD_INTERFACE:${paraconf_BINARY_DIR}/>"
)
set_property(TARGET paraconf PROPERTY C_STANDARD 99)
set_property(TARGET paraconf PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET paraconf PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET paraconf PROPERTY VERSION ${paraconf_VERSION})
set_property(TARGET paraconf PROPERTY SOVERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf PROPERTY paraconf_MAJOR_VERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf APPEND PROPERTY COMPATIBLE_INTERFACE_STRING paraconf_MAJOR_VERSION)
add_library(paraconf::paraconf ALIAS paraconf)
install(TARGETS paraconf EXPORT PC_export
	ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
	LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" ${LIBRARY_COMPONENT}
	INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(FILES include/paraconf.h "${paraconf_BINARY_DIR}/paraconf_export.h"
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	COMPONENT Development
)


## Fortran version

if("${BUILD_FORTRAN}")
	add_library(paraconf_f90 src/fortran/paraconf.f90)
	target_link_libraries(paraconf_f90 PUBLIC paraconf)
	target_include_directories(paraconf_f90
		PUBLIC "$<BUILD_INTERFACE:${paraconf_SOURCE_DIR}/include>"
		INTERFACE "$<BUILD_INTERFACE:${paraconf_BINARY_DIR}/fmoddir/>")
	set_property(TARGET paraconf_f90 PROPERTY VERSION ${paraconf_VERSION})
	set_property(TARGET paraconf_f90 PROPERTY SOVERSION ${paraconf_VERSION_MAJOR})
	set_property(TARGET paraconf_f90 PROPERTY paraconf_MAJOR_VERSION ${paraconf_VERSION_MAJOR})
	set_property(TARGET paraconf_f90 PROPERTY Fortran_MODULE_DIRECTORY "${paraconf_BINARY_DIR}/fmoddir/")
	add_library(paraconf::paraconf_f90 ALIAS paraconf_f90)
	
	# Expect compilers to provide compatible modules at the minor release level at least
	string(REGEX REPLACE "^([0-9]*\\.[0-9]*)\\..*$" "\\1" Fortran_COMPILER_MINOR_VERSION "${CMAKE_Fortran_COMPILER_VERSION}")
	set(INSTALL_FMODDIR
		"${CMAKE_INSTALL_LIBDIR}/paraconf/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION}"
		CACHE PATH
		"Fortran module directory (LIBDIR/paraconf/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION})")
	if("${CMAKE_VERSION}" VERSION_LESS "3.12")
		set(LIBRARY_COMPONENT "COMPONENT" "Runtime")
	else()
		set(LIBRARY_COMPONENT "COMPONENT" "Runtime" "NAMELINK_COMPONENT" "Development")
	endif()
	install(TARGETS paraconf_f90 EXPORT PC_export
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" ${LIBRARY_COMPONENT}
		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}" "${INSTALL_FMODDIR}"
	)
	install(DIRECTORY "${paraconf_BINARY_DIR}/fmoddir/"
		DESTINATION "${INSTALL_FMODDIR}"
		COMPONENT Development
	)
	install(FILES
		include/paraconf.F90
		include/paraconff.h
		include/paraconf_f90_types.h
		include/paraconf_f90.h
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
		COMPONENT Development
	)
endif()


# Tests

if("${BUILD_TESTING}")
	add_subdirectory(tests tests)
endif()


# Installable config

write_basic_package_version_file("${paraconf_BINARY_DIR}/paraconfConfigVersion.cmake"
	VERSION "${paraconf_VERSION}"
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT PC_export
	NAMESPACE "paraconf::"
	DESTINATION "${INSTALL_CMAKEDIR}"
	FILE paraconf.cmake
	COMPONENT Development
)
install(FILES
	cmake/paraconfConfig.cmake
	cmake/Findyaml.cmake
	"${paraconf_BINARY_DIR}/paraconfConfigVersion.cmake"
	DESTINATION "${INSTALL_CMAKEDIR}"
	COMPONENT Development
)
