# Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
#               root of the project or at https://github.com/pdidev/paraconf
#
# SPDX-License-Identifier: MIT

cmake_minimum_required(VERSION 3.22...4.2)
project(paraconf LANGUAGES C)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")


# Includes

include(CTest)
include(CMakePackageConfigHelpers)
include(GenerateExportHeader)
include(GNUInstallDirs)
set(INSTALL_CMAKEDIR "${CMAKE_INSTALL_DATADIR}/paraconf/cmake" CACHE STRING "installation path for cmake files")
set(PARACONF_INSTALL_CMAKEDIR "${INSTALL_CMAKEDIR}" CACHE STRING "installation path for cmake files of Paraconf") # not provided by GNUInstallDirs


# Build options

option(BUILD_SHARED_LIBS "Build shared libraries rather than static ones" ON)
option(BUILD_EXAMPLE     "Build example" ON)
option(BUILD_FORTRAN     "Enable Fortran support" ON)
option(PARACONF_BUILD_EXAMPLE "Build Paraconf example" "${BUILD_EXAMPLE}")
option(PARACONF_BUILD_FORTRAN "Enable Fortran support in Paraconf" "${BUILD_FORTRAN}")
option(PARACONF_BUILD_TESTING "Enable Tests in Paraconf" "${BUILD_TESTING}")

if("${PARACONF_BUILD_FORTRAN}")
	enable_language(Fortran)
endif()


# Dependencies

find_package(Threads REQUIRED)
find_package(yaml REQUIRED)


# Version

file(READ VERSION paraconf_VERSION)
string(STRIP "${paraconf_VERSION}" paraconf_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${paraconf_VERSION}")
set(paraconf_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(paraconf_VERSION_MINOR "${CMAKE_MATCH_2}")
set(paraconf_VERSION_PATCH "${CMAKE_MATCH_3}")
set(paraconf_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${paraconf_VERSION_MODIF}" OR "alpha" STREQUAL "${paraconf_VERSION_MODIF}")
	execute_process(COMMAND "${paraconf_SOURCE_DIR}/cmake/version-uid" "${paraconf_VERSION_MODIF}" WORKING_DIRECTORY "${paraconf_SOURCE_DIR}" OUTPUT_VARIABLE paraconf_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(paraconf_VERSION "${paraconf_VERSION_MAJOR}.${paraconf_VERSION_MINOR}.${paraconf_VERSION_PATCH}${paraconf_VERSION_MODIF}")


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
	LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime NAMELINK_COMPONENT Development
	INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(FILES include/paraconf.h "${paraconf_BINARY_DIR}/paraconf_export.h"
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	COMPONENT Development
)


## Fortran version

if("${PARACONF_BUILD_FORTRAN}")
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
	install(TARGETS paraconf_f90 EXPORT PC_export
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime NAMELINK_COMPONENT Development
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


# Example

if("${PARACONF_BUILD_EXAMPLE}")
	add_subdirectory(example)
endif()


# Tests

if("${PARACONF_BUILD_TESTING}")
	add_subdirectory(tests)
endif()


# Installable config

write_basic_package_version_file("${paraconf_BINARY_DIR}/paraconfConfigVersion.cmake"
	VERSION "${paraconf_VERSION}"
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT PC_export
	NAMESPACE "paraconf::"
	DESTINATION "${PARACONF_INSTALL_CMAKEDIR}"
	FILE paraconf.cmake
	COMPONENT Development
)
install(FILES
	cmake/paraconfConfig.cmake
	cmake/Findyaml.cmake
	"${paraconf_BINARY_DIR}/paraconfConfigVersion.cmake"
	DESTINATION "${PARACONF_INSTALL_CMAKEDIR}"
	COMPONENT Development
)
