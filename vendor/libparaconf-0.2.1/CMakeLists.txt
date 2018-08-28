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
# enable C++ to work-around https://gitlab.kitware.com/cmake/cmake/issues/16967
project(paraconf LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(GenerateExportHeader)
include(CMakePackageConfigHelpers)
include(GNUInstallDirs)


# Build options

option(BUILD_SHARED_LIBS "Build paraconf as a shared library instead of a static one" ON)
option(ENABLE_FORTRAN    "Enable compilation of the Fortran version of the library" ON)
option(USE_SYSTEM_YAML   "Use an already installed libyaml instead of building the embedded one" ON)

if("${ENABLE_FORTRAN}")
	enable_language(Fortran)
endif()


# Find dependancies

find_package(Threads REQUIRED)
if("${USE_SYSTEM_YAML}")
	find_package(yaml REQUIRED)
else()
	function(build_yaml)
		set(INSTALL_CMAKE_DIR share/yaml-0.2.1/cmake)
		set(BUILD_TESTING OFF CACHE BOOL "" FORCE)
		add_subdirectory(vendor/libyaml-0.2.1)
	endfunction()
	build_yaml()
endif()


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
set_property(TARGET paraconf PROPERTY C_STANDARD 99)
set_property(TARGET paraconf PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET paraconf PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET paraconf PROPERTY VERSION ${paraconf_VERSION})
set_property(TARGET paraconf PROPERTY SOVERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf PROPERTY paraconf_MAJOR_VERSION ${paraconf_VERSION_MAJOR})
set_property(TARGET paraconf APPEND PROPERTY COMPATIBLE_INTERFACE_STRING paraconf_MAJOR_VERSION)
target_include_directories(paraconf PUBLIC
	"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>"
	"$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/>"
	"$<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}>"
)
target_link_libraries(paraconf Threads::Threads yaml)
install(TARGETS paraconf EXPORT PC_export
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(FILES include/paraconf.h "${CMAKE_CURRENT_BINARY_DIR}/paraconf_export.h"
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)


## Fortran version

if("${ENABLE_FORTRAN}")
	add_library(paraconf_f90 INTERFACE)
	target_link_libraries(paraconf_f90 INTERFACE paraconf)
	target_include_directories(paraconf_f90 INTERFACE
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
		"$<INSTALL_INTERFACE:include>")
	install(TARGETS paraconf_f90 EXPORT PC_export)
	install(FILES include/paraconf.F90
			DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	)
endif()


# Test examples

enable_testing()
add_subdirectory(example/)


# Installable config

set(ConfigPackageLocation "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}-${paraconf_VERSION}/cmake")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/paraconf/paraconfConfigVersion.cmake"
	VERSION ${paraconf_VERSION}
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT PC_export NAMESPACE "paraconf::" DESTINATION "${ConfigPackageLocation}" FILE paraconf.cmake)
install(FILES
		cmake/Findyaml.cmake
		cmake/paraconfConfig.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/paraconf/paraconfConfigVersion.cmake"
	DESTINATION "${ConfigPackageLocation}"
)
