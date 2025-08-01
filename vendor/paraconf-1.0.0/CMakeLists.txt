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



### Project header

cmake_minimum_required(VERSION 3.5)
project(PARACONF_DISTRIB LANGUAGES C CXX)



### Build options

## Global options

set(DIST_PROFILE "User" CACHE STRING "Profile to use for PDI distribution build. Options are: User, Devel")
set_property(CACHE DIST_PROFILE PROPERTY STRINGS User Devel)

if("Devel" STREQUAL "${DIST_PROFILE}")
	set(DEFAULT_BUILD_TYPE "Debug")
	option(BUILD_TESTING   "Build tests" ON)
elseif("User" STREQUAL "${DIST_PROFILE}")
	set(DEFAULT_BUILD_TYPE "Release")
	option(BUILD_TESTING   "Build tests" OFF)
else()
	message(FATAL_ERROR "DIST_PROFILE should be set to one of: User, Devel")
endif()

set(USE_DEFAULT AUTO CACHE STRING "Default version of libraries to use; this can be 1) EMBEDDED to use the provided version, 2) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 3) AUTO to use SYSTEM if available and EMBEDDED otherwise")


# Modules to build

option(BUILD_SHARED_LIBS "Build shared libraries rather than static ones" ON)
option(BUILD_FORTRAN     "Enable Fortran support" ON)



### Default build type

if(NOT "${CMAKE_BUILD_TYPE}")
	set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING "Choose the type of build, options are: None Debug Release(default) RelWithDebInfo MinSizeRel ..." FORCE)
endif()
message(STATUS " **Profile**: Distribution profile is: `${DIST_PROFILE}' (-DDIST_PROFILE=${DIST_PROFILE})")
message(STATUS " **Profile**: Build type is: `${CMAKE_BUILD_TYPE}' (-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})")
message(STATUS " **Profile**: Prefix path is: `${CMAKE_PREFIX_PATH}' (-DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})")
message(STATUS " **Profile**: Default use is: `${USE_DEFAULT}' (-DUSE_DEFAULT=${USE_DEFAULT})")
set(FEATURE_ENABLED "DISABLED")
if("${BUILD_FORTRAN}")
	set(FEATURE_ENABLED "ENABLED ")
endif()
message(STATUS " **Feature**: ${FEATURE_ENABLED} FORTRAN (-DBUILD_FORTRAN=${BUILD_FORTRAN})")
set(FEATURE_ENABLED "DISABLED")
if("${BUILD_TESTING}")
	set(FEATURE_ENABLED "ENABLED ")
endif()
message(STATUS " **Feature**: ${FEATURE_ENABLED} TESTING (-DBUILD_TESTING=${BUILD_TESTING})")



### Include sub-projects find modules

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" "${CMAKE_CURRENT_SOURCE_DIR}/paraconf/cmake")



### Dependencies

include(GNUInstallDirs)
include(SuperBuild)


## Fortran

if("${ENABLE_FORTRAN}")
	enable_language(Fortran)
endif()


## Yaml

sbuild_add_dependency(yaml "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/libyaml-0.2.2.tar"
	CMAKE_CACHE_ARGS
		"-DBUILD_TESTING:BOOL=OFF"
		"-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
		"-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
		"-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}"
		"-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
)



### Own modules

sbuild_add_module(PARACONF
	SOURCE_DIR "${PARACONF_DISTRIB_SOURCE_DIR}/paraconf"
	DEPENDS yaml
	SUBSTEPS test
)


sbuild_add_module(EXAMPLE
	ENABLE_BUILD "${BUILD_TESTING}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
	DEPENDS PARACONF
	NO_INSTALL
	SUBSTEPS test
)
