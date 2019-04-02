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

# The header project

project(PARACONFSUPER LANGUAGES)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")


# Build options

option(BUILD_SHARED_LIBS             "Build shared libraries rather than static ones" ON)
option(BUILD_TESTING                 "Build tests" ON)
option(ENABLE_FORTRAN                "Enable Fortran support" ON)
set(USE_YAML     SYSTEM CACHE STRING "version of yaml to use, SYSTEM and EMBEDDED can be used")

## Options handling

if("${USE_YAML}" STREQUAL EMBEDDED)
	set(YAML_PATH "${PARACONFSUPER_SOURCE_DIR}/vendor/libyaml-0.2.1.tar")
else()
	set(YAML_PATH "${USE_YAML}")
endif()


# Includes

include(ExternalProject)


# A list of all cache variables to forward to subprojects

list(APPEND CMAKE_PREFIX_PATH "${PARACONFSUPER_BINARY_DIR}/staging" "${PARACONFSUPER_BINARY_DIR}/build-dep")
list(APPEND CMAKE_BUILD_RPATH "${PARACONFSUPER_BINARY_DIR}/staging/lib")
get_cmake_property(CACHE_VARIABLES CACHE_VARIABLES)
set(CMAKE_CACHE_ARGS)
foreach(CACHE_VARIABLE ${CACHE_VARIABLES})
	get_property(CACHE_VARIABLE_TYPE CACHE "${CACHE_VARIABLE}" PROPERTY TYPE)
	if(NOT "${CACHE_VARIABLE_TYPE}" STREQUAL STATIC AND NOT "${CACHE_VARIABLE_TYPE}" STREQUAL INTERNAL)
		list(APPEND CMAKE_CACHE_ARGS "-D${CACHE_VARIABLE}:${CACHE_VARIABLE_TYPE}=${${CACHE_VARIABLE}}")
	endif()
endforeach()
list(APPEND CMAKE_CACHE_ARGS 
		"-DCMAKE_BUILD_RPATH:PATH=${CMAKE_BUILD_RPATH}"
		"-DCMAKE_STAGING_PREFIX:PATH=<INSTALL_DIR>"
		"-DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}")

set(EP_ENV "$ENV{LD_LIBRARY_PATH}")
if(NOT "x${EP_ENV}x" STREQUAL "xx")
	set(EP_ENV ":${EP_ENV}")
endif()
set(EP_ENV "LD_LIBRARY_PATH=${PARACONFSUPER_BINARY_DIR}/staging/lib:${PARACONFSUPER_BINARY_DIR}/build-dep/lib${EP_ENV}")


# Testing

if("${BUILD_TESTING}")
	enable_testing()
	set_property(DIRECTORY PROPERTY TEST_INCLUDE_FILE "${PARACONFSUPER_BINARY_DIR}/SubTests.cmake")
	file(WRITE "${PARACONFSUPER_BINARY_DIR}/SubTests.cmake" 
#TODO: replace these path by the actual path where stuff is installed
"set(ADDPATH [=[${PARACONFSUPER_BINARY_DIR}/staging/lib:${PARACONFSUPER_BINARY_DIR}/build-dep/lib]=])\n"
[===[
set(LD_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
if("x${LD_LIBRARY_PATH}x" STREQUAL xx)
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}")
else()
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}:${LD_LIBRARY_PATH}")
endif()
]===])
endif()
function(add_testing EP_NAME)
	ExternalProject_Get_Property("${EP_NAME}" BINARY_DIR)
	if("${BUILD_TESTING}")
		file(APPEND "${PARACONFSUPER_BINARY_DIR}/SubTests.cmake" "subdirs([=[${BINARY_DIR}]=])\n")
	endif()
endfunction()


# Main project

ExternalProject_Add(PARACONF
		PREFIX "${PARACONFSUPER_BINARY_DIR}/paraconf"
		SOURCE_DIR "${PARACONFSUPER_SOURCE_DIR}/src" BUILD_ALWAYS TRUE
		CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
		INSTALL_DIR "${PARACONFSUPER_BINARY_DIR}/staging")


## Main project dependancies


if(NOT "${USE_YAML}" STREQUAL SYSTEM)
	file(MD5 "${YAML_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(YAML
			PREFIX "${PARACONFSUPER_BINARY_DIR}/yaml"
			URL "${YAML_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF" "-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
			INSTALL_DIR "${PARACONFSUPER_BINARY_DIR}/staging")
	add_dependencies(PARACONF YAML)
endif()


# Examples

if("${BUILD_TESTING}")
	ExternalProject_Add(PARACONF_EXAMPLE
			PREFIX "${PARACONFSUPER_BINARY_DIR}/paraconf_example"
			SOURCE_DIR "${PARACONFSUPER_SOURCE_DIR}/example" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			INSTALL_COMMAND ""
			DEPENDS PARACONF)
	add_testing(PARACONF_EXAMPLE)
endif()


# Installation

install(DIRECTORY "${PARACONFSUPER_BINARY_DIR}/staging/" DESTINATION "." USE_SOURCE_PERMISSIONS)
