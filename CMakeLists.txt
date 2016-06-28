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

cmake_minimum_required(VERSION 3.0)
# enable C++ to work-around cmake issue #15829 https://cmake.org/Bug/view.php?id=15829
project(pdi LANGUAGES C CXX Fortran)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

if(POLICY CMP0063)
cmake_policy(SET CMP0063 NEW)
endif()

include(AddCCompilerFlags)
include(CMakePackageConfigHelpers)
include(FortranCInterface)
include(GenerateExportHeader)


# Build options

option(BUILD_SHARED_LIBS 
	"Build pdi as a shared library instead of a static one"
	ON
)
option(ENABLE_FORTRAN
	"Enable compilation of the Fortran version of the library"
	ON
)
option(ENABLE_TESTING
	"Enable compilation of the (example) tests"
	OFF
)
option(USE_SYSTEM_PARACONF
	"Use an already installed paraconf instead of building the embedded one"
	OFF
)

get_property(ENABLED_LANGUAGES GLOBAL PROPERTY ENABLED_LANGUAGES)
if("${ENABLE_FORTRAN}" AND NOT "${ENABLED_LANGUAGES}" MATCHES Fortran)
	message(FATAL_ERROR "Fortran language support disabled can not build Fortran lib, set ENABLE_FORTRAN to OFF")
endif()


# Find dependancies

find_package(MPI REQUIRED)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf)
else()
	add_subdirectory(vendor/paraconf)
endif()


# Version

file(READ VERSION pdi_VERSION)
string(STRIP "${pdi_VERSION}" pdi_VERSION)
string(REPLACE "." ";" pdi_VERSION_list ${pdi_VERSION})
list(GET pdi_VERSION_list 0 pdi_VERSION_MAJOR)
list(GET pdi_VERSION_list 1 pdi_VERSION_MINOR)
list(GET pdi_VERSION_list 2 pdi_VERSION_PATCH)


# Libraries

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)
install(DIRECTORY include/ DESTINATION include)

#C version
add_library(pdi
	src/api.c
	src/conf.c
	src/datatype.c
	src/status.c
	src/plugin_loader.c
	src/utils.c
	src/value.c
)
add_compiler_flags(pdi PRIVATE -std=c99 -pedantic -Wall -Wextra -Werror -Wfatal-errors -pedantic-errors)
generate_export_header(pdi)
set_property(TARGET pdi PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY VERSION ${pdi_VERSION})
set_property(TARGET pdi PROPERTY SOVERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi PROPERTY pdi_MAJOR_VERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)
target_include_directories(pdi PUBLIC
	$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)
target_link_libraries(pdi paraconf mpi)
install(TARGETS pdi EXPORT PDI_export
	LIBRARY DESTINATION lib INCLUDES DESTINATION include
	ARCHIVE DESTINATION lib INCLUDES DESTINATION include
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/pdi_export.h" DESTINATION include)

#Fortran version
if("${ENABLE_FORTRAN}")
	add_library(pdi_f90 src/pdi.F90)
	target_link_libraries(pdi_f90 pdi paraconf_f90)
	set_property(TARGET pdi_f90 PROPERTY Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/modinclude")
	target_include_directories(pdi_f90 INTERFACE 
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modinclude/>" 
		"$<INSTALL_INTERFACE:lib/pdi-${pdi_VERSION}/finclude>")
	set_property(TARGET pdi_f90 PROPERTY VERSION ${pdi_VERSION})
	set_property(TARGET pdi_f90 PROPERTY SOVERSION ${pdi_VERSION_MAJOR})
	set_property(TARGET pdi_f90 PROPERTY pdi_MAJOR_VERSION ${pdi_VERSION_MAJOR})
	set_property(TARGET pdi_f90 APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)
	install(TARGETS pdi_f90 EXPORT PDI_export
		LIBRARY DESTINATION lib INCLUDES DESTINATION "lib/pdi-${pdi_VERSION}"
		ARCHIVE DESTINATION lib INCLUDES DESTINATION "lib/pdi-${pdi_VERSION}"
	)
	install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modinclude/
		DESTINATION "lib/pdi-${pdi_VERSION}/finclude"
	)
endif()


# The plugins

add_subdirectory(src/hdf5_per_process/)
add_subdirectory(src/test_plugin/)


# Test examples

if("${ENABLE_TESTING}")
	enable_testing()
	add_subdirectory(example/)
endif()


# Installable config

set(ConfigPackageLocation "lib/CMake/pdi-${pdi_VERSION}")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
	VERSION ${pdi_VERSION}
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT PDI_export DESTINATION ${ConfigPackageLocation})
install(FILES
		cmake/FindMPI.cmake
		cmake/FindLibYaml.cmake
		cmake/pdiConfig.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
	DESTINATION ${ConfigPackageLocation}
)
