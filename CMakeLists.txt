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
project(pdi)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

if(POLICY CMP0063)
cmake_policy(SET CMP0063 NEW)
endif()


# Build options

option(BUILD_SHARED_LIBS 
	"Build pdi as a shared library instead of a static one"
	ON
)
option(WITH_TESTS
	"Build tests in addition to the actual library"
	ON
)
option(USE_SYSTEM_PARACONF
	"Use an already installed paraconf instead of building the embedded one"
	OFF
)


# dependancies

# find_package(MPI REQUIRED)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf)
else()
	add_subdirectory(vendor/paraconf)
endif()


# version handling

file(READ VERSION pdi_VERSION)


# the actual library

add_library(pdi
	src/api.c
	src/conf.c
	src/datatype.c
	src/plugin_loader.c
	src/value.c
	src/hdf5_per_process/hdf5_per_process.c
)
include(GenerateExportHeader)
generate_export_header(pdi)
set_target_properties(pdi PROPERTIES C_VISIBILITY_PRESET hidden)
target_include_directories(pdi
	PUBLIC
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)
target_link_libraries(pdi paraconf )
# set_property(TARGET pdi PROPERTY VERSION ${pdi_VERSION})
# set_property(TARGET pdi PROPERTY SOVERSION 3)
# set_property(TARGET pdi PROPERTY INTERFACE_pdi_MAJOR_VERSION 3)
# set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)


# the test example

if("${WITH_TESTS}")
	add_subdirectory(example/)
endif()


# library installation

install(TARGETS pdi EXPORT pdiTargets
	LIBRARY DESTINATION lib INCLUDES DESTINATION include
	ARCHIVE DESTINATION lib INCLUDES DESTINATION include
)
install(FILES
		include/pdi.h
		"${CMAKE_CURRENT_BINARY_DIR}/pdi_export.h"
	DESTINATION include
)


# cmake stuff installation

set(ConfigPackageLocation "lib/CMake/pdi-${pdi_VERSION}")
include(CMakePackageConfigHelpers)
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
	VERSION ${pdi_VERSION}
	COMPATIBILITY AnyNewerVersion
)
install(EXPORT pdiTargets DESTINATION ${ConfigPackageLocation})
install(FILES
# 		cmake/FindMPI.cmake
		cmake/pdiConfig.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
	DESTINATION ${ConfigPackageLocation}
)
