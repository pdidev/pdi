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

include(AddCCompilerFlags)
include(CMakePackageConfigHelpers)
include(GenerateExportHeader)
include(TestCLocArray)


# Build options

get_property(ENABLED_LANGUAGES GLOBAL PROPERTY ENABLED_LANGUAGES)
if("${ENABLED_LANGUAGES}" MATCHES Fortran)
	set(FORTRAN_ENABLEABLE ON)
else()
	set(FORTRAN_ENABLEABLE OFF)
endif()

option(BUILD_SHARED_LIBS 
		"Build pdi as a shared library instead of a static one"
		ON
)
option(ENABLE_FORTRAN
		"Enable compilation of the Fortran version of the library"
		${FORTRAN_ENABLEABLE}
)

if("${ENABLE_FORTRAN}" AND NOT "${FORTRAN_ENABLEABLE}")
	message(FATAL_ERROR "Fortran language support disabled can not build Fortran lib, set ENABLE_FORTRAN to OFF")
endif()


# Libraries

set(CMAKE_INCLUDE_CURRENT_DIR ON)
install(DIRECTORY ../include/ DESTINATION include)

## C version
add_library(pdi
		api.c
		conf.c
		datatype.c
		status.c
		plugin_loader.c
		utils.c
		value.c
)
set_target_properties(pdi PROPERTIES C_STANDARD 99 C_STANDARD_REQUIRED ON)
add_compiler_flags(pdi PRIVATE -Wall -Wextra -std=c99)
generate_export_header(pdi)
set_property(TARGET pdi PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY VERSION ${pdi_VERSION})
set_property(TARGET pdi PROPERTY SOVERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi PROPERTY pdi_MAJOR_VERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)
target_include_directories(pdi PUBLIC
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>" 
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include/>"
)
target_link_libraries(pdi paraconf mpi)
install(TARGETS pdi EXPORT PDI_export
		LIBRARY DESTINATION lib INCLUDES DESTINATION include
		ARCHIVE DESTINATION lib INCLUDES DESTINATION include
)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/pdi_export.h" DESTINATION include)

## Fortran version
if("${ENABLE_FORTRAN}")
	add_subdirectory(../vendor/bpp bpp EXCLUDE_FROM_ALL)
	
	test_cloc_array(CLOC_ARRAY_WORKS)
	
	set(BPP_DEFINES)
	if (NOT "${CLOC_ARRAY_WORKS}")
		set(BPP_DEFINES DEFINES "NO_CLOC_ARRAY=1")
	endif()
	
	bpp_preprocess(OUTPUT SRC_PDI_F90_BPP "${BPP_DEFINES}" SOURCES PDI.h.bpp)
	get_filename_component(GENINC "${SRC_PDI_F90_BPP}" DIRECTORY)
	add_library(pdi_f90 fapi.c ${SRC_PDI_F90_BPP})
	target_link_libraries(pdi_f90 pdi paraconf_f90)
	set_source_files_properties(${SRC_PDI_F90_BPP} PROPERTIES GENERATED TRUE)
	target_compile_definitions(pdi_f90 INTERFACE PDI_USE_F90_INCLUDE)
	target_include_directories(pdi_f90 INTERFACE 
			"$<BUILD_INTERFACE:${GENINC}>" 
			"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include/>" 
			"$<INSTALL_INTERFACE:include>")
	set_property(TARGET pdi_f90 PROPERTY VERSION ${pdi_VERSION})
	set_property(TARGET pdi_f90 PROPERTY SOVERSION ${pdi_VERSION_MAJOR})
	set_property(TARGET pdi_f90 PROPERTY pdi_MAJOR_VERSION ${pdi_VERSION_MAJOR})
	set_property(TARGET pdi_f90 APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)
	install(FILES ${SRC_PDI_F90_BPP} ${CMAKE_CURRENT_SOURCE_DIR}/../include/PDI.f90
			DESTINATION "include")
	install(TARGETS pdi_f90 EXPORT PDI_export
			LIBRARY DESTINATION lib INCLUDES DESTINATION "include"
			ARCHIVE DESTINATION lib INCLUDES DESTINATION "include"
	)
endif()


# Installable config

set(ConfigPackageLocation "lib/CMake/pdi-${pdi_VERSION}")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
		VERSION ${pdi_VERSION}
		COMPATIBILITY AnyNewerVersion
)
install(EXPORT PDI_export DESTINATION ${ConfigPackageLocation})
install(FILES ../cmake/pdiConfig.cmake "${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
		DESTINATION ${ConfigPackageLocation}
)
