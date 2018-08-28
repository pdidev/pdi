#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
project(PDI LANGUAGES CXX C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/../cmake")


# Build options

option(BUILD_SHARED_LIBS "Build PDI as a shared library instead of a static one" ON)
option(ENABLE_FORTRAN    "Enable compilation of the Fortran version of PDI" ON)


# Language support

if("${ENABLE_FORTRAN}")
	enable_language(Fortran)
endif()


# Includes

include(CTest)
include(CMakePackageConfigHelpers)
include(DefaultKind)
include(GenerateExportHeader)
include(GNUInstallDirs)


# Dependencies

set(MPI_LANGS C CXX)
if("${ENABLE_FORTRAN}")
	list(APPEND MPI_LANGS Fortran)
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_LANGS})
find_package(paraconf REQUIRED)
find_package(spdlog REQUIRED)
find_package(Astyle)
if("${BUILD_DOCUMENTATION}")
	find_package(Doxygen REQUIRED OPTIONAL_COMPONENTS dot)
endif()


# Version

file(READ ../VERSION PDI_VERSION)
string(STRIP "${PDI_VERSION}" PDI_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${PDI_VERSION}")
set(PDI_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PDI_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PDI_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PDI_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${PDI_VERSION_MODIF}")
	execute_process(COMMAND "${PDI_SOURCE_DIR}/cmake/version-uid" WORKING_DIRECTORY "${PDI_SOURCE_DIR}" OUTPUT_VARIABLE PDI_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()


# Check for some portability stuff and generate config.h

get_default_kind(INTEGER   PDI_INTEGER_DEFAULT_KIND)
get_default_kind(CHARACTER PDI_CHARACTER_DEFAULT_KIND)
get_default_kind(REAL      PDI_REAL_DEFAULT_KIND)
get_default_kind(LOGICAL   PDI_LOGICAL_DEFAULT_KIND)
set(PDI_VERSION_MODIF "${PDI_VERSION_MODIF}")
configure_file(config.h.in config.h @ONLY)
configure_file(version.h.in pdi/version.h @ONLY)


# Libraries

install(DIRECTORY ../include/ DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")


## C version

set(PDI_C_SRC 
		array_datatype.cxx
		context.cxx
		global_context.cxx
		data_descriptor.cxx
		data_descriptor_impl.cxx
		datatype.cxx
		datatype_template.cxx
		error.cxx
		expression.cxx
		logger.cxx
		paraconf_wrapper.cxx
		pdi.cxx
		plugin.cxx
		record_datatype.cxx
		ref_any.cxx
		scalar_datatype.cxx
)
add_library(pdi ${PDI_C_SRC})
generate_export_header(pdi EXPORT_FILE_NAME pdi/export.h)
target_link_libraries(pdi PUBLIC paraconf::paraconf spdlog::spdlog MPI::MPI_CXX ${CMAKE_DL_LIBS})
target_include_directories(pdi PUBLIC
		"$<BUILD_INTERFACE:${PDI_BINARY_DIR}>" 
		"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/../include/>"
		"$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
if("${Astyle_FOUND}")
	add_custom_target(indent)
	Astyle_add_indent(PDI_indent
			OPTIONS_FILE "${PDI_SOURCE_DIR}/../docs/formating.astyle"
			TEST
			RECURSIVE
			../include/*.h ${PDI_C_SRC})
	add_dependencies(indent PDI_indent)
endif()
set_property(TARGET pdi PROPERTY ENABLE_EXPORTS TRUE)
set_property(TARGET pdi PROPERTY C_STANDARD 99)
set_property(TARGET pdi PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY CXX_STANDARD 11)
set_property(TARGET pdi PROPERTY CXX_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY CXX_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET pdi PROPERTY VERSION ${PDI_VERSION})
set_property(TARGET pdi PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
install(TARGETS pdi EXPORT PDI_EXPORT
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
install(FILES
		"${PDI_BINARY_DIR}/pdi/export.h"
		"${PDI_BINARY_DIR}/pdi/version.h"
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/pdi")


## Fortran version

if("${ENABLE_FORTRAN}")
find_package(Bpp REQUIRED)

bpp_preprocess(OUTPUT SRC_PDI_F90_BPP SOURCES pdi_fortran.F90.bpp)
include_directories("${PDI_BINARY_DIR}")

get_filename_component(GENINC "${SRC_PDI_F90_BPP}" DIRECTORY)
add_library(pdi_f90 fapi.c ${SRC_PDI_F90_BPP})
target_link_libraries(pdi_f90 pdi paraconf::paraconf_f90 MPI::MPI_Fortran)
set_source_files_properties(${SRC_PDI_F90_BPP} PROPERTIES GENERATED TRUE)
target_compile_definitions(pdi_f90 INTERFACE PDI_USE_F90_INCLUDE)
target_include_directories(pdi_f90 INTERFACE 
		"$<BUILD_INTERFACE:${GENINC}>" 
		"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/../include/>"
		"$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")
set_property(TARGET pdi_f90 PROPERTY VERSION ${PDI_VERSION})
set_property(TARGET pdi_f90 PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi_f90 PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi_f90 APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
install(FILES ${SRC_PDI_F90_BPP}
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
install(TARGETS pdi_f90 EXPORT PDI_EXPORT
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)
endif()


# Documentation

if("${BUILD_DOCUMENTATION}")
	configure_file("${PDI_SOURCE_DIR}/../docs/doxygen.cfg.in" "${PDI_BINARY_DIR}/docs/doxygen.cfg" @ONLY)
	add_custom_command(OUTPUT "${PDI_BINARY_DIR}/docs/html"
			COMMAND "${CMAKE_COMMAND}" -E remove_directory "${PDI_BINARY_DIR}/docs/html"
			COMMAND "${CMAKE_COMMAND}" -E make_directory "${PDI_BINARY_DIR}/docs"
			COMMAND "${CMAKE_COMMAND}" -E chdir "${PDI_BINARY_DIR}/docs" "${DOXYGEN_EXECUTABLE}" "doxygen.cfg"
			DEPENDS ${PDI_C_SRC}
	)
	add_custom_target(doc ALL DEPENDS "${PDI_BINARY_DIR}/docs/html")
	install(DIRECTORY "${PDI_BINARY_DIR}/docs/html/" DESTINATION "${CMAKE_INSTALL_DOCDIR}")
endif()


# Tests

if("${BUILD_TESTING}")
	add_subdirectory(../tests tests)
endif()


# Installable config

set(ConfigPackageLocation "${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}-${PDI_VERSION}/cmake")
write_basic_package_version_file("${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		VERSION "${PDI_VERSION}"
		COMPATIBILITY AnyNewerVersion
)
install(EXPORT PDI_EXPORT NAMESPACE "PDI::" DESTINATION "${ConfigPackageLocation}" FILE "pdi.cmake")
install(FILES
		../cmake/CMakeFindDependencyMacro.cmake
		../cmake/FindMPI.cmake
		../cmake/FindPackageHandleStandardArgs.cmake
		../cmake/FindPackageMessage.cmake
		../cmake/PDIConfig.cmake
		"${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		DESTINATION "${ConfigPackageLocation}"
)
install(DIRECTORY
		../cmake/FindMPI
		DESTINATION "${ConfigPackageLocation}"
		USE_SOURCE_PERMISSIONS
)
