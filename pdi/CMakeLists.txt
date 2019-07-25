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

cmake_minimum_required(VERSION 3.5)
project(PDI LANGUAGES CXX C)
list(APPEND CMAKE_MODULE_PATH "${PDI_SOURCE_DIR}/cmake")


# Build options

option(BUILD_UNSTABLE     "Build all features by default including those not stable yet" OFF)

option(BUILD_DOCUMENTATION "Build documentation" ON)
option(BUILD_SHARED_LIBS   "Build shared libraries rather than static ones" ON)
option(BUILD_TESTING       "Build tests" ON)
option(BUILD_FORTRAN       "Enable Fortran support" ON)
option(BUILD_PYTHON        "Enable Python support" "${BUILD_UNSTABLE}")
option(BUILD_INDENT        "Enable automatic code indentation" OFF)

if("${BUILD_TESTING}" AND "${CMAKE_VERSION}" VERSION_LESS "3.10") 
	message(FATAL_ERROR "Minimum CMake version to build tests is 3.10. Please update CMake or turn off testing by adding the flag `-DBUILD_TESTING=OFF` to the cmake command line.")
endif()
if("${BUILD_DOCUMENTATION}" AND "${CMAKE_VERSION}" VERSION_LESS "3.10") 
	message(FATAL_ERROR "Minimum CMake version to build documentation is 3.10. Please update CMake or turn off documentation generation by adding the flag `-DBUILD_DOCUMENTATION=OFF` to the cmake command line.")
endif()
if(NOT "${BUILD_SHARED_LIBS}") 
	message(FATAL_ERROR "Building PDI as a static lib is not supported currently")
endif()
if(NOT "${CMAKE_VERSION}" VERSION_LESS "3.12")
	set(PDI_NAMELINK_COMPONENT NAMELINK_COMPONENT Development)
endif()


# Language support

if("${BUILD_FORTRAN}")
	enable_language(Fortran)
endif()


# Includes

include(CTest)
include(CMakePackageConfigHelpers)
if("${BUILD_FORTRAN}")
	include(DefaultKind)
endif()
include(GenerateExportHeader)
include(GNUInstallDirs)


# Dependencies

set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
find_package(paraconf 0.4.0 REQUIRED COMPONENTS ${PARACONF_COMPONENTS})
find_package(spdlog 1.3.1 REQUIRED)
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
endif()
if("${BUILD_PYTHON}")
	find_package(Python3Path REQUIRED COMPONENTS Interpreter Development)
	find_package(pybind11 2.3.0 REQUIRED)
endif()
if("${BUILD_FORTRAN}")
	find_package(Bpp 0.3.0 REQUIRED)
endif()


# Version

file(READ VERSION PDI_VERSION)
string(STRIP "${PDI_VERSION}" PDI_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${PDI_VERSION}")
set(PDI_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PDI_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PDI_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PDI_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${PDI_VERSION_MODIF}")
	execute_process(COMMAND "${PDI_SOURCE_DIR}/cmake/version-uid" WORKING_DIRECTORY "${PDI_SOURCE_DIR}" OUTPUT_VARIABLE PDI_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(PDI_VERSION "${PDI_VERSION_MAJOR}.${PDI_VERSION_MINOR}.${PDI_VERSION_PATCH}")
if(NOT "x${PDI_VERSION_MODIF}x" STREQUAL "xx")
	set(PDI_VERSION "${PDI_VERSION}-${PDI_VERSION_MODIF}")
endif()


# Check for some portability stuff and generate config.h

if("${BUILD_FORTRAN}")
	get_default_kind(INTEGER   PDI_INTEGER_DEFAULT_KIND)
	get_default_kind(CHARACTER PDI_CHARACTER_DEFAULT_KIND)
	get_default_kind(REAL      PDI_REAL_DEFAULT_KIND)
	get_default_kind(LOGICAL   PDI_LOGICAL_DEFAULT_KIND)
endif()
configure_file(src/config.h.in config.h @ONLY)
configure_file(src/version.h.in pdi/version.h @ONLY)


# Libraries

## C version

set(PDI_SRC 
		src/array_datatype.cxx
		src/context.cxx
		src/context_proxy.cxx
		src/global_context.cxx
		src/data_descriptor.cxx
		src/data_descriptor_impl.cxx
		src/datatype.cxx
		src/datatype_template.cxx
		src/error.cxx
		src/expression.cxx
		src/logger.cxx
		src/paraconf_wrapper.cxx
		src/plugin.cxx
		src/record_datatype.cxx
		src/ref_any.cxx
		src/scalar_datatype.cxx
)
set(PDI_C_SRC 
		${PDI_SRC}
		src/pdi.cxx
)
add_library(pdi ${PDI_C_SRC})
generate_export_header(pdi EXPORT_FILE_NAME pdi/export.h)
target_link_libraries(pdi PUBLIC paraconf::paraconf spdlog::spdlog ${CMAKE_DL_LIBS})
target_include_directories(pdi PUBLIC
		"$<BUILD_INTERFACE:${PDI_BINARY_DIR}>" 
		"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include/>"
		PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
)
set_property(TARGET pdi PROPERTY ENABLE_EXPORTS TRUE)
set_property(TARGET pdi PROPERTY C_STANDARD 99)
set_property(TARGET pdi PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY CXX_STANDARD 14)
set_property(TARGET pdi PROPERTY CXX_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY CXX_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET pdi PROPERTY VERSION ${PDI_VERSION})
set_property(TARGET pdi PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
add_library(PDI::pdi ALIAS pdi)
install(TARGETS pdi EXPORT PDI_EXPORT
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime ${PDI_NAMELINK_COMPONENT}
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(DIRECTORY "${PDI_SOURCE_DIR}/include/"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
		COMPONENT Development
		FILES_MATCHING PATTERN *.h PATTERN python/*.h EXCLUDE
)
install(FILES
		"${PDI_BINARY_DIR}/pdi/export.h"
		"${PDI_BINARY_DIR}/pdi/version.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/pdi"
		COMPONENT Development
)

file(COPY scripts/pdirun_intree DESTINATION "${CMAKE_BINARY_DIR}/")
add_executable(pdirun IMPORTED)
add_executable(PDI::pdirun IMPORTED)
set_target_properties(pdirun PDI::pdirun PROPERTIES IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/pdirun_intree")

install(PROGRAMS scripts/pdirun
		DESTINATION "${CMAKE_INSTALL_BINDIR}"
		COMPONENT Runtime
)

if("${BUILD_INDENT}")
	add_custom_target(indent)
	Astyle_add_indent(pdi_indent
			OPTIONS_FILE "${PDI_SOURCE_DIR}/docs/formating.astyle"
			TEST
			RECURSIVE
			${PDI_SOURCE_DIR}/include/*.h ${PDI_C_SRC})
	add_dependencies(indent pdi_indent)
endif()


## Python version

if("${BUILD_PYTHON}")
	add_library(pdi_python src/python/tools.cxx)
	target_link_libraries(pdi_python PUBLIC pybind11::pybind11 pdi)
	target_include_directories(pdi_python PUBLIC
			"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include>"
			PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
	)
	set_property(TARGET pdi_python PROPERTY ENABLE_EXPORTS TRUE)
	set_property(TARGET pdi_python PROPERTY C_STANDARD 99)
	set_property(TARGET pdi_python PROPERTY C_STANDARD_REQUIRED TRUE)
	set_property(TARGET pdi_python PROPERTY C_VISIBILITY_PRESET hidden)
	set_property(TARGET pdi_python PROPERTY CXX_STANDARD 14)
	set_property(TARGET pdi_python PROPERTY CXX_STANDARD_REQUIRED TRUE)
	set_property(TARGET pdi_python PROPERTY CXX_VISIBILITY_PRESET hidden)
	set_property(TARGET pdi_python PROPERTY POSITION_INDEPENDENT_CODE ON)
	set_property(TARGET pdi_python PROPERTY VERSION ${PDI_VERSION})
	set_property(TARGET pdi_python PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET pdi_python PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET pdi_python APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
	add_library(PDI::pdi_python ALIAS pdi_python)
	install(TARGETS pdi_python EXPORT PDI_EXPORT
			LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime ${PDI_NAMELINK_COMPONENT}
			ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
			INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	)
	install(DIRECTORY "${PDI_SOURCE_DIR}/include/pdi/python"
			DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/pdi"
			COMPONENT Development
	)
	
	pybind11_add_module(pdi_python_module MODULE src/python/pdi.cxx)
	target_link_libraries(pdi_python_module PUBLIC pdi_python)
	target_include_directories(pdi_python_module PUBLIC
			"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include>"
			PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
	)
	set_property(TARGET pdi_python_module PROPERTY LIBRARY_OUTPUT_NAME "_pdi")
	set_property(TARGET pdi_python_module PROPERTY ENABLE_EXPORTS TRUE)
	set_property(TARGET pdi_python_module PROPERTY C_STANDARD 99)
	set_property(TARGET pdi_python_module PROPERTY C_STANDARD_REQUIRED TRUE)
	set_property(TARGET pdi_python_module PROPERTY C_VISIBILITY_PRESET hidden)
	set_property(TARGET pdi_python_module PROPERTY CXX_STANDARD 14)
	set_property(TARGET pdi_python_module PROPERTY CXX_STANDARD_REQUIRED TRUE)
	set_property(TARGET pdi_python_module PROPERTY CXX_VISIBILITY_PRESET hidden)
	set_property(TARGET pdi_python_module PROPERTY POSITION_INDEPENDENT_CODE ON)
	set_property(TARGET pdi_python_module PROPERTY LIBRARY_OUTPUT_DIRECTORY pdi)
	install(TARGETS pdi_python_module EXPORT PDI_EXPORT
			LIBRARY DESTINATION "${Python3Path_INSTALL_SITEARCHDIR}/pdi" COMPONENT Runtime
	)
	install(DIRECTORY src/python/pdi
			DESTINATION "${Python3Path_INSTALL_SITEARCHDIR}"
			COMPONENT Runtime
	)
	file(COPY src/python/pdi DESTINATION "${CMAKE_BINARY_DIR}")

	if("${BUILD_INDENT}")
		Astyle_add_indent(python_indent
				OPTIONS_FILE "${PDI_SOURCE_DIR}/docs/formating.astyle"
				TEST
				RECURSIVE
				include/*.h src/python/pdi.cxx src/python/tools.cxx)
		add_dependencies(indent python_indent)
	endif()
endif()


## Fortran version

if("${BUILD_FORTRAN}")
	bpp_preprocess(OUTPUT PDIF_H SOURCES src/fortran/pdif.h.bpp)
	bpp_preprocess(OUTPUT PDI_F90 SOURCES src/fortran/pdi.f90.bpp)
	file(MAKE_DIRECTORY "${PDI_BINARY_DIR}/fortran_inc")
	add_custom_target(mv_bpp_files
			COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${PDIF_H}" "${PDI_BINARY_DIR}/fortran_inc/pdif.h"
			DEPENDS "${PDIF_H}"
			VERBATIM
	)

	add_library(pdi_f90 ${PDI_F90} src/fortran/pdi_c.f90)
	target_link_libraries(pdi_f90 pdi paraconf::paraconf_f90)
	add_dependencies(pdi_f90 mv_bpp_files)
	target_include_directories(pdi_f90 PUBLIC
			"$<BUILD_INTERFACE:${PDI_BINARY_DIR}/fortran_inc>"
			PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
	)
	set_property(TARGET pdi_f90 PROPERTY VERSION ${PDI_VERSION})
	set_property(TARGET pdi_f90 PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET pdi_f90 PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET pdi_f90 APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
	set_property(TARGET pdi_f90 PROPERTY Fortran_MODULE_DIRECTORY "${PDI_BINARY_DIR}/fortran_inc/")
	add_library(PDI::pdi_f90 ALIAS pdi_f90)
	if(GNU STREQUAL "${CMAKE_Fortran_COMPILER_ID}")
		set(COMPILER_FID "gcc")
		string(REGEX REPLACE "\\..*" "" COMPILER_FVER "${CMAKE_Fortran_COMPILER_VERSION}")
	else()
		set(COMPILER_FID "${CMAKE_Fortran_COMPILER_ID}")
		set(COMPILER_FVER "${CMAKE_Fortran_COMPILER_VERSION}")
	endif()
	set(INSTALL_FINCLUDEDIR "${CMAKE_INSTALL_LIBDIR}/${COMPILER_FID}/${CMAKE_Fortran_LIBRARY_ARCHITECTURE}/${COMPILER_FVER}/finclude" CACHE PATH "Fortran module files (LIBDIR/${COMPILER_FID}/${CMAKE_Fortran_LIBRARY_ARCHITECTURE}/${COMPILER_FVER}/finclude)")
	install(TARGETS pdi_f90 EXPORT PDI_EXPORT
			LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime ${PDI_NAMELINK_COMPONENT}
			ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
			INCLUDES DESTINATION "${INSTALL_FINCLUDEDIR}"
	)
	install(DIRECTORY "${PDI_BINARY_DIR}/fortran_inc/"
			DESTINATION "${INSTALL_FINCLUDEDIR}"
			COMPONENT Development
	)
	install(FILES "${PDI_SOURCE_DIR}/include/pdi.F90"
			DESTINATION "${INSTALL_FINCLUDEDIR}"
			COMPONENT Development
	)
endif()


# Documentation

if("${BUILD_DOCUMENTATION}")
	add_subdirectory(docs docs)
endif()


# Tests

if("${BUILD_TESTING}")
	add_subdirectory(tests tests)
endif()


# Installable config

set(INSTALL_PDIDATADIR "${CMAKE_INSTALL_DATADIR}/pdi" CACHE PATH "PDI data directory (DATADIR/pdi)")
configure_file(scripts/env.bash.in env.bash @ONLY)
configure_file(cmake/PDIConfig.cmake.in PDIConfig.cmake @ONLY)
install(FILES "${PDI_BINARY_DIR}/env.bash"
		DESTINATION "${INSTALL_PDIDATADIR}"
		COMPONENT Runtime
)

set(INSTALL_CMAKEDIR "${INSTALL_PDIDATADIR}/cmake" CACHE PATH "cmake modules (PDIDATADIR/cmake)")
write_basic_package_version_file("${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		VERSION "${PDI_VERSION}"
		COMPATIBILITY AnyNewerVersion
)
install(EXPORT PDI_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "pdi.cmake")
install(FILES
		cmake/CMakeFindDependencyMacro.cmake
		cmake/FindPackageHandleStandardArgs.cmake
		cmake/FindPackageMessage.cmake
		"${PDI_BINARY_DIR}/PDIConfig.cmake"
		"${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		DESTINATION "${INSTALL_CMAKEDIR}"
		COMPONENT Development
)
