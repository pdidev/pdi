#=============================================================================
# Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.16...3.29)
project(PDI LANGUAGES CXX C)
list(APPEND CMAKE_MODULE_PATH "${PDI_SOURCE_DIR}/cmake")

# Version

string(STRIP "${PDI_VERSION}" PDI_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${PDI_VERSION}")
set(PDI_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PDI_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PDI_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PDI_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${PDI_VERSION_MODIF}" OR "alpha" STREQUAL "${PDI_VERSION_MODIF}")
	execute_process(COMMAND "${PDI_SOURCE_DIR}/cmake/version-uid" "${PDI_VERSION_MODIF}" WORKING_DIRECTORY "${PDI_SOURCE_DIR}" OUTPUT_VARIABLE PDI_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(PDI_VERSION "${PDI_VERSION_MAJOR}.${PDI_VERSION_MINOR}.${PDI_VERSION_PATCH}")
set(PDI_VERSION "${PDI_VERSION}${PDI_VERSION_MODIF}")

# A little compatibility macro

if("${CMAKE_VERSION}" VERSION_LESS "3.12")
	set(LIBRARY_COMPONENT "COMPONENT" "Runtime")
else()
	set(LIBRARY_COMPONENT "COMPONENT" "Runtime" "NAMELINK_COMPONENT" "Development")
endif()


# Includes

# include(CTest)
# include(CMakePackageConfigHelpers)
# include(GenerateExportHeader)
set(_REAL_PROJECT_NAME "${PROJECT_NAME}")
set(PROJECT_NAME pdi)
# include(GNUInstallDirs)
set(PROJECT_NAME "${_REAL_PROJECT_NAME}")
set(INSTALL_PDIDATADIR "${CMAKE_INSTALL_DATADIR}/pdi" CACHE PATH "PDI data directory (DATADIR/pdi)")
set(INSTALL_CMAKEDIR   "${INSTALL_PDIDATADIR}/cmake"  CACHE PATH "cmake modules (PDIDATADIR/cmake)")
set(INSTALL_PDIPLUGINDIR  "${CMAKE_INSTALL_LIBDIR}/pdi/plugins_${PDI_VERSION}" CACHE PATH "cmake modules (LIBDIR/pdi/plugins_${PDI_VERSION})")


# Check for some portability stuff and generate config.h

if(IS_ABSOLUTE "${INSTALL_PDIPLUGINDIR}")
	set(PDI_DEFAULT_PLUGIN_PATH "${INSTALL_PDIPLUGINDIR}")
else()
	file(RELATIVE_PATH PDI_DEFAULT_PLUGIN_PATH "/${CMAKE_INSTALL_LIBDIR}" "/${INSTALL_PDIPLUGINDIR}" )
endif()

# add_library(PDI_C INTERFACE)
# generate_export_header(PDI_C BASE_NAME pdi EXPORT_FILE_NAME no-pdi/export.h)
# target_include_directories(PDI_C
# 		INTERFACE "$<BUILD_INTERFACE:${PDI_BINARY_DIR}>"  "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include/>"
# 		INTERFACE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>")
# target_link_libraries(PDI_C
# 		INTERFACE paraconf::paraconf ${CMAKE_DL_LIBS}
# 		INTERFACE spdlog::spdlog)
# target_compile_features(PDI_C INTERFACE cxx_std_17 c_std_11)
# set_property(TARGET PDI_C PROPERTY LIBRARY_OUTPUT_NAME "pdi")
# set_property(TARGET PDI_C PROPERTY ENABLE_EXPORTS TRUE)
# set_property(TARGET PDI_C PROPERTY C_VISIBILITY_PRESET hidden)
# set_property(TARGET PDI_C PROPERTY CXX_VISIBILITY_PRESET hidden)
# set_property(TARGET PDI_C PROPERTY POSITION_INDEPENDENT_CODE ON)
# set_property(TARGET PDI_C PROPERTY VERSION ${PDI_VERSION})
# set_property(TARGET PDI_C PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
# set_property(TARGET PDI_C PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
# set_property(TARGET PDI_C APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
# add_library(PDI::pdi   ALIAS PDI_C)
# add_library(PDI::PDI_C ALIAS PDI_C)
# install(TARGETS PDI_C EXPORT PDI_C_EXPORT
# 		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" ${LIBRARY_COMPONENT}
# 		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
# 		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
# )
install(FILES
		"${PDI_SOURCE_DIR}/../no-pdi/include/pdi.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
		COMPONENT Development
)
install(FILES
		"${PDI_BINARY_DIR}/no-pdi/export.h"
		"${PDI_BINARY_DIR}/no-pdi/version.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/no-pdi"
		COMPONENT Development
)
install(EXPORT PDI_C_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "PDI_C.cmake")

add_executable(pdirun IMPORTED)
add_executable(PDI::pdirun IMPORTED)
set_target_properties(pdirun PDI::pdirun PROPERTIES IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/pdirun_intree")


# Installable config

# configure_file(scripts/env.sh.in env.sh @ONLY)
# configure_file(scripts/env.xsh env.bash @ONLY)
# configure_file(scripts/env.xsh env.zsh @ONLY)
# install(FILES
# 			"${PDI_BINARY_DIR}/env.bash"
# 			"${PDI_BINARY_DIR}/env.sh"
# 			"${PDI_BINARY_DIR}/env.zsh"
# 		DESTINATION "${INSTALL_PDIDATADIR}"
# 		COMPONENT Runtime
# )


configure_file(cmake/PDIConfig.cmake.in PDIConfig.cmake @ONLY)
write_basic_package_version_file("${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		VERSION "${PDI_VERSION}"
		COMPATIBILITY AnyNewerVersion
)
install(FILES
		cmake/CMakeFindDependencyMacro.cmake
		cmake/FindPackageHandleStandardArgs.cmake
		cmake/FindPackageMessage.cmake
		"${PDI_BINARY_DIR}/PDIConfig.cmake"
		"${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		DESTINATION "${INSTALL_CMAKEDIR}"
		COMPONENT Development
)
