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

include(CheckSymbolExists)
include(CMakePackageConfigHelpers)
include(GenerateExportHeader)

# Check for some portability stuff and generate config.h

include(DefaultKind)
get_default_kind(INTEGER   PDI_INTEGER_DEFAULT_KIND)
get_default_kind(CHARACTER PDI_CHARACTER_DEFAULT_KIND)
get_default_kind(REAL      PDI_REAL_DEFAULT_KIND)
get_default_kind(LOGICAL   PDI_LOGICAL_DEFAULT_KIND)
set(PDI_VERSION_MODIF "${pdi_VERSION_MODIF}")
configure_file(config.h.in config.h @ONLY)
configure_file(version.h.in pdi/version.h @ONLY)

# Libraries

install(DIRECTORY ../include/ DESTINATION include)

## C++ version

add_library(pdi SHARED
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
		reference_datatype.cxx
		scalar_datatype.cxx
)
set_target_properties(pdi PROPERTIES POSITION_INDEPENDENT_CODE ON)
generate_export_header(pdi EXPORT_FILE_NAME pdi/export.h)
# This is left for the interface of PDI that may use C99 features
set_property(TARGET pdi PROPERTY C_STANDARD 99)
set_property(TARGET pdi PROPERTY C_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY CXX_STANDARD 11)
set_property(TARGET pdi PROPERTY CXX_STANDARD_REQUIRED TRUE)
set_property(TARGET pdi PROPERTY CXX_VISIBILITY_PRESET hidden)
set_property(TARGET pdi PROPERTY VERSION ${pdi_VERSION})
set_property(TARGET pdi PROPERTY SOVERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi PROPERTY pdi_MAJOR_VERSION ${pdi_VERSION_MAJOR})
set_property(TARGET pdi APPEND PROPERTY COMPATIBLE_INTERFACE_STRING pdi_MAJOR_VERSION)
target_include_directories(pdi PUBLIC
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>" 
		"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include/>"
		"$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/vendor/spdlog/>"
		"$<INSTALL_INTERFACE:include>"
)
target_link_libraries(pdi PUBLIC paraconf MPI::MPI_CXX ${CMAKE_DL_LIBS})
install(TARGETS pdi EXPORT PDI_export
		LIBRARY DESTINATION lib
		ARCHIVE DESTINATION lib
)
install(FILES
		"${CMAKE_CURRENT_BINARY_DIR}/pdi/export.h"
		"${CMAKE_CURRENT_BINARY_DIR}/pdi/version.h"
	DESTINATION include/pdi)

## Fortran version

if("${ENABLE_FORTRAN}")
add_subdirectory(../vendor/bpp bpp EXCLUDE_FROM_ALL)

bpp_preprocess(OUTPUT SRC_PDI_F90_BPP SOURCES pdi_fortran.h.bpp)
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

get_filename_component(GENINC "${SRC_PDI_F90_BPP}" DIRECTORY)
add_library(pdi_f90 fapi.c ${SRC_PDI_F90_BPP})
target_link_libraries(pdi_f90 pdi paraconf_f90 MPI::MPI_Fortran)
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
install(FILES ${SRC_PDI_F90_BPP}
		DESTINATION "include")
install(TARGETS pdi_f90 EXPORT PDI_export
		LIBRARY DESTINATION lib
		ARCHIVE DESTINATION lib
)
endif()

# Installable config

set(ConfigPackageLocation "share/pdi-${pdi_VERSION}/cmake/")
write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
		VERSION ${pdi_VERSION}
		COMPATIBILITY AnyNewerVersion
)
install(EXPORT PDI_export DESTINATION "${ConfigPackageLocation}" FILE pdi.cmake)
install(FILES
		../cmake/FindMPI.cmake
		../cmake/FindPackageHandleStandardArgs.cmake
		../cmake/FindPackageMessage.cmake
		../cmake/pdiConfig.cmake
		"${CMAKE_CURRENT_BINARY_DIR}/pdi/pdiConfigVersion.cmake"
		DESTINATION ${ConfigPackageLocation}
)
install(DIRECTORY ../cmake/FindMPI DESTINATION "${ConfigPackageLocation}")
