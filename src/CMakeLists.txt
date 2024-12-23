#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

# Includes

include(CTest)
include(CMakePackageConfigHelpers)
if("${BUILD_FORTRAN}")
	include(DefaultKind)
endif()
include(GenerateExportHeader)


# Check for some portability stuff and generate config.h

if(IS_ABSOLUTE "${INSTALL_PDIPLUGINDIR}")
	set(PDI_DEFAULT_PLUGIN_PATH "${INSTALL_PDIPLUGINDIR}")
else()
	file(RELATIVE_PATH PDI_DEFAULT_PLUGIN_PATH "/${CMAKE_INSTALL_LIBDIR}" "/${INSTALL_PDIPLUGINDIR}" )
endif()

if("${BUILD_FORTRAN}")
	get_default_kind(INTEGER   PDI_INTEGER_DEFAULT_KIND)
	get_default_kind(CHARACTER PDI_CHARACTER_DEFAULT_KIND)
	get_default_kind(REAL      PDI_REAL_DEFAULT_KIND)
	get_default_kind(LOGICAL   PDI_LOGICAL_DEFAULT_KIND)
endif()
configure_file(config.h.in config.h @ONLY)
configure_file(version.h.in pdi/version.h @ONLY)


# C API library

set(PDI_C_SRC
		array_datatype.cxx
		callbacks.cxx
		context.cxx
		context_proxy.cxx
		data_descriptor.cxx
		data_descriptor_impl.cxx
		datatype.cxx
		datatype_template.cxx
		error.cxx
		expression.cxx
		expression/impl.cxx
		expression/impl/float_literal.cxx
		expression/impl/int_literal.cxx
		expression/impl/mapping.cxx
		expression/impl/operation.cxx
		expression/impl/reference_expression.cxx
		expression/impl/sequence.cxx
		expression/impl/string_literal.cxx
		global_context.cxx
		logger.cxx
		paraconf_wrapper.cxx
		pdi.cxx
		plugin.cxx
		plugin_store.cxx
		pointer_datatype.cxx
		record_datatype.cxx
		ref_any.cxx
		scalar_datatype.cxx
		string_tools.cxx
		tuple_datatype.cxx
)
add_library(PDI_C ${PDI_C_SRC})
generate_export_header(PDI_C BASE_NAME pdi EXPORT_FILE_NAME pdi/export.h)
target_include_directories(PDI_C
		PUBLIC "$<BUILD_INTERFACE:${PDI_BINARY_DIR}>"  "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include/>"
		PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>")
target_link_libraries(PDI_C
		PUBLIC paraconf::paraconf ${CMAKE_DL_LIBS}
		PRIVATE spdlog::spdlog)
target_compile_features(PDI_C PRIVATE cxx_std_17 c_std_11)
set_property(TARGET PDI_C PROPERTY LIBRARY_OUTPUT_NAME "pdi")
set_property(TARGET PDI_C PROPERTY ENABLE_EXPORTS TRUE)
set_property(TARGET PDI_C PROPERTY C_VISIBILITY_PRESET hidden)
set_property(TARGET PDI_C PROPERTY CXX_VISIBILITY_PRESET hidden)
set_property(TARGET PDI_C PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET PDI_C PROPERTY VERSION ${PDI_VERSION})
set_property(TARGET PDI_C PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
set_property(TARGET PDI_C PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
set_property(TARGET PDI_C APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
add_library(PDI::pdi   ALIAS PDI_C)
add_library(PDI::PDI_C ALIAS PDI_C)
install(TARGETS PDI_C EXPORT PDI_C_EXPORT
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime NAMELINK_COMPONENT Development
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(FILES
		"${PDI_SOURCE_DIR}/include/pdi.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
		COMPONENT Development
)
install(FILES
		"${PDI_BINARY_DIR}/pdi/export.h"
		"${PDI_BINARY_DIR}/pdi/version.h"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/pdi"
		COMPONENT Development
)
install(EXPORT PDI_C_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "PDI_C.cmake")


# C++ plugin API library

add_library(PDI_plugins INTERFACE)
target_link_libraries(PDI_plugins INTERFACE PDI::PDI_C spdlog::spdlog)
target_compile_features(PDI_plugins INTERFACE cxx_std_17 c_std_11)
add_library(PDI::PDI_plugins ALIAS PDI_plugins)
install(TARGETS PDI_plugins EXPORT PDI_plugins_EXPORT
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime NAMELINK_COMPONENT Development
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
		INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
install(DIRECTORY "${PDI_SOURCE_DIR}/include/pdi"
		DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
		COMPONENT Development
		FILES_MATCHING PATTERN *.h PATTERN python/*.h EXCLUDE
)
install(EXPORT PDI_plugins_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "PDI_plugins.cmake")


if("${BUILD_PYTHON}")
	
	# C++ python support library
	add_library(PDI_pysupport python/python_ref_wrapper.cxx python/tools.cxx)
	target_include_directories(PDI_pysupport PUBLIC
			"$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/include>"
			PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
	)
	# We should link to PDI::PDI_plugins, but since it imposes C++17 which fails with 
	# pybind11 <= 2.4.5 , we use its dependants, cross fingers and hope for C++-14 to work
	if(2.4.5 VERSION_LESS_EQUAL "${pybind11_VERSION}")
		target_link_libraries(PDI_pysupport PUBLIC PDI::PDI_plugins)
	else()
		target_link_libraries(PDI_pysupport PUBLIC PDI::PDI_C spdlog::spdlog)
	endif()
	target_link_libraries(PDI_pysupport PUBLIC pybind11::pybind11)
	target_compile_features(PDI_pysupport PUBLIC cxx_std_14 c_std_11)
	set_property(TARGET PDI_pysupport PROPERTY LIBRARY_OUTPUT_NAME "pdi_pysupport")
	set_property(TARGET PDI_pysupport PROPERTY ENABLE_EXPORTS TRUE)
	set_property(TARGET PDI_pysupport PROPERTY C_VISIBILITY_PRESET hidden)
	set_property(TARGET PDI_pysupport PROPERTY CXX_VISIBILITY_PRESET hidden)
	set_property(TARGET PDI_pysupport PROPERTY POSITION_INDEPENDENT_CODE ON)
	set_property(TARGET PDI_pysupport PROPERTY VERSION ${PDI_VERSION})
	set_property(TARGET PDI_pysupport PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET PDI_pysupport PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET PDI_pysupport APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
	add_library(PDI::PDI_pysupport ALIAS PDI_pysupport)
	install(TARGETS PDI_pysupport EXPORT PDI_pysupport_EXPORT
			LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Runtime NAMELINK_COMPONENT Development
			ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
			INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	)
	install(DIRECTORY "${PDI_SOURCE_DIR}/include/pdi/python"
			DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/pdi"
			COMPONENT Development
	)
	install(EXPORT PDI_pysupport_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "PDI_pysupport.cmake")
	
	
	# Python API module
	
	pybind11_add_module(PDI_python MODULE python/pdi.cxx)
	target_link_libraries(PDI_python PRIVATE PDI::PDI_pysupport)
	target_include_directories(PDI_python PRIVATE
			"${PDI_SOURCE_DIR}/include"
			"${PDI_SOURCE_DIR}/src"
	)
	set_property(TARGET PDI_python PROPERTY LIBRARY_OUTPUT_NAME "_pdi")
	set_property(TARGET PDI_python PROPERTY ENABLE_EXPORTS TRUE)
	set_property(TARGET PDI_python PROPERTY C_VISIBILITY_PRESET hidden)
	set_property(TARGET PDI_python PROPERTY CXX_VISIBILITY_PRESET hidden)
	set_property(TARGET PDI_python PROPERTY POSITION_INDEPENDENT_CODE ON)
	set_property(TARGET PDI_python PROPERTY LIBRARY_OUTPUT_DIRECTORY pdi)
	install(TARGETS PDI_python
			LIBRARY DESTINATION "${Python3Path_INSTALL_SITEARCHDIR}/pdi" COMPONENT Runtime
	)
	install(DIRECTORY python/pdi
			DESTINATION "${Python3Path_INSTALL_SITEARCHDIR}"
			COMPONENT Runtime
	)
	configure_file("python/pdi/__init__.py" "pdi" COPYONLY)

endif()


# Fortran API library

if("${BUILD_FORTRAN}" OR "${BUILD_DOCUMENTATION}")
	zpp_preprocess(OUTPUT PDIF_H SOURCES fortran/pdif.h.zpp)
	add_custom_target(generated_files_for_doc DEPENDS "${PDIF_H}")
endif()
if("${BUILD_FORTRAN}")
	zpp_preprocess(OUTPUT PDI_F90 SOURCES fortran/pdi.f90.zpp)
	file(MAKE_DIRECTORY "${PDI_BINARY_DIR}/fmoddir")
	add_custom_target(mv_zpp_files # we put in in fmoddir because pdif.h is compiler dependant
			COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${PDIF_H}" "${PDI_BINARY_DIR}/fmoddir/pdif.h"
			DEPENDS "${PDIF_H}"
			VERBATIM
	)
	add_library(PDI_f90 ${PDI_F90} fortran/pdi_c.f90)
	target_link_libraries(PDI_f90 PUBLIC PDI::PDI_C paraconf::paraconf_f90 PRIVATE PDI::PDI_plugins)
	add_dependencies(PDI_f90 mv_zpp_files)
	target_include_directories(PDI_f90 PUBLIC
			"$<BUILD_INTERFACE:${PDI_BINARY_DIR}/fmoddir>"
			PRIVATE "$<BUILD_INTERFACE:${PDI_SOURCE_DIR}/src>"
	)
	set_property(TARGET PDI_f90 PROPERTY LIBRARY_OUTPUT_NAME "pdi_f90")
	set_property(TARGET PDI_f90 PROPERTY VERSION ${PDI_VERSION})
	set_property(TARGET PDI_f90 PROPERTY SOVERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET PDI_f90 PROPERTY PDI_MAJOR_VERSION ${PDI_VERSION_MAJOR})
	set_property(TARGET PDI_f90 APPEND PROPERTY COMPATIBLE_INTERFACE_STRING PDI_MAJOR_VERSION)
	set_property(TARGET PDI_f90 PROPERTY Fortran_MODULE_DIRECTORY "${PDI_BINARY_DIR}/fmoddir/")
	# Expect compilers to provide compatible modules at the minor release level at least
	string(REGEX REPLACE "^([0-9]*\\.[0-9]*)\\..*$" "\\1" Fortran_COMPILER_MINOR_VERSION "${CMAKE_Fortran_COMPILER_VERSION}")
	set(INSTALL_FMODDIR
		"${CMAKE_INSTALL_LIBDIR}/pdi/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION}"
		CACHE PATH
		"Fortran module directory (LIBDIR/pdi/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION})")
	install(TARGETS PDI_f90 EXPORT pdi_f90_EXPORT
			LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT COMPONENT Runtime NAMELINK_COMPONENT Development
			ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}" COMPONENT Development
			INCLUDES DESTINATION "${INSTALL_FMODDIR}"
	)
	install(DIRECTORY "${PDI_BINARY_DIR}/fmoddir/"
			DESTINATION "${INSTALL_FMODDIR}"
			COMPONENT Development
	)
	install(EXPORT pdi_f90_EXPORT NAMESPACE "PDI::" DESTINATION "${INSTALL_CMAKEDIR}" FILE "PDI_f90.cmake")
	add_library(PDI::pdi_f90     ALIAS PDI_f90)
	add_library(PDI::PDI_f90     ALIAS PDI_f90)
	add_library(PDI::PDI_Fortran ALIAS PDI_f90)
endif()


# Tests

if("${BUILD_TESTING}")
	add_subdirectory(tests tests)
endif()


# Installable config

configure_file(cmake/PDIConfig.cmake.in PDIConfig.cmake @ONLY)
write_basic_package_version_file("${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		VERSION "${PDI_VERSION}"
		COMPATIBILITY AnyNewerVersion
)
install(FILES
		cmake/CMakeFindDependencyMacro.cmake
		"${PDI_BINARY_DIR}/PDIConfig.cmake"
		"${PDI_BINARY_DIR}/PDIConfigVersion.cmake"
		DESTINATION "${INSTALL_CMAKEDIR}"
		COMPONENT Development
)
