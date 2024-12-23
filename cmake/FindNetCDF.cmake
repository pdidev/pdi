################################################################################
# Copyright (C) 2020-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#     * Neither the name of the <organization> nor the
#     names of its contributors may be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

#[==[
.rst:

FindNetCDF
----------

Find NetCDF, a library for reading and writing self describing array data.
This module invokes the NetCDF config script (nc-config) installed alongside
NetCDF to detect the options required to use it.

To provide the module with a hint about where to find your NetCDF installation,
you can set the environment variable NetCDF_CONFIG. The Find module will use
this as the NetCDF configuration script.

This module will define the following variables:

:NetCDF_FOUND: Whether NetCDF was found or not.
:NetCDF_VERSION: The version of NetCDF found.
:NetCDF_INCLUDE_DIRECTORIES: .
:NetCDF_COMPILE_DEFINITIONS: .
:NetCDF_LINK_LIBRARIES: .
:NetCDF_FEATURES: the features supported amongst CDF5, DAP, DAP2, DAP4,
                  DISKLESS, HDF4, HDF5, JNA, MMAP, NC2, NC4, PARALLEL,
                  PARALLEL4, PNETCDF

In addition, the module provides the following target to link against:

:NetCDF::NetCDF: A target for NetCDF C API.

The following variable can be set to guide the search for NetCDF libraries and
includes:

:NetCDF_CFGSCRIPT: The config script to use.

:NetCDF_FIND_DEBUG: Set to true to get extra debugging output.

:NetCDF_FIND_STRATEGIES: A list of strategies to use in order to find NetCDF,
  defaults to: `CMAKE` `CFGSCRIPT` `PKGCONFIG` `FALLBACK`.

#]==]

cmake_minimum_required(VERSION 3.16...3.25)

set(_NetCDF_features_list CDF5 DAP DAP2 DAP4 DISKLESS HDF4 HDF5 JNA MMAP NC2 NC4 PARALLEL PARALLEL4 PNETCDF)

macro(_NetCDF_remove_duplicates_from_beginning _list_name)
	if(${_list_name})
		list(REVERSE ${_list_name})
		list(REMOVE_DUPLICATES ${_list_name})
		list(REVERSE ${_list_name})
	endif()
endmacro()


macro(_NetCDF_public_vars)
	foreach(_NetCDF_VAR  NetCDF_FOUND NetCDF_VERSION NetCDF_INCLUDE_DIRECTORIES NetCDF_COMPILE_DEFINITIONS NetCDF_COMPILE_OPTIONS NetCDF_LINK_LIBRARIES NetCDF_FEATURES)
		set("${_NetCDF_VAR}" "${${_NetCDF_VAR}}" PARENT_SCOPE)
		if(NetCDF_FIND_DEBUG)
			message(STATUS "${_NetCDF_VAR}: ${${_NetCDF_VAR}}")
		endif()
	endforeach()
endmacro()


function(_NetCDF_target_from_flags CFLAGS INCLUDE_DIRECTORIES COMPILE_DEFINITIONS LDFLAGS LIBRARY_DIRS LINK_LIBRARIES)
	string(REPLACE ";" " " LDFLAGS "${LDFLAGS}")
	string(REPLACE ";" " " CFLAGS "${CFLAGS}")
	if(${WIN32})
		set(CM35_NATIVE_COMMAND WINDOWS_COMMAND)
	else(${WIN32})
		set(CM35_NATIVE_COMMAND UNIX_COMMAND)
	endif(${WIN32})
	separate_arguments(COMPILE_ARGS ${CM35_NATIVE_COMMAND} "${LDFLAGS} ${CFLAGS}")

	foreach(ARG IN LISTS COMPILE_ARGS)
		if("${ARG}" MATCHES "^-I(.*)$")
			# include directory
			if(NOT EXISTS "${CMAKE_MATCH_1}")
				continue()
			endif()
			list(APPEND INCLUDE_DIRECTORIES "${CMAKE_MATCH_1}")
		elseif("${ARG}" MATCHES "^-D(.*)$")
			# compile definition
			list(APPEND COMPILE_DEFINITIONS "${CMAKE_MATCH_1}")
		elseif("${ARG}" MATCHES "^-L(.*)$")
			# library search path
			if(NOT EXISTS "${CMAKE_MATCH_1}")
				continue()
			endif()
			list(APPEND LIBRARY_DIRS "${CMAKE_MATCH_1}")
		elseif("${ARG}" MATCHES "^-l(.*)$")
			# library name
			list(APPEND LINK_LIBRARIES "${CMAKE_MATCH_1}")
		elseif("${ARG}" MATCHES "^(.:)?[/\\].*\\.(a|so|dylib|sl|lib)$")
			# library file
			if(NOT EXISTS "${ARG}")
				continue()
			endif()
			get_filename_component(LPATH "${ARG}" DIRECTORY)
			get_filename_component(LNAME "${ARG}" NAME_WE)
			string(REGEX REPLACE "^lib" "" LNAME "${LNAME}")
			list(APPEND LIBRARY_DIRS "${LPATH}")
			list(APPEND LINK_LIBRARIES "${LNAME}")
		endif()
	endforeach()
	
	set(LIBRARIES_UP "${LINK_LIBRARIES}")
	set(LINK_LIBRARIES)
	foreach(LIB IN LISTS LIBRARIES_UP)
		if(EXISTS "${LIB}")
			get_filename_component(LPATH "${LIB}" DIRECTORY)
			get_filename_component(LNAME "${LIB}" NAME_WE)
			string(REGEX REPLACE "^lib" "" LNAME "${LNAME}")
			list(APPEND LIBRARY_DIRS "${LPATH}")
			list(APPEND LINK_LIBRARIES "${LNAME}")
		else()
			list(APPEND LINK_LIBRARIES "${LIB}")
		endif()
	endforeach()
	
	_NetCDF_remove_duplicates_from_beginning(LIBRARY_DIRS)
	set(LIBRARIES_UP "${LINK_LIBRARIES}")
	set(LINK_LIBRARIES)
	foreach(LIB IN LISTS LIBRARIES_UP)
		find_library("NetCDF_LIBRARY_${LIB}" NAMES "${LIB}"
			HINTS ${LIBRARY_DIRS}
		)
		if(EXISTS "${NetCDF_LIBRARY_${LIB}}")
			list(APPEND LINK_LIBRARIES "${NetCDF_LIBRARY_${LIB}}")
		else()
			message(STATUS "Unable to locate lib `${LIB}' (${LIBRARY_DIRS})")
			return()
		endif()
	endforeach()
	set(NetCDF_FOUND TRUE PARENT_SCOPE)
	_NetCDF_remove_duplicates_from_beginning(INCLUDE_DIRECTORIES)
	set(NetCDF_INCLUDE_DIRECTORIES "${INCLUDE_DIRECTORIES}" PARENT_SCOPE)
	_NetCDF_remove_duplicates_from_beginning(COMPILE_DEFINITIONS)
	set(NetCDF_COMPILE_DEFINITIONS "${COMPILE_DEFINITIONS}" PARENT_SCOPE)
	_NetCDF_remove_duplicates_from_beginning(LINK_LIBRARIES)
	set(NetCDF_LINK_LIBRARIES "${LINK_LIBRARIES}" PARENT_SCOPE)
endfunction()


function(_NetCDF_parse_includes VERSION FEATURES)
	find_file(NetCDF_META_H "netcdf_meta.h" HINTS ${NetCDF_INCLUDE_DIRECTORIES})
	if(NOT EXISTS "${NetCDF_META_H}")
		return()
	endif()
	mark_as_advanced(NetCDF_META_H)
	if("${VERSION}")
		file(STRINGS "${NetCDF_META_H}" VERSION_LINES
			REGEX "#define[ \t]+NC_VERSION_(MAJOR|MINOR|PATCH|NOTE)"
		)
		string(REGEX REPLACE ".*NC_VERSION_MAJOR *\([0-9]*\).*" "\\1" VERSION_MAJOR "${VERSION_LINES}")
		string(REGEX REPLACE ".*NC_VERSION_MINOR *\([0-9]*\).*" "\\1" VERSION_MINOR "${VERSION_LINES}")
		string(REGEX REPLACE ".*NC_VERSION_PATCH *\([0-9]*\).*" "\\1" VERSION_PATCH "${VERSION_LINES}")
		string(REGEX REPLACE ".*NC_VERSION_NOTE *\"\([^\"]*\)\".*" "\\1" VERSION_NOTE "${VERSION_LINES}")
		set(NetCDF_VERSION "${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}${VERSION_NOTE}")
	endif()
	
	set(NetCDF_FEATURES)
	foreach(FEATURE IN LISTS _NetCDF_features_list)
		file(STRINGS "${NetCDF_META_H}" FEATURE_${FEATURE}
			REGEX "#[ \t]*define[ \t]+NC_HAS_${FEATURE}[ \t]+[A-za-z0-9]+"
		)
		string(REGEX REPLACE "#[ \t]*define[ \t]+NC_HAS_${FEATURE}[ \t]+([A-za-z0-9]+).*" "\\1" FEATURE_${FEATURE} "${FEATURE_${FEATURE}}")
		if("${FEATURE_${FEATURE}}")
			list(APPEND NetCDF_FEATURES "${FEATURE}")
		endif()
	endforeach()
	_NetCDF_public_vars()
endfunction()


# Try to find NetCDF using an installed netcdf-config.cmake
function(_NetCDF_find_CMAKE)
	find_package(netCDF CONFIG QUIET)
	if("${netCDF_FOUND}")
		# Forward the variables in a consistent way.
		set(NetCDF_FOUND TRUE)
		set(NetCDF_VERSION "${netCDF_VERSION}")
		if(TARGET "${netCDF_LIBRARIES}")
			if(NetCDF_FIND_DEBUG)
				message(STATUS "Found NetCDF CMAKE target ${netCDF_LIBRARIES}")
			endif()
			set(NetCDF_LINK_LIBRARIES ${netCDF_LIBRARIES})
		else()
			_NetCDF_target_from_flags("" "${netCDF_INCLUDE_DIR}" "" "" "${netCDF_LIB_DIR}" "${netCDF_LIBRARIES}")
		endif()
		_NetCDF_parse_includes(FALSE TRUE)
		foreach(FEATURE IN LISTS _NetCDF_features_list)
			if("${netCDF_HAS_${FEATURE}}")
				list(APPEND NetCDF_FEATURES "${FEATURE}")
			endif()
		endforeach()
		list(REMOVE_DUPLICATES NetCDF_FEATURES)
	endif()
	_NetCDF_public_vars()
endfunction()


function(_NetCDF_find_PKGCONFIG)
	find_package(PkgConfig QUIET)
	if("${PkgConfig_FOUND}")
		pkg_check_modules(_NetCDF QUIET netcdf)
		if("${_NetCDF_FOUND}")
			# Forward the variables in a consistent way.
			set(NetCDF_VERSION "${_NetCDF_VERSION}")
			_NetCDF_target_from_flags("${_NetCDF_CFLAGS} ${_NetCDF_CFLAGS_OTHER}" "${_NetCDF_INCLUDE_DIRECTORIES}" "" "${_NetCDF_LDFLAGS} ${_NetCDF_LDFLAGS_OTHER}" "${_NetCDF_LIBRARY_DIRS}" "${_NetCDF_LIBRARIES}")
			_NetCDF_parse_includes(FALSE TRUE)
		endif()
	endif()
	_NetCDF_public_vars()
endfunction()


function(_NetCDF_find_CFGSCRIPT CFGSCRIPT)
	if(NOT IS_ABSOLUTE "${CFGSCRIPT}")
		find_program(_NetCDF_CFGSCRIPT "${CFGSCRIPT}")
		set(CFGSCRIPT "${_NetCDF_CFGSCRIPT}")
		unset(_NetCDF_CFGSCRIPT CACHE)
	endif()
	if(NOT EXISTS "${CFGSCRIPT}")
		return()
	endif()
	
	execute_process(COMMAND "${CFGSCRIPT}" "--version"
		OUTPUT_VARIABLE NetCDF_VERSION
		RESULT_VARIABLE CFGSCRIPT_RETURN
		OUTPUT_STRIP_TRAILING_WHITESPACE
	)
	if(NOT "${CFGSCRIPT_RETURN}" EQUAL 0)
		message(STATUS "got result: `${CFGSCRIPT_RETURN}'")
		return()
	endif()
	string(REGEX REPLACE "netCDF *([0-9\\.]+).*" "\\1" NetCDF_VERSION "${NetCDF_VERSION}")
	set(NetCDF_VERSION "${NetCDF_VERSION}" PARENT_SCOPE)
	
	set(CFLAGS_OPT       "--cflags")
	set(INCLUDE_DIRECTORIES_OPT "--includedir")
	set(LDFLAGS_OPT      "--libs")
	set(LIBRARY_DIRS_OPT "--libdir")
	foreach(INFO CFLAGS INCLUDE_DIRECTORIES LDFLAGS LIBRARY_DIRS)
		execute_process(COMMAND "${CFGSCRIPT}" "${${INFO}_OPT}"
			OUTPUT_VARIABLE "${INFO}"
			RESULT_VARIABLE CFGSCRIPT_RETURN
			OUTPUT_STRIP_TRAILING_WHITESPACE
		)
		if(NOT "${CFGSCRIPT_RETURN}" EQUAL "0")
			set(NetCDF_FOUND "FALSE" PARENT_SCOPE)
			return()
		endif()
		if(NetCDF_FIND_DEBUG)
			message(STATUS "${CFGSCRIPT} ${${INFO}_OPT} returns ${${INFO}}")
		endif()
	endforeach()
	_NetCDF_target_from_flags("${CFLAGS}" "${INCLUDE_DIRECTORIES}" "" "${LDFLAGS}" "${LIBRARY_DIRS}" "")
	
	
	foreach(FEATURE IN LISTS _NetCDF_features_list)
		string(TOLOWER "--has-${FEATURE}" FEATURE_OPT)
		execute_process(COMMAND "${CFGSCRIPT}" "${FEATURE_OPT}"
			OUTPUT_VARIABLE "FEATURE_${FEATURE}"
			RESULT_VARIABLE CFGSCRIPT_RETURN
			OUTPUT_STRIP_TRAILING_WHITESPACE
		)
		if(NOT "${CFGSCRIPT_RETURN}" EQUAL "0")
			message(WARNING "Error running ${CFGSCRIPT} ${FEATURE_OPT}")
			continue()
		endif()
		if("x${FEATURE_${FEATURE}}" MATCHES "yes")
			list(APPEND NetCDF_FEATURES "${FEATURE}")
			set(NetCDF_FEATURES "${NetCDF_FEATURES}" PARENT_SCOPE)
		endif()
	endforeach()
	
	_NetCDF_public_vars()
endfunction()


function(_NetCDF_find_BASIC)
	find_file(NetCDF_INCLUDE_DIR "netcdf.h")
	mark_as_advanced(NetCDF_INCLUDE_DIR)
	
	_NetCDF_target_from_flags("" "${NetCDF_INCLUDE_DIR}" "" "" "" "netcdf")
	
	_NetCDF_parse_includes(TRUE TRUE)
	_NetCDF_public_vars()
endfunction()

function(_NetCDF_find STRATEGIES)
	if("x${STRATEGIES}x" STREQUAL "xx")
		set(STRATEGIES CMAKE CFGSCRIPT PKGCONFIG FALLBACK)
	endif()
	if("x${NetCDF_CFGSCRIPT}x" STREQUAL "xx")
		set(NetCDF_CFGSCRIPT nc-config)
	endif()
	if(TARGET NetCDF::NetCDF)
		return()
	endif()
	set(NetCDF_FOUND FALSE)
	foreach(STRATEGY IN LISTS STRATEGIES)
		if("CMAKE" STREQUAL "${STRATEGY}")
			set(STRATEGY_NAME "cmake config file")
			message(STATUS "Looking for NetCDF using ${STRATEGY_NAME}")
			_NetCDF_find_CMAKE()
		elseif("CFGSCRIPT" STREQUAL "${STRATEGY}")
			set(STRATEGY_NAME "${NetCDF_CFGSCRIPT}")
			message(STATUS "Looking for NetCDF using ${STRATEGY_NAME}")
			_NetCDF_find_CFGSCRIPT("${NetCDF_CFGSCRIPT}")
		elseif("PKGCONFIG" STREQUAL "${STRATEGY}")
			set(STRATEGY_NAME "pkg-config")
			message(STATUS "Looking for NetCDF using ${STRATEGY_NAME}")
			_NetCDF_find_PKGCONFIG()
		elseif("FALLBACK" STREQUAL "${STRATEGY}")
			set(STRATEGY_NAME "fallback strategy")
			message(STATUS "Looking for NetCDF using ${STRATEGY_NAME}")
			_NetCDF_find_BASIC()
		else()
			message(SEND_ERROR "Invalid strategy to find NetCDF: ${STRATEGY}")
		endif()
		if("${NetCDF_FOUND}")
			message(STATUS "Looking for NetCDF using ${STRATEGY_NAME} -- found")
			break()
		endif()
	endforeach()
	_NetCDF_public_vars()
endfunction()

_NetCDF_find("${NetCDF_FIND_STRATEGIES}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(NetCDF
	REQUIRED_VARS NetCDF_LINK_LIBRARIES
	VERSION_VAR   NetCDF_VERSION
)

if("${NetCDF_FOUND}")
	if("HDF5" IN_LIST NetCDF_FEATURES)
		set(HDF5_USE_STATIC_LIBRARIES OFF)
		if("PARALLEL4" IN_LIST NetCDF_FEATURES)
			set(HDF5_PREFER_PARALLEL ON)
		endif()
		find_package(HDF5 REQUIRED COMPONENTS C)
		if("${HDF5_VERSION}" VERSION_LESS "1.8.0")
			message(ERROR "HDF5 version 1.8.0 at least required by NetCDF, HDF5 ${HDF5_VERSION} found.")
		endif()
		if("PARALLEL4" IN_LIST NetCDF_FEATURES AND NOT "${HDF5_IS_PARALLEL}")
			message(ERROR "Parallel HDF5 required by NetCDF, sequential HDF5 only found.")
		endif()
		list(APPEND NetCDF_LINK_LIBRARIES hdf5::hdf5)
		if(${HDF5_IS_PARALLEL})
			find_package(MPI REQUIRED COMPONENTS C)
			list(APPEND NetCDF_LINK_LIBRARIES MPI::MPI_C)
		endif()
	elseif("PARALLEL4" IN_LIST NetCDF_FEATURES)
		find_package(MPI REQUIRED COMPONENTS C)
		list(APPEND NetCDF_LINK_LIBRARIES MPI::MPI_C)
	endif()

	if(NOT TARGET NetCDF::NetCDF)
		add_library(NetCDF::NetCDF INTERFACE IMPORTED)
		set_target_properties(NetCDF::NetCDF PROPERTIES
			INTERFACE_LINK_LIBRARIES "${NetCDF_LINK_LIBRARIES}"
			INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_INCLUDE_DIRECTORIES}"
			INTERFACE_COMPILE_DEFINITIONS "${NetCDF_COMPILE_DEFINITIONS}"
			INTERFACE_COMPILE_OPTIONS "${NetCDF_COMPILE_OPTIONS}"
		)
	endif()
endif()
