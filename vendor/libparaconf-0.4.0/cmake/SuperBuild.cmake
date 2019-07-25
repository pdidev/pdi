################################################################################
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)

include(GNUInstallDirs)
include(ExternalProject)


### Generate a build command to build a subproject with access to its dependencies
# 
# \param #1 the variable in which to store the result
# \param #2 (optional) the target to build
###
function(__sbuild_build_command _SBUILD_OUTVAR)
	set(_SBUILD_MK_TARGET)
	set(_SBUILD_CM_TARGET)
	if ("${ARGC}" GREATER 1)
		set(_SBUILD_MK_TARGET "${ARGV1}")
		set(_SBUILD_CM_TARGET --target "${ARGV1}")
	endif()
	sbuild_get_env(_SBUILD_ENV_LD_LIBRARY_PATH LD_LIBRARY_PATH)
	set(_SBUILD_RESULT "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}")
	if("${CMAKE_GENERATOR}" MATCHES "Make") #< Use recursive make.
		list(APPEND _SBUILD_RESULT "\$(MAKE)" ${_SBUILD_MK_TARGET})
	else() #< Drive the project with "cmake --build".
		list(APPEND _SBUILD_RESULT "${CMAKE_COMMAND}" --build "." ${_SBUILD_CM_TARGET})
		get_property(_SBUILD_IS_MULTICONFIG GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
		if(_SBUILD_IS_MULTICONFIG)
			list(APPEND _SBUILD_RESULT --config $<CONFIG>)
		endif()
	endif()
	set("${_SBUILD_OUTVAR}" ${_SBUILD_RESULT} PARENT_SCOPE)
endfunction()


### Generate a list of all cache variables to forward to subprojects
# 
# \param #1 the variable in which to store the result
###
function(__sbuild_collect_variables _SBUILD_OUTVAR)
	set(_SBUILD_RESULT)
	
	# append all current variables
	get_cmake_property(_SBUILD_CACHE_VARIABLES CACHE_VARIABLES)
	foreach(_SBUILD_CACHE_VARIABLE ${_SBUILD_CACHE_VARIABLES})
		get_property(_SBUILD_CACHE_VARIABLE_TYPE CACHE "${_SBUILD_CACHE_VARIABLE}" PROPERTY TYPE)
		get_property(_SBUILD_CACHE_VARIABLE_VALUE CACHE "${_SBUILD_CACHE_VARIABLE}" PROPERTY VALUE)
		if(NOT "${_SBUILD_CACHE_VARIABLE_TYPE}" STREQUAL "STATIC"
			AND NOT "${_SBUILD_CACHE_VARIABLE_TYPE}" STREQUAL "INTERNAL")
			list(APPEND _SBUILD_RESULT "-D${_SBUILD_CACHE_VARIABLE}:${_SBUILD_CACHE_VARIABLE_TYPE}=${_SBUILD_CACHE_VARIABLE_VALUE}")
		endif()
	endforeach()
	
	set(_SBUILD_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${CMAKE_BINARY_DIR}/staging" "${CMAKE_BINARY_DIR}/build-dep")
	list(APPEND _SBUILD_RESULT
			#TODO: STAGING_PREFIX is not a very good solution, package may not be relocatable (see python) and it prevents RPATH (replace it by INSTALL_PREFIX)
			# setting INSTALL_PREFIX at install time would be slightly better
			"-DCMAKE_STAGING_PREFIX:PATH=<INSTALL_DIR>"
			"-DCMAKE_PREFIX_PATH:PATH=${_SBUILD_PREFIX_PATH}")
	
	set("${_SBUILD_OUTVAR}" "${_SBUILD_RESULT}" PARENT_SCOPE)
endfunction()


### 
###
function(__sbuild_env_append _SBUILD_VAR _SBUILD_SUBPATH)
	if("xx" STREQUAL "x${${_SBUILD_VAR}}x")
		set("${_SBUILD_VAR}" "${CMAKE_BINARY_DIR}/staging/${_SBUILD_SUBPATH}:${CMAKE_BINARY_DIR}/build-dep/${_SBUILD_SUBPATH}" PARENT_SCOPE)
	else()
		set("${_SBUILD_VAR}" "${CMAKE_BINARY_DIR}/staging/${_SBUILD_SUBPATH}:${CMAKE_BINARY_DIR}/build-dep/${_SBUILD_SUBPATH}:${${_SBUILD_VAR}}" PARENT_SCOPE)
	endif()
endfunction()


### Add a dependency module to the project
# 
###
function(sbuild_add_dependency _SBUILD_NAME _SBUILD_DEFAULT)
	if("${CMAKE_VERSION}" VERSION_LESS "3.7")
		cmake_parse_arguments(_SBUILD "BUILD_DEPENDENCY;NO_INSTALL" "EMBEDDED_PATH;BUILD_IN_SOURCE;VERSION" "CMAKE_CACHE_ARGS;DEPENDS;CONFIGURE_COMMAND;BUILD_COMMAND;INSTALL_COMMAND;COMPONENTS;OPTIONAL_COMPONENTS;PATCH_COMMAND" ${ARGN})
	else()
		cmake_parse_arguments(PARSE_ARGV 2 _SBUILD "BUILD_DEPENDENCY;NO_INSTALL" "EMBEDDED_PATH;BUILD_IN_SOURCE;VERSION" "CMAKE_CACHE_ARGS;DEPENDS;CONFIGURE_COMMAND;BUILD_COMMAND;INSTALL_COMMAND;COMPONENTS;OPTIONAL_COMPONENTS;PATCH_COMMAND")
	endif()
	
	if(DEFINED _SBUILD_EMBEDDED_PATH)
		set("USE_${_SBUILD_NAME}" "${_SBUILD_DEFAULT}" CACHE STRING "version of ${_SBUILD_NAME} to use, this can be 1) a path to the library source, 2) EMBEDDED to use the provided version, 3) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 4) AUTO to use SYSTEM if available and EMBEDDED otherwise")
	else()
		set("USE_${_SBUILD_NAME}" "${_SBUILD_DEFAULT}")
	endif()
	
	set(_SBUILD_TOBUILD FALSE)
	
	get_filename_component(_SBUILD_EMBEDDED_PATH "${_SBUILD_EMBEDDED_PATH}" ABSOLUTE)
	
	if(DEFINED _SBUILD_COMPONENTS)
		set(_SBUILD_COMPONENTS COMPONENTS ${_SBUILD_COMPONENTS})
	endif()
	
	if(DEFINED _SBUILD_OPTIONAL_COMPONENTS)
		set(_SBUILD_COMPONENTS OPTIONAL_COMPONENTS ${_SBUILD_OPTIONAL_COMPONENTS})
	endif()
	
	
	if("${USE_${_SBUILD_NAME}}" STREQUAL SYSTEM)
		# use the preinstalled dep, should be available in the default path
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, found SYSTEM version")
	elseif("${USE_${_SBUILD_NAME}}" STREQUAL EMBEDDED)
		# use the dependency as provided in the distribution
		set(_SBUILD_TOBUILD TRUE)
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, using EMBEDDED version")
	elseif("${USE_${_SBUILD_NAME}}" STREQUAL AUTO)
		# try to behave like SYSTEM, but fallback on EMBEDDED if unavailable
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} QUIET ${_SBUILD_COMPONENTS})
		string(TOUPPER "${_SBUILD_NAME}_FOUND" _SBUILD_IS_FOUND)
		if(NOT "${${_SBUILD_NAME}_FOUND}" AND NOT "${${_SBUILD_IS_FOUND}}")
			set(_SBUILD_TOBUILD TRUE)
			message(STATUS " **Dependency**: ${_SBUILD_NAME} (AUTO), using EMBEDDED version (SYSTEM not found)")
		else()
			find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
			message(STATUS " **Dependency**: ${_SBUILD_NAME} (AUTO), using SYSTEM version")
		endif()
	else()
		# use the provided path as:
		# 1. the path to the source of the library
		# 2. the path to a tarball of the library source
		if(NOT EXISTS "${USE_${_SBUILD_NAME}}")
			message(SEND_ERROR "Invalid path provided for \"${_SBUILD_NAME}\": \"${USE_${_SBUILD_NAME}}\" does not exist")
			return()
		endif()
		set("_SBUILD_EMBEDDED_PATH" "${USE_${_SBUILD_NAME}}")
		
		set(_SBUILD_TOBUILD TRUE)
		message(STATUS " **Dependency**: ${_SBUILD_NAME} (PROVIDED), using PROVIDED version (${_SBUILD_EMBEDDED_PATH})")
	endif()
	
	if(NOT "${_SBUILD_TOBUILD}")
		add_custom_target("${_SBUILD_NAME}_pkg")
		return()
	endif()
	
	if(IS_DIRECTORY "${_SBUILD_EMBEDDED_PATH}")
		set(_SBUILD_PATH_DATA SOURCE_DIR "${_SBUILD_EMBEDDED_PATH}")
	elseif(EXISTS "${_SBUILD_EMBEDDED_PATH}")
		file(MD5 "${_SBUILD_EMBEDDED_PATH}" HASH) #< trick to prevent no-hash warning
		set(_SBUILD_PATH_DATA URL "${_SBUILD_EMBEDDED_PATH}" URL_HASH "MD5=${HASH}")
	else()
		message(SEND_ERROR "Invalid path provided for \"${_SBUILD_NAME}\": \"${_SBUILD_EMBEDDED_PATH}\" does not exist")
		return()
	endif()
	
	__sbuild_collect_variables(_SBUILD_VARS)
	set(_SBUILD_CMAKE_CACHE_ARGS ${_SBUILD_VARS} ${_SBUILD_CMAKE_CACHE_ARGS})
	
	if(NOT DEFINED _SBUILD_BUILD_COMMAND)
		__sbuild_build_command(_SBUILD_BUILD_COMMAND)
	endif()
	
	if(DEFINED _SBUILD_CONFIGURE_COMMAND)
		set(_SBUILD_CONFIGURE_COMMAND CONFIGURE_COMMAND ${_SBUILD_CONFIGURE_COMMAND})
	endif()
	
	if(NOT DEFINED _SBUILD_BUILD_IN_SOURCE)
		set(_SBUILD_BUILD_IN_SOURCE OFF)
	endif()
	
	if(DEFINED _SBUILD_INSTALL_COMMAND OR "${_SBUILD_NO_INSTALL}")
		if("xx" STREQUAL "x${_SBUILD_INSTALL_COMMAND}x")
			set(_SBUILD_INSTALL_COMMAND "INSTALL_COMMAND" "${CMAKE_COMMAND}" "-E" "echo" "No install step for ${_SBUILD_NAME}_pkg")
		else()
			list(INSERT _SBUILD_INSTALL_COMMAND 0 "INSTALL_COMMAND")
		endif()
	endif()
	
	if(DEFINED _SBUILD_PATCH_COMMAND)
		set(_SBUILD_PATCH_COMMAND PATCH_COMMAND "${_SBUILD_PATCH_COMMAND}")
	endif()
	
	set(_SBUILD_STAGE staging)
	if("${_SBUILD_BUILD_DEPENDENCY}")
		set(_SBUILD_STAGE build-dep)
	endif()
	
	unset(_SBUILD_DEPENDS_NEW)
	foreach(_SBUILD_ONE_DEPENDS IN LISTS _SBUILD_DEPENDS)
		list(APPEND _SBUILD_DEPENDS_NEW "${_SBUILD_ONE_DEPENDS}_pkg")
	endforeach()
	set(_SBUILD_DEPENDS "${_SBUILD_DEPENDS_NEW}")
	
	sbuild_get_env(_SBUILD_ENV_LD_LIBRARY_PATH LD_LIBRARY_PATH)
	set(_SBUILD_CMAKE_COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}" "${CMAKE_COMMAND}")
	
	ExternalProject_Add("${_SBUILD_NAME}_pkg"
		CMAKE_COMMAND "${_SBUILD_CMAKE_COMMAND}"
		PREFIX "${CMAKE_BINARY_DIR}/${_SBUILD_NAME}"
		${_SBUILD_PATH_DATA}
		EXCLUDE_FROM_ALL 1
		DEPENDS "${_SBUILD_DEPENDS}"
		${_SBUILD_PATCH_COMMAND}
		${_SBUILD_CONFIGURE_COMMAND}
		CMAKE_CACHE_ARGS "${_SBUILD_CMAKE_CACHE_ARGS}"
		BUILD_COMMAND ${_SBUILD_BUILD_COMMAND}
		BUILD_IN_SOURCE "${_SBUILD_BUILD_IN_SOURCE}"
		INSTALL_DIR "${CMAKE_BINARY_DIR}/${_SBUILD_STAGE}"
		${_SBUILD_INSTALL_COMMAND}
	)
endfunction()


### Add a personal module to the project
# 
###
function(sbuild_add_module _SBUILD_NAME)
	if("${CMAKE_VERSION}" VERSION_LESS "3.7")
		cmake_parse_arguments(_SBUILD "NO_INSTALL" "SOURCE_DIR;ENABLE_BUILD" "CMAKE_CACHE_ARGS;DEPENDS;SUBSTEPS;INSTALL_COMMAND" ${ARGN})
	else()
		cmake_parse_arguments(PARSE_ARGV 1 _SBUILD "NO_INSTALL" "SOURCE_DIR;ENABLE_BUILD" "CMAKE_CACHE_ARGS;DEPENDS;SUBSTEPS;INSTALL_COMMAND")
	endif()
	
	if(DEFINED _SBUILD_ENABLE_BUILD AND NOT "${_SBUILD_ENABLE_BUILD}")
		message(STATUS " **Module**: DISABLED ${_SBUILD_NAME}")
		return()
	else()
		message(STATUS " **Module**: ENABLED  ${_SBUILD_NAME}")
	endif()
	__sbuild_collect_variables(_SBUILD_VARS)
	set(_SBUILD_CMAKE_CACHE_ARGS ${_SBUILD_VARS} ${_SBUILD_CMAKE_CACHE_ARGS})
	
	__sbuild_build_command(_SBUILD_BUILD_COMMAND)
	
	if(DEFINED _SBUILD_INSTALL_COMMAND OR "${_SBUILD_NO_INSTALL}")
		if("xx" STREQUAL "x${_SBUILD_INSTALL_COMMAND}x")
			set(_SBUILD_INSTALL_COMMAND "INSTALL_COMMAND" "${CMAKE_COMMAND}" "-E" "echo" "No install step for ${_SBUILD_NAME}_pkg")
		else()
			list(INSERT _SBUILD_INSTALL_COMMAND 0 "INSTALL_COMMAND")
		endif()
	endif()
	
	sbuild_get_env(_SBUILD_ENV_LD_LIBRARY_PATH LD_LIBRARY_PATH)
	set(_SBUILD_CMAKE_COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}" "${CMAKE_COMMAND}")
	
	unset(_SBUILD_DEPENDS_NEW)
	foreach(_SBUILD_ONE_DEPENDS IN LISTS _SBUILD_DEPENDS)
		list(APPEND _SBUILD_DEPENDS_NEW "${_SBUILD_ONE_DEPENDS}_pkg")
	endforeach()
	set(_SBUILD_DEPENDS "${_SBUILD_DEPENDS_NEW}")
	
	ExternalProject_Add("${_SBUILD_NAME}_pkg"
		CMAKE_COMMAND "${_SBUILD_CMAKE_COMMAND}"
		PREFIX "${CMAKE_BINARY_DIR}/${_SBUILD_NAME}"
		SOURCE_DIR "${_SBUILD_SOURCE_DIR}"
		BUILD_ALWAYS TRUE
		DEPENDS "${_SBUILD_DEPENDS}"
		CMAKE_CACHE_ARGS "${_SBUILD_CMAKE_CACHE_ARGS}"
		BUILD_COMMAND ${_SBUILD_BUILD_COMMAND}
		INSTALL_DIR "${CMAKE_BINARY_DIR}/staging"
		${_SBUILD_INSTALL_COMMAND}
	)
	if(DEFINED _SBUILD_SUBSTEPS)
		ExternalProject_Add_StepTargets("${_SBUILD_NAME}_pkg" "configure")
		ExternalProject_Get_Property("${_SBUILD_NAME}_pkg" BINARY_DIR)
	endif()
	
	foreach(_SBUILD_ST_NAME ${_SBUILD_SUBSTEPS})
		if("${_SBUILD_ST_NAME}" STREQUAL "test" AND "${BUILD_TESTING}")
			file(APPEND "${CMAKE_BINARY_DIR}/SubTests.cmake" "subdirs([=[${BINARY_DIR}]=])\n")
			continue()
		elseif(TARGET "${_SBUILD_ST_NAME}")
			__sbuild_build_command(_SBUILD_BUILD_COMMAND "${_SBUILD_ST_NAME}")
			add_custom_command(TARGET "${_SBUILD_ST_NAME}" POST_BUILD
				COMMAND ${_SBUILD_BUILD_COMMAND}
				WORKING_DIRECTORY "${BINARY_DIR}"
				COMMENT "Doing ${_SBUILD_ST_NAME} for '${_SBUILD_NAME}_pkg'")
			add_dependencies("${_SBUILD_ST_NAME}" "${_SBUILD_NAME}_pkg-configure")
		endif()
	endforeach()
endfunction()


###
#
###
function(sbuild_get_env _SBUILD_VAR _SBUILD_ENV_NAME)
	set(_SBUILD_ENV_VAL "$ENV{${_SBUILD_ENV_NAME}}")
	if("LD_LIBRARY_PATH" STREQUAL "${_SBUILD_ENV_NAME}" OR "LIBRARY_PATH" STREQUAL "${_SBUILD_ENV_NAME}")
		__sbuild_env_append(_SBUILD_ENV_VAL "${CMAKE_INSTALL_LIBDIR}")
		__sbuild_env_append(_SBUILD_ENV_VAL "lib")
	elseif("CPATH" STREQUAL "${_SBUILD_ENV_NAME}")
		__sbuild_env_append(_SBUILD_ENV_VAL "${CMAKE_INSTALL_INCLUDEDIR}")
	else()
		message(FATAL_ERROR "sbuild_get_env called with unsupported Environment variable name: `${_SBUILD_ENV_NAME}'")
	endif()
	set("${_SBUILD_VAR}" "${_SBUILD_ENV_VAL}" PARENT_SCOPE)
endfunction()



## Testing handling

if("${BUILD_TESTING}")
	enable_testing()
	if("${CMAKE_VERSION}" VERSION_LESS 3.10)
		set_property(DIRECTORY "${CMAKE_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_BINARY_DIR}/SubTests.cmake")
	else()
		set_property(DIRECTORY "${CMAKE_SOURCE_DIR}" APPEND PROPERTY TEST_INCLUDE_FILES "${CMAKE_BINARY_DIR}/SubTests.cmake")
	endif()
	file(WRITE "${CMAKE_BINARY_DIR}/SubTests.cmake"
	"set(ADDPATH [=[${CMAKE_BINARY_DIR}/staging/${CMAKE_INSTALL_LIBDIR}:${CMAKE_BINARY_DIR}/staging/lib]=])\n"
	[===[
set(LD_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
if("x${LD_LIBRARY_PATH}x" STREQUAL xx)
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}")
else()
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}:${LD_LIBRARY_PATH}")
endif()
]===]
	)
endif()


## Installation

install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/staging/" DESTINATION "." USE_SOURCE_PERMISSIONS)
