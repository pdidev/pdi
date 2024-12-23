################################################################################
# Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.16...3.25)


### Add a dependency module to the project
# 
###
macro(sbuild_add_dependency _SBUILD_arg_NAME _SBUILD_arg_DEFAULT)
	cmake_parse_arguments(_SBUILD_arg "" "SOURCE_DIR;VERSION" "FIND_PACKAGE_ARGS" ${ARGN})
	if(NOT DEFINED _SBUILD_arg_SOURCE_DIR)
		message(FATAL_ERROR "sbuild_add_dependency: SOURCE_DIR has to be set for \"${_SBUILD_arg_NAME}\"")
	endif()
	get_filename_component(_SBUILD_arg_SOURCE_DIR "${_SBUILD_arg_SOURCE_DIR}" ABSOLUTE BASE_DIR "${CMAKE_SOURCE_DIR}")
	if(NOT IS_DIRECTORY "${_SBUILD_arg_SOURCE_DIR}")
		message(FATAL_ERROR "sbuild_add_dependency: Invalid path provided for \"${_SBUILD_arg_NAME}\": \"${_SBUILD_arg_SOURCE_DIR}\" is not a directory")
	endif()
	
	
	set("USE_${_SBUILD_arg_NAME}" "${_SBUILD_arg_DEFAULT}" CACHE STRING "version of ${_SBUILD_arg_NAME} to use, this can be 1) EMBEDDED to use the provided version, 2) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 3) AUTO to use SYSTEM if available and EMBEDDED otherwise")
	
	
	if("${USE_${_SBUILD_arg_NAME}}" STREQUAL SYSTEM)
		# use the preinstalled dep, should be available in the default path
		message(STATUS " **Dependency**: ${_SBUILD_arg_NAME}, using SYSTEM version (-DUSE_${_SBUILD_arg_NAME}=SYSTEM)")
		find_package("${_SBUILD_arg_NAME}" "${_SBUILD_arg_VERSION}" REQUIRED ${_SBUILD_arg_FIND_PACKAGE_ARGS})
	elseif("${USE_${_SBUILD_arg_NAME}}" STREQUAL EMBEDDED)
		# use the dependency as provided in the distribution
		message(STATUS " **Dependency**: ${_SBUILD_arg_NAME}, using EMBEDDED version (-DUSE_${_SBUILD_arg_NAME}=EMBEDDED)")
		add_subdirectory("${_SBUILD_arg_SOURCE_DIR}" EXCLUDE_FROM_ALL)
	elseif("${USE_${_SBUILD_arg_NAME}}" STREQUAL AUTO)
		# try to behave like SYSTEM, but fallback on EMBEDDED if unavailable
		find_package("${_SBUILD_arg_NAME}" ${_SBUILD_arg_VERSION} QUIET ${_SBUILD_arg_FIND_PACKAGE_ARGS})
		string(TOUPPER "${_SBUILD_arg_NAME}_FOUND" _SBUILD_arg_IS_FOUND)
		if(NOT "${${_SBUILD_arg_NAME}_FOUND}" AND NOT "${${_SBUILD_arg_IS_FOUND}}")
			set(_SBUILD_arg_TOBUILD TRUE)
			set(_SBUILD_arg_VERSION_MSG)
			if(DEFINED _SBUILD_arg_VERSION)
				set(_SBUILD_arg_VERSION_MSG " in version \"${_SBUILD_arg_VERSION}\"")
			endif()
			message(STATUS " **Dependency**: ${_SBUILD_arg_NAME} using EMBEDDED version (SYSTEM not found${_SBUILD_arg_VERSION_MSG}) (-DUSE_${_SBUILD_arg_NAME}=AUTO)")
			add_subdirectory("${_SBUILD_arg_SOURCE_DIR}" EXCLUDE_FROM_ALL)
		else()
			message(STATUS " **Dependency**: ${_SBUILD_arg_NAME} found and using SYSTEM version (-DUSE_${_SBUILD_arg_NAME}=AUTO)")
			find_package("${_SBUILD_arg_NAME}" "${_SBUILD_arg_VERSION}" REQUIRED ${_SBUILD_arg_FIND_PACKAGE_ARGS})
		endif()
	else()
		message(FATAL_ERROR "sbuild_add_dependency: Invalid value for USE_${_SBUILD_arg_NAME}, `${USE_${_SBUILD_arg_NAME}}' is not one of AUTO, EMBEDDED or SYSTEM")
	endif()
endmacro()


### Add a personal module to the project
# 
###
macro(sbuild_add_module _SBUILD_arg_NAME)
	cmake_parse_arguments(_SBUILD_arg "" "SOURCE_DIR;ENABLE_BUILD_FLAG" "" ${ARGN})
	if(NOT DEFINED _SBUILD_arg_SOURCE_DIR)
		message(FATAL_ERROR "sbuild_add_dependency requires SOURCE_DIR to be set for \"${_SBUILD_arg_NAME}\"")
	endif()
	get_filename_component(_SBUILD_arg_SOURCE_DIR "${_SBUILD_arg_SOURCE_DIR}" ABSOLUTE BASE_DIR "${CMAKE_SOURCE_DIR}")
	if(NOT IS_DIRECTORY "${_SBUILD_arg_SOURCE_DIR}")
		message(FATAL_ERROR "sbuild_add_dependency: Invalid path provided for \"${_SBUILD_arg_NAME}\": \"${_SBUILD_arg_SOURCE_DIR}\" is not a directory")
	endif()
	if(NOT DEFINED _SBUILD_arg_ENABLE_BUILD_FLAG)
		set(_SBUILD_arg_ENABLE_BUILD_FLAG "${BUILD_${_SBUILD_arg_NAME}}")
	endif()
	
	
	if("${${_SBUILD_arg_ENABLE_BUILD_FLAG}}")
		message(STATUS " **Module**: ENABLED  ${_SBUILD_arg_NAME} (-D${_SBUILD_arg_ENABLE_BUILD_FLAG}=ON)")
		add_subdirectory("${_SBUILD_arg_SOURCE_DIR}")
	else()
		message(STATUS " **Module**: DISABLED ${_SBUILD_arg_NAME} (-D${_SBUILD_arg_ENABLE_BUILD_FLAG}=OFF)")
	endif()
	
endmacro()
