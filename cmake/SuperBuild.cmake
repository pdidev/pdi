################################################################################
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

cmake_minimum_required(VERSION 3.22...4.2)

include(GNUInstallDirs)
include(ExternalProject)

# Where the dependencies are installed while the distribution is built, and copied from on install.
set(_SBUILD_STAGING "${CMAKE_BINARY_DIR}/staging")

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
	sbuild_get_env(_SBUILD_ENV_DYLD_LIBRARY_PATH DYLD_LIBRARY_PATH)
	set(_SBUILD_RESULT "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}" "DYLD_LIBRARY_PATH=${_SBUILD_ENV_DYLD_LIBRARY_PATH}")
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


### Generate a list of all cache variables to forward to a subproject
#
# The cache as it stands, plus the prefix path the staged dependencies live at.
# This is what every subproject wants, PDI included; _sbuild_dependency_policy() below holds what
# only a vendored dependency wants.
#
# \param #1 the variable in which to store the result
###
function(_sbuild_collect_variables _SBUILD_OUTVAR)
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
	
	set(_SBUILD_PREFIX_PATH ${CMAKE_PREFIX_PATH} "${_SBUILD_STAGING}")
	list(APPEND _SBUILD_RESULT "-DCMAKE_PREFIX_PATH:PATH=${_SBUILD_PREFIX_PATH}")
	
	set("${_SBUILD_OUTVAR}" "${_SBUILD_RESULT}" PARENT_SCOPE)
endfunction()


### Generate the settings that apply to a vendored dependency and to nothing else
#
# These are policy rather than a sweep of what the user asked for, and PDI itself wants none of
# them: it installs to the final prefix rather than to a staging tree, and it sets its own
# BUILD_SHARED_LIBS.  Keeping them apart is what saves the PDI call site from having to undo them
# afterwards.
#
# \param #1 the variable to append the result to
###
function(_sbuild_dependency_policy _SBUILD_OUTVAR)
	set(_SBUILD_RESULT "${${_SBUILD_OUTVAR}}")
	
	list(APPEND _SBUILD_RESULT
			# CMAKE_STAGING_PREFIX makes CMake bake CMAKE_INSTALL_PREFIX into what it generates while
			# writing the files under <INSTALL_DIR>, which is what lets a dependency be used from the
			# staging tree during the build and still be correct once copied to the final prefix.
			# The price is that RPATHs point at the final prefix rather than at the staging tree, so
			# a staged dependency is only loadable from where it will eventually live.
			# PDI itself no longer pays it -- see sbuild_add_self() -- but a dependency has two
			# locations to satisfy and one configure cannot bake both.
			# Fixing that means re-configuring each dependency with the final prefix at install time;
			# it is also what keeps NetCDF from being relocatable, see
			# https://github.com/pdidev/pdi/issues/644
			# <INSTALL_DIR> is an ExternalProject placeholder, and means nothing anywhere else.
			"-DCMAKE_STAGING_PREFIX:PATH=<INSTALL_DIR>"
			# PDI is only ever linked against shared, position-independent dependencies; neither
			# setting reaches them from the cache sweep, so both are stated here.
			"-DBUILD_SHARED_LIBS:BOOL=ON"
			"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON")
	
	set("${_SBUILD_OUTVAR}" "${_SBUILD_RESULT}" PARENT_SCOPE)
endfunction()


### 
###
function(__sbuild_env_append _SBUILD_VAR _SBUILD_SUBPATH)
	if("xx" STREQUAL "x${${_SBUILD_VAR}}x")
		set("${_SBUILD_VAR}" "${CMAKE_BINARY_DIR}/staging/${_SBUILD_SUBPATH}" PARENT_SCOPE)
	else()
		set("${_SBUILD_VAR}" "${CMAKE_BINARY_DIR}/staging/${_SBUILD_SUBPATH}:${${_SBUILD_VAR}}" PARENT_SCOPE)
	endif()
endfunction()


### Add a dependency, either found on the system or built into the staging tree
#
# \param #1 the name of the dependency, as find_package() knows it
# \param #2 the default of USE_<name>
# \param EMBEDDED_PATH the source directory or tarball of the copy shipped with the distribution
# \param VERSION (optional) the minimum version of a system copy
# \param COMPONENTS (optional) the components to find on the system
# \param MODULE_VARS (optional) the variables set by find_package() to forward to the caller
# \param DEPENDS (optional) the dependencies to build this one after
# \param CMAKE_CACHE_ARGS (optional) additional settings to build the shipped copy with
###
function(sbuild_add_dependency _SBUILD_NAME _SBUILD_DEFAULT)
	cmake_parse_arguments(PARSE_ARGV 2 _SBUILD "" "EMBEDDED_PATH;VERSION" "CMAKE_CACHE_ARGS;COMPONENTS;DEPENDS;MODULE_VARS")
	
	if(NOT DEFINED _SBUILD_EMBEDDED_PATH)
		message(FATAL_ERROR "sbuild_add_dependency(${_SBUILD_NAME}) requires an EMBEDDED_PATH")
	endif()
	set("USE_${_SBUILD_NAME}" "${_SBUILD_DEFAULT}" CACHE STRING "version of ${_SBUILD_NAME} to use, this can be 1) a path to the library source, 2) EMBEDDED to use the provided version, 3) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 4) AUTO to use SYSTEM if available and EMBEDDED otherwise")
	
	set(_SBUILD_TOBUILD FALSE)
	
	get_filename_component(_SBUILD_EMBEDDED_PATH "${_SBUILD_EMBEDDED_PATH}" ABSOLUTE)
	
	if(DEFINED _SBUILD_COMPONENTS)
		set(_SBUILD_COMPONENTS COMPONENTS ${_SBUILD_COMPONENTS})
	endif()
	
	
	if("${USE_${_SBUILD_NAME}}" STREQUAL SYSTEM)
		# use the preinstalled dep, should be available in the default path
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, using SYSTEM version (-DUSE_${_SBUILD_NAME}=${USE_${_SBUILD_NAME}})")
	elseif("${USE_${_SBUILD_NAME}}" STREQUAL EMBEDDED)
		# use the dependency as provided in the distribution
		set(_SBUILD_TOBUILD TRUE)
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, using EMBEDDED version (-DUSE_${_SBUILD_NAME}=${USE_${_SBUILD_NAME}})")
	elseif("${USE_${_SBUILD_NAME}}" STREQUAL AUTO)
		# try to behave like SYSTEM, but fallback on EMBEDDED if unavailable
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} QUIET ${_SBUILD_COMPONENTS})
		string(TOUPPER "${_SBUILD_NAME}_FOUND" _SBUILD_IS_FOUND)
		if(NOT "${${_SBUILD_NAME}_FOUND}" AND NOT "${${_SBUILD_IS_FOUND}}")
			set(_SBUILD_TOBUILD TRUE)
			set(_SBUILD_VERSION_MSG)
			if(DEFINED _SBUILD_VERSION)
				set(_SBUILD_VERSION_MSG " in version \"${_SBUILD_VERSION}\"")
			endif()
			message(STATUS " **Dependency**: ${_SBUILD_NAME} using EMBEDDED version (SYSTEM not found${_SBUILD_VERSION_MSG}) (-DUSE_${_SBUILD_NAME}=${USE_${_SBUILD_NAME}})")
		else()
			find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
			message(STATUS " **Dependency**: ${_SBUILD_NAME} found and using SYSTEM version (-DUSE_${_SBUILD_NAME}=${USE_${_SBUILD_NAME}})")
		endif()
	else()
		# use the provided path as:
		# 1. the path to the source of the library
		# 2. the path to a tarball of the library source
		set("_SBUILD_EMBEDDED_PATH" "${USE_${_SBUILD_NAME}}")
	
		set(_SBUILD_TOBUILD TRUE)
		message(STATUS " **Dependency**: ${_SBUILD_NAME} (PROVIDED), using PROVIDED version (${_SBUILD_EMBEDDED_PATH})")
	endif()
	
	if(NOT "${_SBUILD_TOBUILD}")
		add_custom_target("${_SBUILD_NAME}_pkg")
		set_property(GLOBAL APPEND PROPERTY _SBUILD_DEPENDENCY_TARGETS "${_SBUILD_NAME}_pkg")
		set("${_SBUILD_NAME}_FOUND" TRUE PARENT_SCOPE)
		foreach(_SBUILD_VAR ${_SBUILD_MODULE_VARS})
			set("${_SBUILD_VAR}" "${${_SBUILD_VAR}}" PARENT_SCOPE)
		endforeach()
		return()
	endif()
	set("${_SBUILD_NAME}_FOUND" FALSE PARENT_SCOPE)
	
	if(IS_DIRECTORY "${_SBUILD_EMBEDDED_PATH}")
		set(_SBUILD_PATH_DATA SOURCE_DIR "${_SBUILD_EMBEDDED_PATH}")
	elseif(EXISTS "${_SBUILD_EMBEDDED_PATH}")
		file(MD5 "${_SBUILD_EMBEDDED_PATH}" HASH) #< trick to prevent no-hash warning
		set(_SBUILD_PATH_DATA URL "${_SBUILD_EMBEDDED_PATH}" URL_HASH "MD5=${HASH}")
	else()
		message(SEND_ERROR "Invalid path provided for \"${_SBUILD_NAME}\": \"${_SBUILD_EMBEDDED_PATH}\" does not exist")
		return()
	endif()
	
	_sbuild_collect_variables(_SBUILD_VARS)
	_sbuild_dependency_policy(_SBUILD_VARS)
	set(_SBUILD_CMAKE_CACHE_ARGS ${_SBUILD_VARS} ${_SBUILD_CMAKE_CACHE_ARGS})
	
	__sbuild_build_command(_SBUILD_BUILD_COMMAND)
	
	unset(_SBUILD_DEPENDS_NEW)
	foreach(_SBUILD_ONE_DEPENDS IN LISTS _SBUILD_DEPENDS)
		list(APPEND _SBUILD_DEPENDS_NEW "${_SBUILD_ONE_DEPENDS}_pkg")
	endforeach()
	set(_SBUILD_DEPENDS "${_SBUILD_DEPENDS_NEW}")
	
	sbuild_get_env(_SBUILD_ENV_LD_LIBRARY_PATH LD_LIBRARY_PATH)
	sbuild_get_env(_SBUILD_ENV_DYLD_LIBRARY_PATH DYLD_LIBRARY_PATH)
	set(_SBUILD_CMAKE_COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}" "DYLD_LIBRARY_PATH=${_SBUILD_ENV_DYLD_LIBRARY_PATH}" "${CMAKE_COMMAND}")
	
	ExternalProject_Add("${_SBUILD_NAME}_pkg"
		CMAKE_COMMAND "${_SBUILD_CMAKE_COMMAND}"
		PREFIX "${CMAKE_BINARY_DIR}/${_SBUILD_NAME}"
		${_SBUILD_PATH_DATA}
		EXCLUDE_FROM_ALL 1
		DEPENDS "${_SBUILD_DEPENDS}"
		CMAKE_CACHE_ARGS "${_SBUILD_CMAKE_CACHE_ARGS}"
		BUILD_COMMAND ${_SBUILD_BUILD_COMMAND}
		INSTALL_DIR "${_SBUILD_STAGING}"
	)
	set_property(GLOBAL APPEND PROPERTY _SBUILD_DEPENDENCY_TARGETS "${_SBUILD_NAME}_pkg")
endfunction()


### Add this very project as a sub-project, configured with the superbuild off
#
# The sub-project is built after every dependency added before the call, so call it last.
# It is installed by our own install step, and its tests are run by our own ctest.
###
function(sbuild_add_self)
	_sbuild_collect_variables(_SBUILD_CACHE_ARGS)
	list(APPEND _SBUILD_CACHE_ARGS "-DPDI_SUPERBUILD:BOOL=OFF")
	get_property(_SBUILD_DEPENDS GLOBAL PROPERTY _SBUILD_DEPENDENCY_TARGETS)
	
	__sbuild_build_command(_SBUILD_BUILD_COMMAND)
	sbuild_get_env(_SBUILD_ENV_LD_LIBRARY_PATH LD_LIBRARY_PATH)
	sbuild_get_env(_SBUILD_ENV_DYLD_LIBRARY_PATH DYLD_LIBRARY_PATH)
	set(_SBUILD_CMAKE_COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${_SBUILD_ENV_LD_LIBRARY_PATH}" "DYLD_LIBRARY_PATH=${_SBUILD_ENV_DYLD_LIBRARY_PATH}" "${CMAKE_COMMAND}")
	
	ExternalProject_Add("${PROJECT_NAME}"
		CMAKE_COMMAND "${_SBUILD_CMAKE_COMMAND}"
		PREFIX "${CMAKE_BINARY_DIR}/${PROJECT_NAME}"
		SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}"
		BUILD_ALWAYS TRUE
		DEPENDS "${_SBUILD_DEPENDS}"
		CMAKE_CACHE_ARGS "${_SBUILD_CACHE_ARGS}"
		BUILD_COMMAND ${_SBUILD_BUILD_COMMAND}
		# Building must not install anything: this is done in the SuperBuild install phase below.
		INSTALL_COMMAND ""
	)
	ExternalProject_Get_Property("${PROJECT_NAME}" BINARY_DIR)
	
	# Run the sub-project's install script from ours.
	install(SCRIPT "${BINARY_DIR}/cmake_install.cmake")
	
	# Run the sub-project's tests from ours; this does nothing unless the caller enabled testing.
	set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" APPEND PROPERTY TEST_INCLUDE_FILES "${CMAKE_BINARY_DIR}/${PROJECT_NAME}Tests.cmake")
	file(WRITE "${CMAKE_BINARY_DIR}/${PROJECT_NAME}Tests.cmake" "subdirs([=[${BINARY_DIR}]=])\n")
endfunction()


###
#
###
function(sbuild_get_env _SBUILD_VAR _SBUILD_ENV_NAME)
	set(_SBUILD_ENV_VAL "$ENV{${_SBUILD_ENV_NAME}}")
	if("LD_LIBRARY_PATH" STREQUAL "${_SBUILD_ENV_NAME}" OR "DYLD_LIBRARY_PATH" STREQUAL "${_SBUILD_ENV_NAME}" OR "LIBRARY_PATH" STREQUAL "${_SBUILD_ENV_NAME}")
		__sbuild_env_append(_SBUILD_ENV_VAL "${CMAKE_INSTALL_LIBDIR}")
		__sbuild_env_append(_SBUILD_ENV_VAL "lib")
	elseif("CPATH" STREQUAL "${_SBUILD_ENV_NAME}")
		__sbuild_env_append(_SBUILD_ENV_VAL "${CMAKE_INSTALL_INCLUDEDIR}")
	elseif("PATH" STREQUAL "${_SBUILD_ENV_NAME}")
		__sbuild_env_append(_SBUILD_ENV_VAL "${CMAKE_INSTALL_BINDIR}")
	else()
		message(FATAL_ERROR "sbuild_get_env called with unsupported Environment variable name: `${_SBUILD_ENV_NAME}'")
	endif()
	set("${_SBUILD_VAR}" "${_SBUILD_ENV_VAL}" PARENT_SCOPE)
endfunction()



## Testing handling

if("${BUILD_TESTING}")
	enable_testing()
	set_property(DIRECTORY "${CMAKE_SOURCE_DIR}" APPEND PROPERTY TEST_INCLUDE_FILES "${CMAKE_BINARY_DIR}/SubTests.cmake")
	file(WRITE "${CMAKE_BINARY_DIR}/SubTests.cmake"
	"set(ADDPATH [=[${CMAKE_BINARY_DIR}/staging/${CMAKE_INSTALL_LIBDIR}:${CMAKE_BINARY_DIR}/staging/lib]=])\n"
	[===[
set(LD_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
if("x${LD_LIBRARY_PATH}x" STREQUAL xx)
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}")
else()
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}:${LD_LIBRARY_PATH}")
endif()
set(DYLD_LIBRARY_PATH "$ENV{DYLD_LIBRARY_PATH}")
if("x${DYLD_LIBRARY_PATH}x" STREQUAL xx)
	set(ENV{DYLD_LIBRARY_PATH} "${ADDPATH}")
else()
	set(ENV{DYLD_LIBRARY_PATH} "${ADDPATH}:${DYLD_LIBRARY_PATH}")
endif()
]===]
	)
endif()


## Installation

# The staging tree only fills up with the dependencies that are built, so create it for when none is.
file(MAKE_DIRECTORY "${_SBUILD_STAGING}")
install(DIRECTORY "${_SBUILD_STAGING}/" DESTINATION "." USE_SOURCE_PERMISSIONS)
