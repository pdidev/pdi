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

# This file is included twice: from the project at configure time, and from the install script, which
# only wants sbuild_strip_staging_rpath().  The functions it needs come first and are script-safe;
# everything else follows a check of the role CMake runs in.
# A function runs under the policies in force where it is defined, so this comes before all of them.
cmake_minimum_required(VERSION 3.22...4.2)

include_guard(GLOBAL)


## Install time
#
# A vendored dependency is used out of the staging tree while the distribution builds, so it carries runtime path entries naming that tree: see where
# _sbuild_dependency_policy() injects them.  Once the tree is copied to the final prefix they point into a build directory that may be gone, so every
# entry containing the staging directory is removed, and every other one is kept in its original order: whatever the user asked for, whatever the
# toolchain added.
#
# ELF is rewritten with file(RPATH_SET) from the whole filtered value.  file(RPATH_CHANGE) is not used to drop a single entry: it replaces only the
# matched text and leaves the separators around it, and an empty entry makes the dynamic loader search the current working directory.  Empty entries
# are dropped as well in a file that has to be rewritten anyway, for that same reason.
# The rewrite is in place.  That is only safe because the injected value ends with an entry ending in "/", which no symbol name can share the tail of.
#
# Mach-O keeps one LC_RPATH load command per entry, so each matching one is deleted with install_name_tool, as CMake's own install rules do.

### Remove the runtime path entries naming the staging tree from one ELF file
#
# \param #1 the file
# \param #2 the text an entry has to contain to be removed
###
function(_sbuild_strip_elf_rpath _SBUILD_FILE _SBUILD_NEEDLE)
	# Only an ELF file is worth a file(READ_ELF): up to at least CMake 3.22 it fails outright on any other file, CAPTURE_ERROR or not.
	file(READ "${_SBUILD_FILE}" _SBUILD_MAGIC LIMIT 4 HEX)
	if(NOT "7f454c46" STREQUAL "${_SBUILD_MAGIC}")
		return()
	endif()

	# file(READ_ELF) sets its outputs only when it has something to report
	unset(_SBUILD_ERROR)
	unset(_SBUILD_RPATH)
	unset(_SBUILD_RUNPATH)
	file(READ_ELF "${_SBUILD_FILE}" CAPTURE_ERROR _SBUILD_ERROR RPATH _SBUILD_RPATH RUNPATH _SBUILD_RUNPATH)
	if(DEFINED _SBUILD_ERROR)
		return() #< an ELF file CMake cannot parse
	endif()
	if(DEFINED _SBUILD_RUNPATH)
		set(_SBUILD_CURRENT "${_SBUILD_RUNPATH}")
		if(DEFINED _SBUILD_RPATH AND NOT "${_SBUILD_RPATH}" STREQUAL "${_SBUILD_RUNPATH}")
			# file(RPATH_SET) writes the same value to both
			message(WARNING "Not removing the staging runtime path from \"${_SBUILD_FILE}\": its RPATH and RUNPATH differ")
			return()
		endif()
	elseif(DEFINED _SBUILD_RPATH)
		set(_SBUILD_CURRENT "${_SBUILD_RPATH}")
	else()
		return()
	endif()

	# file(READ_ELF) returns the entries as a list, empty ones included
	set(_SBUILD_KEPT)
	set(_SBUILD_FOUND FALSE)
	foreach(_SBUILD_ENTRY IN LISTS _SBUILD_CURRENT)
		string(FIND "${_SBUILD_ENTRY}" "${_SBUILD_NEEDLE}" _SBUILD_AT)
		if(NOT "${_SBUILD_AT}" EQUAL -1)
			set(_SBUILD_FOUND TRUE)
		elseif(NOT "${_SBUILD_ENTRY}" STREQUAL "")
			list(APPEND _SBUILD_KEPT "${_SBUILD_ENTRY}")
		endif()
	endforeach()
	if(NOT "${_SBUILD_FOUND}")
		return()
	endif()

	# an empty value removes the entry altogether
	list(JOIN _SBUILD_KEPT ":" _SBUILD_NEW)
	file(RPATH_SET FILE "${_SBUILD_FILE}" NEW_RPATH "${_SBUILD_NEW}")
endfunction()


### Remove the runtime path entries naming the staging tree from one Mach-O file
#
# \param #1 the file
# \param #2 the text an entry has to contain to be removed
# \param #3 the otool program
# \param #4 the install_name_tool program
###
function(_sbuild_strip_macho_rpath _SBUILD_FILE _SBUILD_NEEDLE _SBUILD_OTOOL _SBUILD_INSTALL_NAME_TOOL)
	# Spare an otool run on every header and data file: only a Mach-O or universal binary is worth one.
	file(READ "${_SBUILD_FILE}" _SBUILD_MAGIC LIMIT 4 HEX)
	if(NOT "${_SBUILD_MAGIC}" MATCHES "^(feedface|cefaedfe|feedfacf|cffaedfe|cafebabe|bebafeca)$")
		return()
	endif()

	execute_process(COMMAND "${_SBUILD_OTOOL}" -l "${_SBUILD_FILE}"
		OUTPUT_VARIABLE _SBUILD_LOAD_COMMANDS
		ERROR_QUIET
		RESULT_VARIABLE _SBUILD_RESULT)
	if(NOT "${_SBUILD_RESULT}" EQUAL 0)
		return()
	endif()

	# An LC_RPATH load command prints as "cmd LC_RPATH", "cmdsize <n>", then "path <entry> (offset <n>)".
	string(REPLACE "\n" ";" _SBUILD_LINES "${_SBUILD_LOAD_COMMANDS}")
	set(_SBUILD_IN_RPATH FALSE)
	set(_SBUILD_ARGS)
	foreach(_SBUILD_LINE IN LISTS _SBUILD_LINES)
		if("${_SBUILD_LINE}" MATCHES "^ *cmd ")
			set(_SBUILD_IN_RPATH FALSE)
			if("${_SBUILD_LINE}" MATCHES "^ *cmd LC_RPATH$")
				set(_SBUILD_IN_RPATH TRUE)
			endif()
		elseif("${_SBUILD_IN_RPATH}" AND "${_SBUILD_LINE}" MATCHES "^ *path (.*) \\(offset [0-9]+\\)$")
			set(_SBUILD_ENTRY "${CMAKE_MATCH_1}")
			string(FIND "${_SBUILD_ENTRY}" "${_SBUILD_NEEDLE}" _SBUILD_AT)
			if(NOT "${_SBUILD_AT}" EQUAL -1)
				list(APPEND _SBUILD_ARGS -delete_rpath "${_SBUILD_ENTRY}")
			endif()
		endif()
	endforeach()
	if("${_SBUILD_ARGS}" STREQUAL "")
		return()
	endif()

	# install_name_tool re-applies the linker's ad hoc signature itself, which CMake's own install rules rely on too.
	execute_process(COMMAND "${_SBUILD_INSTALL_NAME_TOOL}" ${_SBUILD_ARGS} "${_SBUILD_FILE}" COMMAND_ERROR_IS_FATAL ANY)
	message(STATUS "Removed staging runtime path from \"${_SBUILD_FILE}\"")
endfunction()


### Remove the runtime path entries naming the staging tree from everything installed out of it
#
# \param #1 the binary format: ELF, MACHO or NONE
# \param #2 the staging directory the files were copied from
# \param #3 the directory they were copied to, DESTDIR included
# \param #4 (MACHO only) the otool program
# \param #5 (MACHO only) the install_name_tool program
###
function(sbuild_strip_staging_rpath _SBUILD_FORMAT _SBUILD_STAGING _SBUILD_DESTINATION)
	if("${_SBUILD_FORMAT}" STREQUAL "NONE")
		return()
	endif()
	if("${_SBUILD_FORMAT}" STREQUAL "MACHO")
		if("${ARGC}" LESS 5 OR "${ARGV3}" STREQUAL "" OR "${ARGV4}" STREQUAL "")
			message(WARNING "otool or install_name_tool not found, the installed dependencies keep runtime path entries into \"${_SBUILD_STAGING}\"")
			return()
		endif()
	endif()

	# Only what the copy put there: the destination may be a shared prefix holding other files.
	file(GLOB_RECURSE _SBUILD_FILES LIST_DIRECTORIES false RELATIVE "${_SBUILD_STAGING}" "${_SBUILD_STAGING}/*")
	foreach(_SBUILD_FILE IN LISTS _SBUILD_FILES)
		set(_SBUILD_FILE "${_SBUILD_DESTINATION}/${_SBUILD_FILE}")
		# a link is visited through the file it points to
		if(IS_SYMLINK "${_SBUILD_FILE}" OR NOT EXISTS "${_SBUILD_FILE}")
			continue()
		endif()
		if("${_SBUILD_FORMAT}" STREQUAL "MACHO")
			_sbuild_strip_macho_rpath("${_SBUILD_FILE}" "${_SBUILD_STAGING}/" "${ARGV3}" "${ARGV4}")
		else()
			_sbuild_strip_elf_rpath("${_SBUILD_FILE}" "${_SBUILD_STAGING}/")
		endif()
	endforeach()
endfunction()


# The install script stops here: nothing below can run outside a project.
get_property(_SBUILD_ROLE GLOBAL PROPERTY CMAKE_ROLE)
if(NOT "${_SBUILD_ROLE}" STREQUAL "PROJECT")
	return()
endif()


## Configure time

include(GNUInstallDirs)
include("${CMAKE_CURRENT_LIST_DIR}/PDIOptions.cmake")
include(ExternalProject)

# Where the dependencies are installed while the distribution is built, and copied from on install.
set(_SBUILD_STAGING "${CMAKE_BINARY_DIR}/staging")


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
# them: it installs to the final prefix rather than to a staging tree, and it gets its own build
# RPATH and sets its own BUILD_SHARED_LIBS.  Keeping them apart is what saves the PDI call site from
# having to undo them afterwards.
#
# \param #1 the variable to append the result to
###
function(_sbuild_dependency_policy _SBUILD_OUTVAR)
	set(_SBUILD_RESULT "${${_SBUILD_OUTVAR}}")
	
	# A dependency is used from the stage while the distribution is built, and its own dependencies
	# sit beside it there, so it needs an RPATH of its own: DT_RUNPATH is not consulted for what *it*
	# pulls in, so the RPATH the sub-project gets does not help.
	# The staging directories come first, so that they win during the build, and the user's own
	# CMAKE_INSTALL_RPATH follows them rather than being overridden.  They are absolute, so that no
	# user could have asked for the same entry: sbuild_strip_staging_rpath() removes every entry naming
	# the staging tree once it is installed, and only those.
	set(_SBUILD_RPATH "${_SBUILD_STAGING}/${CMAKE_INSTALL_LIBDIR}" "${_SBUILD_STAGING}/lib")
	list(REMOVE_DUPLICATES _SBUILD_RPATH)
	list(APPEND _SBUILD_RPATH $CACHE{CMAKE_INSTALL_RPATH})
	if(NOT "${APPLE}")
		# The strip rewrites the string in place, which corrupts any symbol name the linker made share
		# its tail (https://gitlab.kitware.com/cmake/cmake/-/work_items/18821).  An entry ending in "/"
		# leaves nothing to share: no symbol name ends that way.  It names a directory already listed,
		# which the loader skips.  Mach-O stores no RPATH among the symbol names.
		list(APPEND _SBUILD_RPATH "${_SBUILD_STAGING}/${CMAKE_INSTALL_LIBDIR}/")
	endif()
	
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
			# PDI is only ever linked against shared, position-independent dependencies, and it never
			# runs their test suites; none of the three reaches them from the cache sweep, so each is
			# stated here.
			"-DBUILD_SHARED_LIBS:BOOL=ON"
			"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON"
			"-DBUILD_TESTING:BOOL=OFF"
			# holds the user's own value, which the cache sweep also forwards, and wins by coming later
			"-DCMAKE_INSTALL_RPATH:STRING=${_SBUILD_RPATH}")
	
	set("${_SBUILD_OUTVAR}" "${_SBUILD_RESULT}" PARENT_SCOPE)
endfunction()



### Add a dependency, either found on the system or built into the staging tree
#
# \param #1 the name of the dependency, as find_package() knows it
# \param #2 the default of PDI_USE_<name>
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
	pdi_setting("USE_${_SBUILD_NAME}" "version of ${_SBUILD_NAME} to use, this can be 1) a path to the library source, 2) EMBEDDED to use the provided version, 3) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 4) AUTO to use SYSTEM if available and EMBEDDED otherwise" "${_SBUILD_DEFAULT}")
	
	set(_SBUILD_TOBUILD FALSE)
	
	get_filename_component(_SBUILD_EMBEDDED_PATH "${_SBUILD_EMBEDDED_PATH}" ABSOLUTE)
	
	if(DEFINED _SBUILD_COMPONENTS)
		set(_SBUILD_COMPONENTS COMPONENTS ${_SBUILD_COMPONENTS})
	endif()
	
	
	if("${PDI_USE_${_SBUILD_NAME}}" STREQUAL "SYSTEM")
		# use the preinstalled dep, should be available in the default path
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, using SYSTEM version (-DPDI_USE_${_SBUILD_NAME}=${PDI_USE_${_SBUILD_NAME}})")
	elseif("${PDI_USE_${_SBUILD_NAME}}" STREQUAL "EMBEDDED")
		# use the dependency as provided in the distribution
		set(_SBUILD_TOBUILD TRUE)
		message(STATUS " **Dependency**: ${_SBUILD_NAME}, using EMBEDDED version (-DPDI_USE_${_SBUILD_NAME}=${PDI_USE_${_SBUILD_NAME}})")
	elseif("${PDI_USE_${_SBUILD_NAME}}" STREQUAL "AUTO")
		# try to behave like SYSTEM, but fallback on EMBEDDED if unavailable
		find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} QUIET ${_SBUILD_COMPONENTS})
		string(TOUPPER "${_SBUILD_NAME}_FOUND" _SBUILD_IS_FOUND)
		if(NOT "${${_SBUILD_NAME}_FOUND}" AND NOT "${${_SBUILD_IS_FOUND}}")
			set(_SBUILD_TOBUILD TRUE)
			set(_SBUILD_VERSION_MSG)
			if(DEFINED _SBUILD_VERSION)
				set(_SBUILD_VERSION_MSG " in version \"${_SBUILD_VERSION}\"")
			endif()
			message(STATUS " **Dependency**: ${_SBUILD_NAME} using EMBEDDED version (SYSTEM not found${_SBUILD_VERSION_MSG}) (-DPDI_USE_${_SBUILD_NAME}=${PDI_USE_${_SBUILD_NAME}})")
		else()
			find_package("${_SBUILD_NAME}" ${_SBUILD_VERSION} REQUIRED ${_SBUILD_COMPONENTS})
			message(STATUS " **Dependency**: ${_SBUILD_NAME} found and using SYSTEM version (-DPDI_USE_${_SBUILD_NAME}=${PDI_USE_${_SBUILD_NAME}})")
		endif()
	else()
		# use the provided path as:
		# 1. the path to the source of the library
		# 2. the path to a tarball of the library source
		set("_SBUILD_EMBEDDED_PATH" "${PDI_USE_${_SBUILD_NAME}}")
	
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
	
	unset(_SBUILD_DEPENDS_NEW)
	foreach(_SBUILD_ONE_DEPENDS IN LISTS _SBUILD_DEPENDS)
		list(APPEND _SBUILD_DEPENDS_NEW "${_SBUILD_ONE_DEPENDS}_pkg")
	endforeach()
	set(_SBUILD_DEPENDS "${_SBUILD_DEPENDS_NEW}")
	
	ExternalProject_Add("${_SBUILD_NAME}_pkg"
		PREFIX "${CMAKE_BINARY_DIR}/${_SBUILD_NAME}"
		${_SBUILD_PATH_DATA}
		EXCLUDE_FROM_ALL 1
		DEPENDS "${_SBUILD_DEPENDS}"
		CMAKE_CACHE_ARGS "${_SBUILD_CMAKE_CACHE_ARGS}"
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
	# Add a BUILD_RPATH to find the dependencies in the staging area at test time.
	set(_SBUILD_BUILD_RPATH "${_SBUILD_STAGING}/${CMAKE_INSTALL_LIBDIR}" "${_SBUILD_STAGING}/lib")
	list(REMOVE_DUPLICATES _SBUILD_BUILD_RPATH)
	list(APPEND _SBUILD_BUILD_RPATH $CACHE{CMAKE_BUILD_RPATH})
	list(APPEND _SBUILD_CACHE_ARGS "-DPDI_SUPERBUILD:BOOL=OFF" "-DCMAKE_BUILD_RPATH:STRING=${_SBUILD_BUILD_RPATH}")
	get_property(_SBUILD_DEPENDS GLOBAL PROPERTY _SBUILD_DEPENDENCY_TARGETS)
	
	ExternalProject_Add("${PROJECT_NAME}"
		PREFIX "${CMAKE_BINARY_DIR}/${PROJECT_NAME}"
		SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}"
		BUILD_ALWAYS TRUE
		DEPENDS "${_SBUILD_DEPENDS}"
		CMAKE_CACHE_ARGS "${_SBUILD_CACHE_ARGS}"
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



## Installation

# The staging tree only fills up with the dependencies that are built, so create it for when none is.
file(MAKE_DIRECTORY "${_SBUILD_STAGING}")
install(DIRECTORY "${_SBUILD_STAGING}/" DESTINATION "." USE_SOURCE_PERMISSIONS)

# Strip the runtime path entries into the staging tree in the same install run as the copy, so that no build tree can be deleted in between.
# The destination is only known at install time: `cmake --install --prefix` and DESTDIR both change it.
if("${APPLE}")
	find_program(PDI_OTOOL NAMES otool llvm-otool)
	mark_as_advanced(PDI_OTOOL)
	set(_SBUILD_STRIP_ARGS MACHO "[==[${_SBUILD_STAGING}]==]" "\"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\""
		"[==[${PDI_OTOOL}]==]" "[==[${CMAKE_INSTALL_NAME_TOOL}]==]")
elseif("${WIN32}")
	set(_SBUILD_STRIP_ARGS NONE "[==[${_SBUILD_STAGING}]==]" "\"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\"")
else()
	set(_SBUILD_STRIP_ARGS ELF "[==[${_SBUILD_STAGING}]==]" "\"\$ENV{DESTDIR}\${CMAKE_INSTALL_PREFIX}\"")
endif()
list(JOIN _SBUILD_STRIP_ARGS " " _SBUILD_STRIP_ARGS)
install(CODE "include([==[${CMAKE_CURRENT_LIST_FILE}]==])\nsbuild_strip_staging_rpath(${_SBUILD_STRIP_ARGS})")
