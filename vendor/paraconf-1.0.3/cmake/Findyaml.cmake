# Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
#               root of the project or at https://github.com/pdidev/paraconf
# Copyright (C) 2012-2013, Lars Baehren <lbaehren@gmail.com>
#
# SPDX-License-Identifier: MIT

cmake_policy(PUSH)
cmake_policy(VERSION 3.22...4.2)

# - Check for the presence of libyaml
#
# The following variables are set when YAML is found:
#  yaml_FOUND      = Set to true, if all components of YAML have been found.
#
# In addition, the following imported target is created:  yaml

function(_yaml_Find_Config)
	find_package(yaml ${yaml_FIND_VERSION} QUIET CONFIG)
	if("${yaml_FOUND}")
		get_target_property(yaml_LIBRARIES yaml LOCATION)
		set(yaml_LIBRARIES "${yaml_LIBRARIES}" PARENT_SCOPE)
		get_target_property(yaml_INCLUDE_DIRS yaml INTERFACE_INCLUDE_DIRECTORIES)
		set(yaml_INCLUDE_DIRS "${yaml_INCLUDE_DIRS}" PARENT_SCOPE)
		mark_as_advanced(yaml_DIR)
		if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
			message(DEBUG "Yaml found through config")
		endif()
	else()
		unset(yaml_DIR CACHE)
		if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
			message(DEBUG "Yaml not found through config")
		endif()
	endif()
endfunction(_yaml_Find_Config)

function(_yaml_Find_Cache)
	if(DEFINED CACHE{yaml_LIBRARIES} AND EXISTS "${yaml_LIBRARIES}")
		add_library(yaml SHARED IMPORTED GLOBAL)
		set_target_properties(yaml PROPERTIES
				IMPORTED_LOCATION ${yaml_LIBRARIES}
				INTERFACE_INCLUDE_DIRECTORIES "${yaml_INCLUDE_DIRS}")
		if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
			message(DEBUG "Yaml found through cache")
		endif()
	else()
		if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
			message(DEBUG "Yaml not found through cache")
		endif()
	endif()
endfunction(_yaml_Find_Cache)

function(_yaml_Find_Pkgconfig)
	find_package(PkgConfig QUIET)
	if(NOT "${PKG_CONFIG_FOUND}")
		if(NOT "${yaml_FIND_QUIETLY}")
			message("PkgConfig not found, unable to look for yaml")
		endif()
		return()
	endif()

	set(PKGCFG_NAMES yaml yaml-0.1)
	if(NOT "x${yaml_FIND_VERSION}" STREQUAL "x")
		set(PKGCFG_NAMES yaml>=${yaml_FIND_VERSION} yaml-0.1>=${yaml_FIND_VERSION} ${PKGCFG_NAMES})
	endif()
	foreach(PKGCFG_NAME IN LISTS PKGCFG_NAMES)
		pkg_search_module(yamlpkg QUIET ${PKGCFG_NAME})
		if("${yamlpkg_FOUND}")
			pkg_get_variable(yamlpkg_INCLUDEDIR ${PKGCFG_NAME} includedir)
			if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
				message(DEBUG "Yaml found as ${PKGCFG_NAME} through pkgconfig")
			endif()
			break()
		else()
			if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
				message(DEBUG "Yaml not found as ${PKGCFG_NAME} through pkgconfig")
			endif()
		endif()
		unset(yamlpkg_FOUND CACHE)
		unset(yamlpkg_FOUND)
	endforeach()

	if("${yamlpkg_FOUND}" AND NOT "0${yaml_FIND_VERSION}" VERSION_GREATER "0${yamlpkg_VERSION}")
		set(yamlpkg_INCLUDE_DIRS ${yamlpkg_INCLUDE_DIRS} ${yamlpkg_INCLUDEDIR})
		find_library(yaml_LIBRARIES NAMES ${yamlpkg_LIBRARIES}
				HINTS ${yamlpkg_LIBRARY_DIRS}
				PATH_SUFFIXES lib
		)
		if(EXISTS "${yaml_LIBRARIES}")
			set(yaml_LIBRARIES "${yaml_LIBRARIES}" CACHE STRING "yaml libraries path" FORCE)
			add_library(yaml SHARED IMPORTED GLOBAL)
			set_target_properties(yaml PROPERTIES
					IMPORTED_LOCATION ${yaml_LIBRARIES})
			if(NOT "${yamlpkg_INCLUDE_DIRS}" STREQUAL "")
				set(yaml_INCLUDE_DIRS "${yamlpkg_INCLUDE_DIRS}" CACHE STRING "yaml include path" FORCE)
				set_target_properties(yaml PROPERTIES
						INTERFACE_INCLUDE_DIRECTORIES "${yamlpkg_INCLUDE_DIRS}")
			endif()
			mark_as_advanced(yaml_INCLUDE_DIRS yaml_LIBRARIES)
			if("${yamlpkg_VERSION}" VERSION_GREATER 0)
				set(yaml_VERSION "${yamlpkg_VERSION}" CACHE STRING "yaml version found" FORCE)
				mark_as_advanced(yaml_VERSION)
			else()
				unset(yaml_VERSION PARENT_SCOPE)
			endif()
		endif()
	elseif("${yamlpkg_FOUND}")
		if("${DEBUG_FIND_YAML}" AND NOT "${yaml_FIND_QUIETLY}")
			message(DEBUG "Yaml version invalid, yaml_FIND_VERSION=${yaml_FIND_VERSION} but yamlpkg_VERSION=${yamlpkg_VERSION}")
		endif()
	endif()
endfunction(_yaml_Find_Pkgconfig)

unset(yaml_FOUND)
unset(yaml_LIBRARIES)
unset(yaml_INCLUDE_DIRS)
unset(yaml_VERSION)
if(TARGET yaml)
	set(yaml_LIBRARIES yaml)
endif()
if(NOT TARGET yaml)
 	_yaml_Find_Config()
endif()
if(NOT TARGET yaml)
	_yaml_Find_Cache()
endif()
if(NOT TARGET yaml)
	_yaml_Find_Pkgconfig()
endif()
if(NOT TARGET yaml)
	unset(yaml_INCLUDE_DIRS CACHE)
	unset(yaml_INCLUDE_DIRS)
	unset(yaml_LIBRARIES CACHE)
	unset(yaml_LIBRARIES)
	unset(yaml_FOUND CACHE)
	unset(yaml_FOUND)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
	yaml
	REQUIRED_VARS yaml_LIBRARIES
	VERSION_VAR yaml_VERSION
)

cmake_policy(POP)
