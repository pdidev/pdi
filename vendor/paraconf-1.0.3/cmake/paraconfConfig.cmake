# Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
#               root of the project or at https://github.com/pdidev/paraconf
#
# SPDX-License-Identifier: MIT

cmake_policy(PUSH)
cmake_policy(VERSION 3.22...4.2)
list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}")

include(CMakeFindDependencyMacro)


# by default, if no component is specified, look for all but only require C

set(_paraconf_FIND_QUIETLY_OPTIONAL "${paraconf_FIND_QUIETLY}")
if("xx" STREQUAL "x${paraconf_FIND_COMPONENTS}x")
	set(paraconf_FIND_COMPONENTS C f90)
	set(paraconf_FIND_REQUIRED_C TRUE)
	set(paraconf_FIND_REQUIRED_f90 FALSE)
	set(_paraconf_FIND_QUIETLY_OPTIONAL TRUE)
endif()


# Import our dependencies

find_dependency(Threads)
find_dependency(yaml)


# The other targets that were exported from paraconf CMakeLists.txt

if(TARGET yaml)
	include("${CMAKE_CURRENT_LIST_DIR}/paraconf.cmake")
endif()

# check the C component

if(NOT TARGET paraconf::paraconf)
	set(paraconf_FOUND "FALSE")
	if(NOT "${paraconf_FIND_QUIETLY}")
		message(WARNING "paraconf: component \"C\" not found")
	endif()
endif()


# Check the other components (f90)

list(REMOVE_ITEM paraconf_FIND_COMPONENTS "C")
foreach(_paraconf_ONE_COMPONENT ${paraconf_FIND_COMPONENTS})
	if(NOT TARGET "paraconf::paraconf_${_paraconf_ONE_COMPONENT}")
		if("${paraconf_FIND_REQUIRED_${_paraconf_ONE_COMPONENT}}")
			set(paraconf_FOUND "FALSE")
		endif()
		if(NOT "${paraconf_FIND_QUIETLY}")
			message(WARNING "paraconf: component \"${_paraconf_ONE_COMPONENT}\" not found")
		endif()
	endif()
endforeach()


# Cleanup

unset(_paraconf_FIND_QUIETLY_OPTIONAL)
unset(_paraconf_ONE_COMPONENT)

cmake_policy(POP)
