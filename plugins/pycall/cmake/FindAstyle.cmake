#[=======================================================================[
.rst:
FindAstyle
-----------

Astyle is a source code indenter, formatter, and beautifier
(http://astyle.sourceforge.net/). This module looks for Astyle.

The following variables are defined by this module:

.. variable:: Astyle_FOUND

True if the ``astyle`` executable was found.

.. variable:: Astyle_VERSION

The version reported by ``astyle --version``.

The module defines ``IMPORTED`` targets for Astyle. These can be used as part of
custom commands, etc. The following import target is defined:

::

Astyle::astyle

#]=======================================================================]

cmake_minimum_required(VERSION 3.5)
list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}")


#
# Find Astyle...
#
macro(_Astyle_find_astyle)
	if(NOT "${Astyle_EXECUTABLE}")
		find_program(
			Astyle_EXECUTABLE
			NAMES astyle
			DOC "Astyle, source code indenter, formatter, and beautifier (http://astyle.sourceforge.net/)"
		)
		mark_as_advanced(Astyle_EXECUTABLE)
	endif()
	if("${Astyle_EXECUTABLE}" AND NOT "${Astyle_VERSION}")
		execute_process(
			COMMAND "${Astyle_EXECUTABLE}" --version
			OUTPUT_VARIABLE Astyle_VERSION
			OUTPUT_STRIP_TRAILING_WHITESPACE
			RESULT_VARIABLE _Astyle_version_result
		)
		if("${_Astyle_version_result}")
			message(WARNING "Unable to determine astyle version: ${_Astyle_version_result}")
		else()
			string(REGEX REPLACE "^Artistic Style Version ([0-9\.]+)$" "\\1" Astyle_VERSION "${Astyle_VERSION}")
			set(Astyle_VERSION "${Astyle_VERSION}" CACHE STRING "Artistic style (Astyle) version" FORCE)
		endif()
		unset(_Astyle_version_result)
	endif()
endmacro()



#
# Add an indentation target
#
function(Astyle_add_indent)
	cmake_parse_arguments(_AS "RECURSIVE;TEST" "WORKING_DIRECTORY;OPTIONS_FILE" "" ${ARGN})
	
	if("${_AS_UNPARSED_ARGUMENTS}" MATCHES "^\\s*$")
		message(FATAL_ERROR "Astyle_add_indent called without a target name")
	endif()
	list(GET       _AS_UNPARSED_ARGUMENTS 0 _AS_TARGET)
	list(REMOVE_AT _AS_UNPARSED_ARGUMENTS 0)
	
	if("${_AS_WORKING_DIRECTORY}" MATCHES "^\\s*$")
		set(_AS_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
	endif()
	
	if("${_AS_OPTIONS_FILE}" MATCHES "^\\s*$")
		message(FATAL_ERROR "Astyle_add_indent called without an OPTIONS_FILE")
	endif()
	
	if(NOT 3.12.0 VERSION_GREATER "${CMAKE_VERSION}")
		set(CONFIGURE_DEPENDS CONFIGURE_DEPENDS)
	else()
		set(CONFIGURE_DEPENDS)
	endif()
	if("${_AS_RECURSIVE}")
		file(GLOB_RECURSE GLOBBED_ARGS FOLLOW_SYMLINKS LIST_DIRECTORIES false RELATIVE "${_AS_WORKING_DIRECTORY}" ${CONFIGURE_DEPENDS} ${_AS_UNPARSED_ARGUMENTS})
	else()
		file(GLOB         GLOBBED_ARGS                 LIST_DIRECTORIES false RELATIVE "${_AS_WORKING_DIRECTORY}" ${CONFIGURE_DEPENDS} ${_AS_UNPARSED_ARGUMENTS})
	endif()
	set(_AS_COMMAND_OPTIONS)
	foreach(FILE ${GLOBBED_ARGS})
		get_filename_component(REAL_FILE "${FILE}" REALPATH BASE_DIR "${_AS_WORKING_DIRECTORY}")
		if(NOT EXISTS  "${REAL_FILE}")
			message(SEND_ERROR "Unable to find file `${REAL_FILE}' for indentation")
		endif()
		list(APPEND _AS_COMMAND_OPTIONS "${REAL_FILE}")
	endforeach()
	
	add_custom_target("${_AS_TARGET}"
			COMMAND Astyle::astyle "--suffix=none" "--options=${_AS_OPTIONS_FILE}" ${_AS_COMMAND_OPTIONS}
			WORKING_DIRECTORY "${_AS_WORKING_DIRECTORY}"
			VERBATIM)
	
	if("${_AS_TEST}")
		string(REPLACE ";" "' '" _AS_COMMAND_OPTIONS "'${_AS_COMMAND_OPTIONS}'")
		file(WRITE "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent-${_AS_TARGET}"
[=[
#!/bin/bash
FORMATTED="$(]=] "'${Astyle_EXECUTABLE}' --dry-run --suffix=none --options='${_AS_OPTIONS_FILE}' ${_AS_COMMAND_OPTIONS}" [=[ | grep Formatted; echo -n x)"
FORMATTED="${FORMATTED%x}"
NB_FORMATTED="$(echo -n "${FORMATTED}" | wc -l)"
echo "${NB_FORMATTED} file(s) need formatting"
echo "${FORMATTED}" | sed 's/Formatted/ *** Needs formatting:/'
test 00 -eq "0${NB_FORMATTED}"
]=])
		file(COPY "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent-${_AS_TARGET}"
			DESTINATION "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/exe"
			FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
		
		add_test(NAME "${_AS_TARGET}"
				COMMAND "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/exe/check-indent-${_AS_TARGET}"
				WORKING_DIRECTORY "${_AS_WORKING_DIRECTORY}")
	endif()
endfunction()



_Astyle_find_astyle()

# Verify find results
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
	Astyle
	REQUIRED_VARS Astyle_EXECUTABLE
	VERSION_VAR Astyle_VERSION
	HANDLE_COMPONENTS
)

if("${Astyle_FOUND}")
	if(NOT TARGET Astyle::astyle)
		add_executable(Astyle::astyle IMPORTED GLOBAL)
		set_target_properties(Astyle::astyle PROPERTIES
			IMPORTED_LOCATION "${Astyle_EXECUTABLE}"
		)
	endif()
else()
	unset(Astyle_EXECUTABLE CACHE)
	unset(Astyle_VERSION CACHE)
endif()
