#[=======================================================================[
.rst:
FindAstyle
-----------

Astyle is a source code indenter, formatter, and beautifier
(http://astyle.sourceforge.net/). This module looks for Astyle.

The following variables are defined by this module:

.. variable:: ASTYLE_FOUND

True if the ``astyle`` executable was found.

.. variable:: ASTYLE_VERSION

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
	if(NOT TARGET Astyle::astyle)
		find_program(
			ASTYLE_EXECUTABLE
			NAMES astyle
			DOC "Astyle, source code indenter, formatter, and beautifier (http://astyle.sourceforge.net/)"
		)
		mark_as_advanced(ASTYLE_EXECUTABLE)

		if(ASTYLE_EXECUTABLE)
			execute_process(
				COMMAND "${ASTYLE_EXECUTABLE}" --version
				OUTPUT_VARIABLE ASTYLE_VERSION
				OUTPUT_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE _Astyle_version_result
			)
			if(_Astyle_version_result)
				message(WARNING "Unable to determine astyle version: ${_Astyle_version_result}")
			else()
				string(REGEX REPLACE "^Artistic Style Version ([0-9\.]+)$" "\\1" ASTYLE_VERSION "${ASTYLE_VERSION}")
			endif()

			# Create an imported target for Astyle
			add_executable(Astyle::astyle IMPORTED GLOBAL)
			set_target_properties(Astyle::astyle PROPERTIES
				IMPORTED_LOCATION "${ASTYLE_EXECUTABLE}"
			)
		endif()
	endif()
endmacro()



#
# Add an indentation target
#
function(Astyle_add_indent)
	cmake_parse_arguments(ASTYLE_INDENT "RECURSIVE;TEST" "WORKING_DIRECTORY;OPTIONS_FILE" "" ${ARGN})
	
	if("${ASTYLE_INDENT_UNPARSED_ARGUMENTS}" MATCHES "^\\s*$")
		message(FATAL_ERROR "Astyle_add_indent called without a target name")
	endif()
	list(GET       ASTYLE_INDENT_UNPARSED_ARGUMENTS 0 ASTYLE_INDENT_TARGET)
	list(REMOVE_AT ASTYLE_INDENT_UNPARSED_ARGUMENTS 0)
	
	if("${ASTYLE_INDENT_WORKING_DIRECTORY}" MATCHES "^\\s*$")
		set(ASTYLE_INDENT_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
	endif()
	
	if("${ASTYLE_INDENT_OPTIONS_FILE}" MATCHES "^\\s*$")
		message(FATAL_ERROR "Astyle_add_indent called without an OPTIONS_FILE")
	endif()
	
	if(NOT 3.12.0 VERSION_GREATER "${CMAKE_VERSION}")
		set(CONFIGURE_DEPENDS CONFIGURE_DEPENDS)
	else()
		set(CONFIGURE_DEPENDS)
	endif()
	if("${ASTYLE_INDENT_RECURSIVE}")
		file(GLOB_RECURSE GLOBBED_ARGS FOLLOW_SYMLINKS LIST_DIRECTORIES false RELATIVE "${ASTYLE_INDENT_WORKING_DIRECTORY}" ${CONFIGURE_DEPENDS} ${ASTYLE_INDENT_UNPARSED_ARGUMENTS})
	else()
		file(GLOB         GLOBBED_ARGS                 LIST_DIRECTORIES false RELATIVE "${ASTYLE_INDENT_WORKING_DIRECTORY}" ${CONFIGURE_DEPENDS} ${ASTYLE_INDENT_UNPARSED_ARGUMENTS})
	endif()
	set(ASTYLE_INDENT_COMMAND_OPTIONS)
	foreach(FILE ${GLOBBED_ARGS})
		get_filename_component(REAL_FILE "${FILE}" REALPATH BASE_DIR "${ASTYLE_INDENT_WORKING_DIRECTORY}")
		if(NOT EXISTS  "${REAL_FILE}")
			message(SEND_ERROR "Unable to find file `${REAL_FILE}' for indentation")
		endif()
		list(APPEND ASTYLE_INDENT_COMMAND_OPTIONS "${REAL_FILE}")
	endforeach()
	
	add_custom_target("${ASTYLE_INDENT_TARGET}"
			COMMAND Astyle::astyle "--suffix=none" "--options=${ASTYLE_INDENT_OPTIONS_FILE}" ${ASTYLE_INDENT_COMMAND_OPTIONS}
			WORKING_DIRECTORY "${ASTYLE_INDENT_WORKING_DIRECTORY}"
			VERBATIM)
	
	if("${ASTYLE_INDENT_TEST}")
		string(REPLACE ";" "' '" ASTYLE_INDENT_COMMAND_OPTIONS "'${ASTYLE_INDENT_COMMAND_OPTIONS}'")
		file(WRITE "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent-${ASTYLE_INDENT_TARGET}"
[=[
#!/bin/bash
FORMATTED="$(]=] "'${ASTYLE_EXECUTABLE}' --dry-run --suffix=none --options='${ASTYLE_INDENT_OPTIONS_FILE}' ${ASTYLE_INDENT_COMMAND_OPTIONS}" [=[ | grep Formatted; echo -n x)"
FORMATTED="${FORMATTED%x}"
NB_FORMATTED="$(echo -n "${FORMATTED}" | wc -l)"
echo "${NB_FORMATTED} file(s) need formatting"
echo "${FORMATTED}" | sed 's/Formatted/ *** Needs formatting:/'
test 00 -eq "0${NB_FORMATTED}"
]=])
		file(COPY "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent-${ASTYLE_INDENT_TARGET}"
			DESTINATION "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/exe"
			FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
		
		add_test(NAME "${ASTYLE_INDENT_TARGET}"
				COMMAND "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/exe/check-indent-${ASTYLE_INDENT_TARGET}"
				WORKING_DIRECTORY "${ASTYLE_INDENT_WORKING_DIRECTORY}")
	endif()
endfunction()



_Astyle_find_astyle()

# Verify find results
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
	Astyle
	REQUIRED_VARS ASTYLE_EXECUTABLE
	VERSION_VAR ASTYLE_VERSION
	HANDLE_COMPONENTS
)
