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


#
# Find Astyle...
#
macro(_Astyle_find_astyle)
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
		if(NOT TARGET Astyle::astyle)
			add_executable(Astyle::astyle IMPORTED GLOBAL)
			set_target_properties(Astyle::astyle PROPERTIES
				IMPORTED_LOCATION "${ASTYLE_EXECUTABLE}"
			)
		endif()
	endif()
endmacro()

# function(Astyle_indent )
# 	add_custom_target(indent
# 		COMMAND Astyle::astyle --suffix=none --options=docs/formating.astyle --recursive src/*.cxx src/*.c include/*.h plugins/*.cxx plugins/*.c example/*.c tests/*.c tests/*.cxx tests/*.h
# 		WORKING_DIRECTORY "${PDI_SOURCE_DIR}/"
# 		VERBATIM)
# 	file(WRITE "${PDI_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent" "#!/bin/bash
# cd '${PDI_SOURCE_DIR}'
# FORMATTED=\"\$(
# '${ASTYLE_EXECUTABLE}' --dry-run --suffix=none --options=docs/formating.astyle --recursive 'src/*.cxx' 'src/*.c' 'include/*.h' 'plugins/*.cxx' 'plugins/*.c' 'example/*.c' 'tests/*.c' 'tests/*.cxx' 'tests/*.h' \\
# | grep Formatted \\
# )\"
# NB_FORMATTED=\"\$(echo -n \"\${FORMATTED}\" | wc -l)\"
# echo \"\${NB_FORMATTED} file(s) need formatting:\"
# echo \"\${FORMATTED}\" | sed 's/Formatted/ *** Needs formatting:/'
# test 00 -eq \"0\${NB_FORMATTED}\"
# ")
# 	file(COPY "${PDI_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent"
# 		DESTINATION "${PDI_BINARY_DIR}"
# 		FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
# 	
# 	add_test(NAME PDI_test_indent COMMAND "${PDI_BINARY_DIR}/check-indent")
# endfunction()

_Astyle_find_astyle()

# Verify find results
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
	Astyle
	REQUIRED_VARS ASTYLE_EXECUTABLE
	VERSION_VAR ASTYLE_VERSION
	HANDLE_COMPONENTS
)
