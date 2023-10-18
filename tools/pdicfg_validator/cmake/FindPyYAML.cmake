################################################################################
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies
# alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10...3.25)

include(FindPackageHandleStandardArgs)

function(__py3yaml_detect)
set(_PyYAML_QUIET)
if("${PyYAML_FIND_QUIETLY}")
	set(_PyYAML_QUIET QUIET)
endif()

set(_PyYAML_REQUIRED)
set(_PyYAML_ERROR_LEVEL)
if("${PyYAML_FIND_REQUIRED}")
	set(_PyYAML_REQUIRED REQUIRED)
	set(_PyYAML_ERROR_LEVEL SEND_ERROR)
endif()

set(PyYAML_COMPONENTS_PARAM)
set(PyYAML_OPTIONAL_COMPONENTS)
foreach(_PyYAML_COMPONENT ${PyYAML_FIND_COMPONENTS})
	if("${PyYAML_FIND_REQUIRED_${_PyYAML_COMPONENT}}")
		list(APPEND PyYAML_COMPONENTS_PARAM "${_PyYAML_COMPONENT}")
	else()
		list(APPEND PyYAML_OPTIONAL_COMPONENTS "${_PyYAML_COMPONENT}")
	endif()
endforeach()
if(NOT "xx" STREQUAL "x${PyYAML_OPTIONAL_COMPONENTS}x")
	list(APPEND PyYAML_COMPONENTS_PARAM OPTIONAL_COMPONENTS ${PyYAML_OPTIONAL_COMPONENTS})
endif()

find_package(Python3 ${_PyYAML_QUIET} ${_PyYAML_REQUIRED} ${PyYAML_COMPONENTS_PARAM})

if("${Python3_FOUND}")
	execute_process (COMMAND "${Python3_EXECUTABLE}" -c "import yaml; print(yaml.__file__)"
			RESULT_VARIABLE _PyYAML_RESULT
			OUTPUT_VARIABLE PyYAML_PATH
			ERROR_VARIABLE _PyYAML_STDERR
			OUTPUT_STRIP_TRAILING_WHITESPACE)
	if (_PyYAML_RESULT)
		unset(PyYAML_PATH)
		if(NOT "${PyYAML_FIND_QUIETLY}")
			message(WARNING "Error importing 'yaml' from python3:\n${_PyYAML_STDERR}")
		endif()
	else()
		get_filename_component(PyYAML_PATH "${PyYAML_PATH}" DIRECTORY)
		set(PyYAML_PATH "${PyYAML_PATH}" PARENT_SCOPE)
	endif()
endif()
endfunction()

__py3yaml_detect()
find_package_handle_standard_args(PyYAML REQUIRED_VARS PyYAML_PATH)
