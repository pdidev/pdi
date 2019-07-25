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

cmake_minimum_required(VERSION 3.5)

include(FindPackageHandleStandardArgs)

function(__py3yaml_detect)
set(_Py3YAML_QUIET)
if("${Py3YAML_FIND_QUIETLY}")
	set(_Py3YAML_QUIET QUIET)
endif()

set(_Py3YAML_REQUIRED)
set(_Py3YAML_ERROR_LEVEL)
if("${Py3YAML_FIND_REQUIRED}")
	set(_Py3YAML_REQUIRED REQUIRED)
	set(_Py3YAML_ERROR_LEVEL SEND_ERROR)
endif()

set(Py3YAML_COMPONENTS_PARAM)
set(Py3YAML_OPTIONAL_COMPONENTS)
foreach(_Py3YAML_COMPONENT ${Py3YAML_FIND_COMPONENTS})
	if("${Py3YAML_FIND_REQUIRED_${_Py3YAML_COMPONENT}}")
		list(APPEND Py3YAML_COMPONENTS_PARAM "${_Py3YAML_COMPONENT}")
	else()
		list(APPEND Py3YAML_OPTIONAL_COMPONENTS "${_Py3YAML_COMPONENT}")
	endif()
endforeach()
if(NOT "xx" STREQUAL "x${Py3YAML_OPTIONAL_COMPONENTS}x")
	list(APPEND Py3YAML_COMPONENTS_PARAM OPTIONAL_COMPONENTS ${Py3YAML_OPTIONAL_COMPONENTS})
endif()

find_package(Python3 ${_Py3YAML_QUIET} ${_Py3YAML_REQUIRED} ${Py3YAML_COMPONENTS_PARAM})

if("${Python3_FOUND}")
	execute_process (COMMAND "${Python3_EXECUTABLE}" -c "import yaml; print(yaml.__file__)"
			RESULT_VARIABLE _Py3YAML_RESULT
			OUTPUT_VARIABLE Py3YAML_PATH
			ERROR_VARIABLE _Py3YAML_STDERR
			OUTPUT_STRIP_TRAILING_WHITESPACE)
	if (_Py3YAML_RESULT)
		unset(Py3YAML_PATH)
		if(NOT "${Py3YAML_FIND_QUIETLY}")
			message(WARNING "Error importing 'yaml' from python3:\n${_Py3YAML_STDERR}")
		endif()
	else()
		get_filename_component(Py3YAML_PATH "${Py3YAML_PATH}" DIRECTORY)
		set(Py3YAML_PATH "${Py3YAML_PATH}" PARENT_SCOPE)
	endif()
endif()
endfunction()

__py3yaml_detect()
find_package_handle_standard_args(Py3YAML REQUIRED_VARS Py3YAML_PATH)
