
#--------------------------------------------------------------------------------
# Copyright (c) 2012-2013, Lars Baehren <lbaehren@gmail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#--------------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.5)

# - Check for the presence of libyaml
#
# The following variables are set when YAML is found:
#  yaml_FOUND      = Set to true, if all components of YAML have been found.
#  yaml_LIBRARIES  = 
#
# In addition, the following imported target is created
#  yaml = Include path for the header files of YAML

function(_yaml_Find_Config)
	find_package(yaml ${yaml_FIND_VERSION} QUIET CONFIG)
	if("${yaml_FOUND}")
		mark_as_advanced(yaml_DIR)
	else()
		unset(yaml_DIR CACHE)
	endif()
endfunction(_yaml_Find_Config)

function(_yaml_Find_Pkgconfig)
	find_package(PkgConfig QUIET REQUIRED)
	
	if( NOT "x${yaml_FIND_VERSION}" STREQUAL "x")
		pkg_search_module(yaml QUIET yaml>=${yaml_FIND_VERSION} yaml-0.1>=${yaml_FIND_VERSION})
	else()
		pkg_search_module(yaml QUIET yaml yaml-0.1)
	endif()
	
	if ( "${yaml_FOUND}" )
	
		find_library (yaml_LIBRARIES NAMES ${yaml_LIBRARIES}
			HINTS ${yaml_LIBRARY_DIRS}
			PATH_SUFFIXES lib
		)
		
		if ( EXISTS "${yaml_LIBRARIES}" )
			add_library(yaml SHARED IMPORTED GLOBAL)
			set_target_properties(yaml PROPERTIES
					IMPORTED_LOCATION ${yaml_LIBRARIES}
					INTERFACE_INCLUDE_DIRECTORIES ${yaml_INCLUDE_DIRS})
		endif()
		set(yaml_VERSION "${yaml_VERSION}" PARENT_SCOPE)
	endif()
	
endfunction(_yaml_Find_Pkgconfig)

unset(yaml_FOUND)
unset(yaml_LIBRARIES)
unset(yaml_VERSION)
if ( NOT TARGET yaml )
	_yaml_Find_Config()
endif()
if ( NOT TARGET yaml )
	_yaml_Find_Pkgconfig()
endif()
if ( TARGET yaml AND NOT "${yaml_LIBRARIES}" )
	set(yaml_LIBRARIES yaml)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(
	yaml
	REQUIRED_VARS yaml_LIBRARIES
	VERSION_VAR yaml_VERSION
)
