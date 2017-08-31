#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.1)
# enable C++ to work-around cmake issue #15829 https://cmake.org/Bug/view.php?id=15829
project(pdi LANGUAGES CXX C Fortran)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

cmake_policy(VERSION 3.1)
if(POLICY CMP0054)
	cmake_policy(SET CMP0054 NEW)
endif()

if(POLICY CMP0063)
cmake_policy(SET CMP0063 NEW)
endif()


# Build options
option(USE_SYSTEM_PARACONF 		"Use an already installed paraconf instead of building the embedded one"
	OFF )

option(ENABLE_DECLH5 			"Build HDF5 based DeclH5 plug-in. Produce one HDF5 file per MPI process."
	ON  )
option(ENABLE_FTI 			"Build FTI plug-in."
	ON  )
option(ENABLE_SIONLIB 			"Build SIONlib plug-in."
	ON  )
option(ENABLE_UTILITIES 		"Build Utilities plug-in."
	ON  )

# Testing options
option(RUN_EXAMPLES 			"Build and run examples during ctest."
	ON  ) 

option(TIMEOUT
	"Tests run (max) duration is 3 seconds. If set to NO, duration is increased to 10mn. Usefull to test with memcheck activated (CTest -T memcheck)."
	"3"
)
if (${TIMEOUT} GREATER "1" ) 
	set(DEFAULT_TIMEOUT ${TIMEOUT})
else()
	set(DEFAULT_TIMEOUT 3)
endif()


## Because of plugins, we need PIC code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)


# Find dependancies
find_package(MPI REQUIRED)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf)
else()
	add_subdirectory(vendor/paraconf paraconf EXCLUDE_FROM_ALL)
endif()


# Version

file(READ VERSION pdi_VERSION)
string(STRIP "${pdi_VERSION}" pdi_VERSION)
string(REPLACE "." ";" pdi_VERSION_list ${pdi_VERSION})
list(GET pdi_VERSION_list 0 pdi_VERSION_MAJOR)
list(GET pdi_VERSION_list 1 pdi_VERSION_MINOR)
list(GET pdi_VERSION_list 2 pdi_VERSION_PATCH)

# Examples & tests
include( CTest )
if( "${RUN_EXAMPLES}" ) 
	add_subdirectory(example/)
endif()
add_subdirectory(tests/)

# The src
add_custom_target(re_format target PRE_BUILD command cd ./tools/Format_and_Indent/ && make all)
## Main source dir
add_subdirectory(src/)

## The plugins
add_subdirectory(plugins/test_plugin/)
add_subdirectory(plugins/User_Code/)

if(${ENABLE_DECLH5})
	add_subdirectory(plugins/DeclH5/)
endif()

if(${ENABLE_FTI})
	add_subdirectory(plugins/FTI/)
endif()

if(${ENABLE_SIONLIB})
	add_subdirectory(plugins/SIONlib/)
endif()

if(${ENABLE_UTILITIES})
	add_subdirectory(plugins/Utilities/)
endif()


