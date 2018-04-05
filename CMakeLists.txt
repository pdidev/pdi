#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)
project(pdi LANGUAGES CXX C Fortran)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")


# Build options
option(USE_SYSTEM_PARACONF     "Use an already installed paraconf instead of building the embedded one" OFF )
option(ENABLE_DECL_HDF5        "Build HDF5 based decl'HDF5 plug-in." ON )
option(ENABLE_FTI              "Build FTI plug-in." ON )
option(ENABLE_DECL_SION        "Build SIONlib plug-in." ON )
option(RUN_EXAMPLES            "Build and run examples during ctest." ON  )
option(USE_SYSTEM_GTEST        "Use an already installed gtest and gmock instead of downloading and building." OFF )

# Because of plugins, we need PIC code
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Find dependencies
find_package(MPI REQUIRED)
find_package(Doxygen OPTIONAL_COMPONENTS dot)
find_package(Astyle)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf REQUIRED)
else()
	add_subdirectory(vendor/paraconf paraconf EXCLUDE_FROM_ALL)
endif()

# Enable testing
include(CTest)
if("${BUILD_TESTING}" AND "${CMAKE_VERSION}" VERSION_LESS "3.10.0") 
	message(FATAL_ERROR "Minimum CMake version for tests is 3.10. Please update CMake or turn off testing by adding flag -DBUILD_TESTING=OFF")
endif()

# Version
file(READ VERSION pdi_VERSION)
string(STRIP "${pdi_VERSION}" pdi_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${pdi_VERSION}")
set(pdi_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(pdi_VERSION_MINOR "${CMAKE_MATCH_2}")
set(pdi_VERSION_PATCH "${CMAKE_MATCH_3}")
set(pdi_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${pdi_VERSION_MODIF}")
	execute_process(COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/cmake/version-uid" WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" OUTPUT_VARIABLE pdi_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

# Default standard in the project is C99 & C++11
set(CMAKE_CXX_STANDARD 11)
set(CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_C_STANDARD 99)
set(C_STANDARD_REQUIRED TRUE)

# Main source dir
add_subdirectory(src/)

# The plugins
add_subdirectory(plugins/test/)
add_subdirectory(plugins/user_code/)
if(${ENABLE_DECL_HDF5})
	add_subdirectory(plugins/decl_hdf5/)
endif()
if(${ENABLE_FTI})
	add_subdirectory(plugins/fti/)
endif()
if(${ENABLE_DECL_SION})
	add_subdirectory(plugins/decl_sion/)
endif()

# Examples & tests
if("${BUILD_TESTING}" AND "${RUN_EXAMPLES}") 
	add_subdirectory(example/)
endif()
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()

# Source formatting
if("${ASTYLE_FOUND}")
	add_custom_target(indent
		COMMAND Astyle::astyle --suffix=none --options=docs/formating.astyle --recursive src/*.cxx src/*.c include/*.h plugins/*.cxx plugins/*.c example/*.c tests/*.c tests/*.cxx tests/mocks/*.h tests/include/*.h
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/"
		VERBATIM)
	file(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent" "#!/bin/bash
cd '${CMAKE_CURRENT_SOURCE_DIR}'
FORMATTED=\"\$(
'${ASTYLE_EXECUTABLE}' --dry-run --suffix=none --options=docs/formating.astyle --recursive 'src/*.cxx' 'src/*.c' 'include/*.h' 'plugins/*.cxx' 'plugins/*.c' 'example/*.c' 'tests/*.c' 'tests/*.cxx' 'tests/mocks/*.h' \\
| grep Formatted \\
)\"
NB_FORMATTED=\"\$(echo -n \"\${FORMATTED}\" | wc -l)\"
echo \"\${NB_FORMATTED} file(s) need formatting:\"
echo \"\${FORMATTED}\" | sed 's/Formatted/ *** Needs formatting:/'
test 00 -eq \"0\${NB_FORMATTED}\"
")
	file(COPY "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/check-indent"
		DESTINATION "${CMAKE_BINARY_DIR}"
		FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
	
	add_test(NAME PDI_test_indent COMMAND "${CMAKE_BINARY_DIR}/check-indent")
endif()

# Documentation
if ("${DOXYGEN_FOUND}")
	add_custom_target(doc COMMAND "${DOXYGEN_EXECUTABLE}" doxygen.cfg WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/docs" VERBATIM)
endif()
