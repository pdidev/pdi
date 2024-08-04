#=============================================================================
# Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.10)
project(pdi_json_plugin LANGUAGES C CXX)

#
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
option(BUILD_FORTRAN "Enable Fortran support" OFF)
option(BUILD_INDENT "Enable automatic code indentation" OFF)

# Includes
include(CTest)
include(GNUInstallDirs)

if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	enable_language(Fortran)
	set(PDI_COMPONENTS f90)
endif()

# PDI
find_package(PDI REQUIRED COMPONENTS plugins "${PDI_COMPONENTS}")

# The plugin
include (CheckIncludeFileCXX)
CHECK_INCLUDE_FILE_CXX("filesystem" HAVE_FILESYSTEM)
CHECK_INCLUDE_FILE_CXX("experimental/filesystem" HAVE_EXPERIMENTAL_FILESYSTEM)
set(USE_EXPERIMENTAL_FILESYSTEM $<$<AND:$<BOOL:${HAVE_EXPERIMENTAL_FILESYSTEM}>,$<NOT:$<BOOL:${HAVE_FILESYSTEM}>>>:stdc++fs>)

add_library(${PROJECT_NAME} MODULE json.cxx)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_VISIBILITY_PRESET hidden)
target_link_libraries(${PROJECT_NAME} PUBLIC PDI::PDI_plugins ${USE_EXPERIMENTAL_FILESYSTEM})

if (HAVE_FILESYSTEM)
    # target_link_libraries(${PROJECT_NAME} PRIVATE std::filesystem)
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_HEADER=<filesystem>)
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_NAMESPACE=std::filesystem)
elseif(HAVE_EXPERIMENTAL_FILESYSTEM)
    # target_compile_options(${PROJECT_NAME} PRIVATE -lstdc++fs)
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_HEADER=<experimental/filesystem>)
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_NAMESPACE=std::experimental::filesystem)
else()
    message(STATUS "Can't find filesystem or experimental/filesystem. Trying to compile with boost/filesystem")
    find_package(Boost COMPONENTS filesystem)
    if (NOT Boost_FOUND)
        message(FATAL_ERROR "Can't compile code, neither filesystem nor boost found.")
    endif()
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_HEADER=<boost/filesystem.hpp>)
    target_compile_definitions(${PROJECT_NAME} PRIVATE -DCXX_FILESYSTEM_NAMESPACE=boost::filesystem)
endif()

# Installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS ${PROJECT_NAME}
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Indentation
if("${BUILD_INDENT}")
    find_package(Astyle 3.1 REQUIRED)
	Astyle_add_indent(indent TEST
		OPTIONS_FILE formating.astyle
		json.cxx tests/*.c)
endif()

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} --rerun-failed --output-on-failure)
