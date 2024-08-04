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

cmake_minimum_required(VERSION 3.10...3.25)
project(pdi_test_gpu_plugin LANGUAGES C CXX)

#
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
option(BUILD_INDENT "Enable automatic code indentation" OFF)
option(BUILD_TESTING "Build tests" OFF)

include(CTest)
# include(GNUInstallDirs)

# PDI
find_package(PDI REQUIRED plugins)

# The plugin
add_library(pdi_test_gpu_plugin MODULE test_gpu.cxx)

target_link_libraries(pdi_test_gpu_plugin PUBLIC PDI::PDI_plugins)
set_target_properties(pdi_test_gpu_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)
# target_compile_options(pdi_test_gpu_plugin PRIVATE -Wall -Wextra -Wpedantic -g3 -Wno-unknown-pragmas -Wconversion -Wdouble-promotion)

# Build main files
# Default standard in the project is C11
# set(CMAKE_C_STANDARD 11)
# set(CMAKE_C_STANDARD_REQUIRED TRUE)

# set(CMAKE_CXX_FLAGS "-g3 -Wall -Wextra -Wconversion -Wdouble-promotion -fsanitize=address -fsanitize=undefined -fsanitize=leak -fsanitize=pointer-compare -fsanitize=pointer-subtract")
# set(DEFAULT_BUILD_TYPE "RelWithDebInfo")
# set(CMAKE_CXX_FLAGS "-g3 -Wall -Wextra -Wconversion -Wdouble-promotion")

# Debug file
add_executable(debug debug.cxx)
target_link_libraries(debug PDI::PDI_C)
# target_compile_options(debug PRIVATE -Wall -Wextra -Wpedantic -g3 -Wconversion -Wdouble-promotion)


# Indentation (Astyle)
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
    if(Astyle_FOUND)
        Astyle_add_indent(indent TEST
            OPTIONS_FILE formating.astyle
            test_gpu.cxx tests/*.cxx)
    endif()
endif()

# Installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_test_gpu_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
