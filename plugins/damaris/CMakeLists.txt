#=============================================================================
# Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
project(pdi_damaris_plugin LANGUAGES C CXX)
# list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")


# target_include_directories(damaris_plugin PUBLIC ${Boost_INCLUDE_DIRS})  
# target_link_libraries(damaris_plugin ${Boost_LIBRARY_DIRS})


option(BUILD_CFG_VALIDATOR "Build config validation script" OFF)
option(BUILD_INDENT "Enable automatic code indentation" OFF)

# Includes
include(GNUInstallDirs)

# MPI
find_package(MPI REQUIRED)

# Astyle
if("${BUILD_INDENT}")
    find_package(Astyle 3.1 REQUIRED)
endif()


find_package(Boost 1.65.0 REQUIRED COMPONENTS python numpy)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)



# Damaris
find_package(Damaris 1.8 REQUIRED)
if (Damaris_FOUND)
    message(STATUS "The Damaris library was found")
    # Adds the libraries used - Damaris and its dependencies to the link line
    # target_link_libraries(mysim PUBLIC ${Damaris_LIBRARIES})
    # This creates a definition of a pre-processor variable (PROG_HAS_DAMARIS)
    # that can be used as a guard in the C/C++ code:
    # target_compile_definitions(mysim PRIVATE PROG_HAS_DAMARIS)
else()
    message(STATUS "The Damaris library was NOT found")
endif()

# The Damaris plugin install path.
# This is the shared library that Damaris loads and uses triggers to call functions from within it.
add_library(damaris_plugin SHARED damaris_plugin/exampleLib.cpp)
target_compile_options(damaris_plugin PRIVATE -fPIC)
include_directories(SYSTEM ${MPI_INCLUDE_PATH})
target_include_directories(damaris_plugin PUBLIC  ${MPI_INCLUDE_PATH} ${Damaris_INCLUDE_DIRS})
target_link_directories(damaris_plugin PRIVATE ${Damaris_LIBRARY_DIRS})
target_link_libraries(damaris_plugin damaris ${Damaris_LIBRARIES})


# The PDI plugin for Damaris
add_library(pdi_damaris_plugin MODULE damaris.cxx)
target_include_directories(pdi_damaris_plugin PUBLIC ${MPI_INCLUDE_DIRS} ${Damaris_INCLUDE_DIRS})
target_link_directories(pdi_damaris_plugin PRIVATE ${Damaris_LIBRARY_DIRS})
target_link_libraries(pdi_damaris_plugin PDI::PDI_plugins MPI::MPI_CXX MPI::MPI_C ${Damaris_LIBRARIES})
set_target_properties(pdi_damaris_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)



# Indentation
if("${BUILD_INDENT}")
    Astyle_add_indent(indent TEST
        OPTIONS_FILE formating.astyle
        damaris.cxx)
endif()

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_damaris_plugin
        LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# # The Damaris plugin install path.
# # This is the shared library that Damaris loads and uses triggers to call functions from within it.
set_target_properties(damaris_plugin PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY /home/jbowden/exa_dost_pdi/pdi_v1.6/pdi/example
)

set_target_properties(pdi_damaris_plugin PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY /home/jbowden/exa_dost_pdi/pdi_v1.6/pdi/example
)


# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
    find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter)
    install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
            DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/")
endif()
