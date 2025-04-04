#=============================================================================
# Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2024 Institut national de recherche en informatique et en automatique (Inria)
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_damaris_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# TODO:
#include(CTest)

include(GNUInstallDirs)

#Damaris
set(Damaris_DEPS damaris MPI::MPI_C MPI::MPI_CXX)
find_package(Damaris)
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
include_directories(${Damaris_INCLUDE_DIRS} ${Damaris_INCLUDE_DIRS}/damaris)
link_directories(${Damaris_LIBRARIES_PATH})

# MPI
find_package(MPI REQUIRED COMPONENTS CXX C)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# The plugin
add_library(pdi_damaris_plugin MODULE
		damaris.cxx
        damaris_cfg.cxx
        damaris_wrapper.cxx
        damaris_api_call_handler.cxx)
target_link_libraries(pdi_damaris_plugin PUBLIC PDI::PDI_plugins ${Damaris_DEPS})
set_target_properties(pdi_damaris_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_damaris_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

#[===[ TODO
# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
]===]

if("${DAMARIS_BUILD_EXAMPLES}")
    add_subdirectory(example/)
endif()
