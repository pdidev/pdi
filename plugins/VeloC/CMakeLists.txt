#=============================================================================
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# Neither the name of CEA nor the names of its contributors may be used to
# endorse or promote products derived from this software without specific
# prior written permission.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================
cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_veloc_plugin LANGUAGES C CXX)

include(CTest)
include(GNUInstallDirs)

add_compile_definitions(OMPI_SKIP_MPICXX=1)

find_package(spdlog 1.9 REQUIRED)
find_package(PDI    REQUIRED plugins)
find_package(MPI    REQUIRED COMPONENTS C CXX)
# --- ER: config mode if available (er >= 0.5), else prefix fallback (er 0.x) ---
find_package(er CONFIG QUIET)
if(NOT er_FOUND)
    find_path(ER_INCLUDE_DIR NAMES er.h
        HINTS ${WITH_ER_PREFIX} ENV WITH_ER_PREFIX ${ER_ROOT} ENV ER_ROOT ${CMAKE_PREFIX_PATH}
        PATH_SUFFIXES include)
    find_library(ER_LIBRARY NAMES er
        HINTS ${WITH_ER_PREFIX} ENV WITH_ER_PREFIX ${ER_ROOT} ENV ER_ROOT ${CMAKE_PREFIX_PATH}
        PATH_SUFFIXES ${CMAKE_INSTALL_LIBDIR} lib lib64)
    if(NOT (ER_INCLUDE_DIR AND ER_LIBRARY))
        message(FATAL_ERROR "ER not found: no CMake config and no er.h/liber under prefix. "
                            "Set WITH_ER_PREFIX or CMAKE_PREFIX_PATH to the ER install.")
    endif()
    add_library(er::er UNKNOWN IMPORTED)
    set_target_properties(er::er PROPERTIES
        IMPORTED_LOCATION "${ER_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${ER_INCLUDE_DIR}")
endif()

# --- AXL: same story, axl <= 0.3.0 ships no config ---
find_package(axl CONFIG QUIET)
if(NOT axl_FOUND)
    find_path(AXL_INCLUDE_DIR NAMES axl.h
        HINTS ${WITH_AXL_PREFIX} ENV WITH_AXL_PREFIX ${AXL_ROOT} ENV AXL_ROOT ${CMAKE_PREFIX_PATH}
        PATH_SUFFIXES include)
    find_library(AXL_LIBRARY NAMES axl
        HINTS ${WITH_AXL_PREFIX} ENV WITH_AXL_PREFIX ${AXL_ROOT} ENV AXL_ROOT ${CMAKE_PREFIX_PATH}
        PATH_SUFFIXES ${CMAKE_INSTALL_LIBDIR} lib lib64)
    if(NOT (AXL_INCLUDE_DIR AND AXL_LIBRARY))
        message(FATAL_ERROR "AXL not found: no CMake config and no axl.h/libaxl under prefix. "
                            "Set WITH_AXL_PREFIX or CMAKE_PREFIX_PATH to the AXL install.")
    endif()
    add_library(axl::axl UNKNOWN IMPORTED)
    set_target_properties(axl::axl PROPERTIES
        IMPORTED_LOCATION "${AXL_LIBRARY}"
        INTERFACE_INCLUDE_DIRECTORIES "${AXL_INCLUDE_DIR}")
endif()
find_package(OpenSSL REQUIRED)
find_package(veloc  REQUIRED)

add_library(pdi_veloc_plugin MODULE
    veloc_wrapper.cxx
    veloc.cxx
    veloc_cfg.cxx)

# to link libraries needed but whose symbols are not directly present in the plugin 
target_link_libraries(pdi_veloc_plugin
    PUBLIC
        PDI::PDI_plugins
        veloc::client
        MPI::MPI_C
        "-Wl,--push-state,--no-as-needed"
        veloc::modules
        er::er
        axl::axl
        OpenSSL::Crypto
        "-Wl,--pop-state")

set_target_properties(pdi_veloc_plugin PROPERTIES
    CXX_VISIBILITY_PRESET  hidden)

set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_veloc_plugin
    LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}")

if(BUILD_TESTING)
    add_subdirectory(tests)
endif()