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
find_package(er     REQUIRED)
find_package(axl    REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(veloc  REQUIRED)

# HDF5
set(HDF5_USE_STATIC_LIBRARIES OFF)
set(HDF5_PREFER_PARALLEL "${BUILD_HDF5_PARALLEL}")
find_package(HDF5 REQUIRED COMPONENTS C)
if("${HDF5_VERSION}" VERSION_LESS 1.10)
    message(FATAL_ERROR "HDF5 version 1.10 at least required, HDF5 ${HDF5_VERSION} found.")
endif()
set(HDF5_DEPS hdf5::hdf5)

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