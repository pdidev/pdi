#=============================================================================
# Copyright (c) 2020-2024 Centre national de la recherche scientifique (CNRS)
# Copyright (c) 2020-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# Copyright (c) 2020-2023 Institut national de recherche en informatique et en automatique (Inria)
# Copyright (c) 2020-2024 Université Paris-Saclay
# Copyright (c) 2020-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
#=============================================================================

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_deisa_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(GNUInstallDirs)

find_package(Python3Path REQUIRED COMPONENTS Interpreter Development)

# PyBind11
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
find_package(pybind11 2.3.0 REQUIRED)

# PDI
find_package(PDI REQUIRED COMPONENTS pysupport)
find_package(MPI REQUIRED COMPONENTS CXX)

# The plugin
add_library(pdi_deisa_plugin MODULE deisa.cxx)
target_link_libraries(pdi_deisa_plugin PUBLIC PDI::PDI_plugins PDI::PDI_pysupport pybind11::embed MPI::MPI_CXX)
set_target_properties(pdi_deisa_plugin PROPERTIES
    CXX_STANDARD_REQUIRED TRUE
    CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_deisa_plugin
    LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)
