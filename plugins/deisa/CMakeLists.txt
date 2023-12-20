f#=============================================================================
# Copyright (c) 2020-2024 Centre national de la recherche scientifique (CNRS)
# Copyright (c) 2020-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# Copyright (c) 2020-2024 Institut national de recherche en informatique et en automatique (Inria)
# Copyright (c) 2020-2024 Université Paris-Saclay
# Copyright (c) 2020-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
#=============================================================================

cmake_minimum_required(VERSION 3.9)
project(pdi_deisa_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_INDENT  "Enable automatic code indentation" OFF)

include(GNUInstallDirs)

find_package(Python3Path REQUIRED COMPONENTS Interpreter Development)

# PyBind11
# Workaround pybind11 incompatibility with cmake<3.12 if Python3_FOUND is set
set(_pdi_deisa_plugin_Python3_FOUND "${Python3_FOUND}")
unset(Python3_FOUND)
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
find_package(pybind11 2.3.0 REQUIRED)
set(Python3_FOUND "${_pdi_deisa_plugin_Python3_FOUND}")

# PDI
find_package(PDI 1.5 REQUIRED COMPONENTS pysupport plugins)
find_package(MPI REQUIRED COMPONENTS CXX)

# The plugin
add_library(pdi_deisa_plugin MODULE deisa.cxx)
# link against Python3::Python to workaround https://github.com/pybind/pybind11/issues/2664
target_link_libraries(pdi_deisa_plugin PUBLIC PDI::PDI_plugins PDI::PDI_pysupport pybind11::embed Python3::Python MPI::MPI_CXX)
#target_compile_features(pdi_deisa_plugin PRIVATE cxx_std_14)
set_target_properties(pdi_deisa_plugin PROPERTIES
    CXX_STANDARD_REQUIRED TRUE
    CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_deisa_plugin
    LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)
