# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_catalyst_plugin LANGUAGES C CXX)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# Python
find_package(Python3 REQUIRED COMPONENTS Interpreter Development)

# Catalyst
find_package(catalyst REQUIRED)
if(NOT ${CATALYST_USE_MPI})
    message(WARNING "No MPI support in your Catalyst library, please activate MPI in your Catalyst build if you want to use in parallel.")
endif()

# The Plugin
add_library(pdi_catalyst_plugin MODULE pdi_catalyst_plugin.cxx)
target_link_libraries(pdi_catalyst_plugin PDI::PDI_plugins catalyst::catalyst)

# Installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_catalyst_plugin
                LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
    enable_testing()
    add_subdirectory(tests)
endif()
