# SPDX-FileCopyrightText: 2023-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_json_plugin LANGUAGES C CXX)

# Includes
include(CTest)
include(GNUInstallDirs)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)
find_package(nlohmann_json 3.9 REQUIRED)

# The plugin
add_library(pdi_json_plugin MODULE json.cxx)
target_link_libraries(pdi_json_plugin PUBLIC PDI::PDI_plugins nlohmann_json::nlohmann_json )

# Installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_json_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
