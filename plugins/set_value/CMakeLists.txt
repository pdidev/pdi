# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_set_value_plugin LANGUAGES C CXX)

# Includes
include(CTest)
include(GNUInstallDirs)

# spdlog
find_package(spdlog 1.9 REQUIRED)

# PDI
find_package(PDI REQUIRED plugins)

# The plugin

add_library(pdi_set_value_plugin MODULE set_value.cxx
										trigger.cxx
										logger_operation.cxx
										operation.cxx
										set_operation.cxx
										share_operation.cxx
										expose_operation.cxx
										event_operation.cxx
										release_operation.cxx)
target_link_libraries(pdi_set_value_plugin PUBLIC PDI::PDI_plugins spdlog::spdlog)
set_target_properties(pdi_set_value_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_set_value_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
