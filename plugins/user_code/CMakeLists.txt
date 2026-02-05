# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_user_code_plugin LANGUAGES C CXX)

include(CTest)

include(GNUInstallDirs)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# The plugin
add_library(pdi_user_code_plugin MODULE user_code.cxx)
target_link_libraries(pdi_user_code_plugin PDI::PDI_plugins)
set_target_properties(pdi_user_code_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_user_code_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
