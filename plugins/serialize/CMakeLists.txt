# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_serialize_plugin LANGUAGES C CXX)

include(CTest)
include(GNUInstallDirs)

# PDI
find_package(PDI REQUIRED plugins)

# The plugin

add_library(pdi_serialize_plugin MODULE serialize.cxx)

target_link_libraries(pdi_serialize_plugin PUBLIC PDI::PDI_plugins)
set_target_properties(pdi_serialize_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_serialize_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
