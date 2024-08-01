#=============================================================================
# Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.10...3.25)
project(pdi_test_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_DEPRECATED "Build deprecated modules" OFF)
if("${BUILD_DEPRECATED}")
	message(WARNING "the test plugin is deprecated, unmaintained and unsupported")
else()
	message(FATAL_ERROR "the test plugin is deprecated, unmaintained and unsupported, set -DBUILD_DEPRECATED=ON to build anyway")
endif()

option(BUILD_CFG_VALIDATOR "Build config validation script" OFF)

# Includes
include(GNUInstallDirs)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# The plugin
add_library(pdi_test_plugin MODULE test.cxx)
target_link_libraries(pdi_test_plugin PDI::PDI_plugins)
set_target_properties(pdi_test_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_test_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/")
endif()
