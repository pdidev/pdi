#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================


cmake_minimum_required(VERSION 3.10)
project(pdi_flowvr_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_INDENT "Enable automatic code indentation" OFF)

include(CTest)

include(GNUInstallDirs)

# Astyle
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
endif()

# FMT
find_package(fmt 6.1.2 REQUIRED)

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")

# FlowVR
find_package(FlowVR REQUIRED)

# The plugin
add_subdirectory(plugin/)

# Python script
add_subdirectory(tools)

# Examples
if("${BUILD_TESTING}")
	add_subdirectory(examples/)
	add_subdirectory(tests/)
endif()
