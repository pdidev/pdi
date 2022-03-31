#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2022 Institut National de Recherche en Informatique et en Automatique
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

cmake_minimum_required(VERSION 3.5)
project(pdi_melissa_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(GNUInstallDirs)

find_package(MPI REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS plugins)

add_library(pdi_melissa_plugin MODULE melissa.cxx)
target_link_libraries(pdi_melissa_plugin PUBLIC PDI::PDI_plugins MPI::MPI_C)
set_target_properties(pdi_melissa_plugin PROPERTIES
		CXX_STANDARD 14
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_melissa_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)
