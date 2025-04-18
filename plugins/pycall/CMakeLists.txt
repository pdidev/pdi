#=============================================================================
# Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_pycall_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(CTest)

include(GNUInstallDirs)

find_package(Python3 3.8.2 REQUIRED COMPONENTS Interpreter Development)

# PyBind11
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
find_package(pybind11 2.4.3 REQUIRED)

# PDI
find_package(PDI REQUIRED COMPONENTS pysupport)

# The plugin
add_library(pdi_pycall_plugin MODULE pycall.cxx)
# link against Python3::Python to workaround https://github.com/pybind/pybind11/issues/2664
target_link_libraries(pdi_pycall_plugin PDI::PDI_pysupport pybind11::embed Python3::Python)
set_target_properties(pdi_pycall_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_pycall_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
