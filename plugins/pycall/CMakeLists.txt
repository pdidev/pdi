# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_pycall_plugin LANGUAGES C CXX)

include(CTest)

include(GNUInstallDirs)

find_package(Python3 3.10 REQUIRED COMPONENTS Interpreter Development)

# PyBind11
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
find_package(pybind11 2.9 REQUIRED)

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
