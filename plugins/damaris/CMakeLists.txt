#=============================================================================
# Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10...3.25)
project(pdi_damaris_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
add_library(damaris_plugin SHARED damaris_plugin/exampleLib.cpp)



option(BUILD_CFG_VALIDATOR "Build config validation script" OFF)
option(BUILD_INDENT "Enable automatic code indentation" OFF)

# Includes
include(GNUInstallDirs)

include_directories(${DAMARIS_SOURCE_DIR}/include)
include_directories(/gpfs/users/racliuspa/damaris/damaris/include/damaris)
include_directories(/gpfs/users/racliuspa/damaris/damaris/include)
include_directories(/gpfs/users/racliuspa/AlternanceCEA/Damaris/dependencies/xsd/include)
include_directories(/gpfs/users/racliuspa/AlternanceCEA/Damaris/dependencies/xerces/include)
include_directories(/gpfs/softs/spack_0.17/opt/spack/linux-centos7-cascadelake/gcc-11.2.0/boost-1.82.0-hlujyueayu4hsldyalhmrodacattzqg4/include/)
include_directories(/gpfs/users/racliuspa/damaris/damaris/damaris_install/include)
include_directories(/gpfs/softs/spack_0.17/opt/spack/linux-centos7-cascadelake/gcc-11.2.0/openmpi-4.1.1-ujlwnrlh5sewm2rxkpio3h5mariwgetn/include)
#include_directories(${DAMARIS_SOURCE_DIR}/include/damaris/data)
# Astyle
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
endif()

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# The plugin
set_target_properties(damaris_plugin PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY /gpfs/users/racliuspa/AlternanceCEA/PDI/pdi/example
)

add_library(pdi_damaris_plugin MODULE damaris.cxx)
target_link_libraries(pdi_damaris_plugin PDI::PDI_plugins)
set_target_properties(pdi_damaris_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

target_include_directories(pdi_damaris_plugin PRIVATE
    $ENV{HOME}/damaris/damaris/damaris_install/include
	${DAMARIS_SOURCE_DIR}/include/damaris/data
    /gpfs/softs/spack_0.17/opt/spack/linux-centos7-cascadelake/gcc-11.2.0/openmpi-4.1.1-ujlwnrlh5sewm2rxkpio3h5mariwgetn/include
)
include_directories(MPI_INCLUDE_PATH)

target_link_directories(pdi_damaris_plugin PRIVATE
    $ENV{HOME}/damaris/damaris/damaris_install/lib
)

# Indentation
if("${BUILD_INDENT}")
	Astyle_add_indent(indent TEST
		OPTIONS_FILE formating.astyle
		damaris.cxx)
endif()

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_damaris_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/")
endif()
