#=============================================================================
# Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
# # SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.5)
project(pdi_decl_hdf5_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_CFG_VALIDATOR "Build config validation script" OFF)
option(BUILD_FORTRAN       "Enable Fortran support" ON)
option(BUILD_HDF5_PARALLEL "Enable HDF5 parallel build" ON)
option(BUILD_INDENT        "Enable automatic code indentation" OFF)

include(CTest)

if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	enable_language(Fortran)
	set(PDI_COMPONENTS f90)
endif()

include(GNUInstallDirs)

# Astyle
if("${BUILD_INDENT}")
	find_package(Astyle 3.1 REQUIRED)
endif()

# HDF5
set(HDF5_USE_STATIC_LIBRARIES OFF)
set(HDF5_PREFER_PARALLEL "${BUILD_HDF5_PARALLEL}")
find_package(HDF5 REQUIRED COMPONENTS C)
if("${HDF5_VERSION}" VERSION_LESS "1.8.0")
	message(FATAL_ERROR "HDF5 version 1.8.0 at least required, HDF5 ${HDF5_VERSION} found.")
endif()
if("${BUILD_HDF5_PARALLEL}" AND NOT "${HDF5_IS_PARALLEL}")
	message(FATAL_ERROR "Parallel HDF5 required, sequential HDF5 only found. Please set -DBUILD_HDF5_PARALLEL=OFF to disable parallel HDF5")
endif()
set(HDF5_DEPS hdf5::hdf5)

# MPI
if("${HDF5_IS_PARALLEL}")
	if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
		set(MPI_COMPONENTS Fortran)
	endif()
	find_package(MPI REQUIRED COMPONENTS CXX C ${MPI_COMPONENTS})
	set(HDF5_DEPS hdf5::hdf5 MPI::MPI_C MPI::MPI_CXX)
endif()

# PDI
find_package(PDI REQUIRED COMPONENTS plugins ${PDI_COMPONENTS})

# The plugin
add_library(pdi_decl_hdf5_plugin MODULE
		attribute_op.cxx
		dataset_op.cxx
		decl_hdf5.cxx
		collision_policy.cxx
		file_op.cxx
		hdf5_wrapper.cxx
		selection.cxx)
target_link_libraries(pdi_decl_hdf5_plugin PUBLIC PDI::PDI_plugins ${HDF5_DEPS})
set_target_properties(pdi_decl_hdf5_plugin PROPERTIES
		CXX_STANDARD 14
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${BUILD_INDENT}")
	Astyle_add_indent(indent TEST
		OPTIONS_FILE formating.astyle
		*.cxx *.h tests/*.c tests/compatibility_tests/*.c)
endif()

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_decl_hdf5_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	find_package(Python3Path REQUIRED COMPONENTS Interpreter)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${Python3Path_INSTALL_SITELIBDIR}/")
endif()

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
