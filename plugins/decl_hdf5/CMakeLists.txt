# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_decl_hdf5_plugin LANGUAGES C CXX)

option(BUILD_BENCHMARKING  "Build PDI benchmarks" ON)
option(BUILD_FORTRAN       "Enable Fortran support" ON)
option(BUILD_HDF5_PARALLEL "Enable HDF5 parallel build" ON)

include(CTest)

if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	enable_language(Fortran)
	set(PDI_COMPONENTS f90)
endif()

include(GNUInstallDirs)

# HDF5
set(HDF5_USE_STATIC_LIBRARIES OFF)
set(HDF5_PREFER_PARALLEL "${BUILD_HDF5_PARALLEL}")
find_package(HDF5 REQUIRED COMPONENTS C)
if("${HDF5_VERSION}" VERSION_LESS 1.10)
	message(FATAL_ERROR "HDF5 version 1.10 at least required, HDF5 ${HDF5_VERSION} found.")
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
set_target_properties(pdi_decl_hdf5_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_decl_hdf5_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()

if("${BUILD_BENCHMARKING}")
	add_subdirectory(benchmarks)
endif()
