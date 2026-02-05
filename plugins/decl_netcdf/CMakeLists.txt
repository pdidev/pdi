# SPDX-FileCopyrightText: 2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_decl_netcdf_plugin LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
if("${CMAKE_VERSION}" VERSION_LESS 4.1)
	# Workaround missing "Prefer h5hl<LANG> compilers if HDF5_FIND_HL is enabled" in cmake pre-4.1
	list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/4.1")
endif()

option(BUILD_NETCDF_PARALLEL "Build Decl'NetCDF in parallel mode" ON)

# Includes
include(CTest)
include(GNUInstallDirs)

# MPI
if("${BUILD_NETCDF_PARALLEL}")
	find_package(MPI REQUIRED COMPONENTS C CXX)
endif()

# NetCDF
find_package(NetCDF 4.8 REQUIRED)
if("${BUILD_NETCDF_PARALLEL}" AND NOT "PARALLEL4" IN_LIST NetCDF_FEATURES)
	message(FATAL_ERROR "Parallel NetCDF required, sequential NetCDF only found. Please set -DBUILD_NETCDF_PARALLEL=OFF to disable parallel NetCDF")
endif()

# PDI
find_package(PDI REQUIRED COMPONENTS plugins)

# The plugin
add_library(pdi_decl_netcdf_plugin MODULE
		decl_netcdf.cxx
		dnc_attribute.cxx
		dnc_file_context.cxx
		dnc_group.cxx
		dnc_io.cxx
		dnc_netcdf_file.cxx
		dnc_variable.cxx)
target_link_libraries(pdi_decl_netcdf_plugin PUBLIC PDI::PDI_plugins NetCDF::NetCDF)
if("${BUILD_NETCDF_PARALLEL}")
	target_link_libraries(pdi_decl_netcdf_plugin PUBLIC MPI::MPI_CXX)
endif()
set_target_properties(pdi_decl_netcdf_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_decl_netcdf_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
