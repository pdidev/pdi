# SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_mpi_plugin LANGUAGES C CXX)

option(BUILD_FORTRAN "Enable Fortran support" ON)

include(CTest)

if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	enable_language(Fortran)
	set(PDI_COMPONENTS f90)
endif()

include(GNUInstallDirs)

# MPI
set(MPI_COMPONENTS CXX)
if("${BUILD_TESTING}")
	list(APPEND MPI_COMPONENTS C)
	if("${BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})

# PDI
find_package(PDI REQUIRED COMPONENTS plugins ${PDI_COMPONENTS})

# The plugin
add_library(pdi_mpi_plugin MODULE mpi.cxx)
target_link_libraries(pdi_mpi_plugin PUBLIC PDI::PDI_plugins MPI::MPI_CXX)
set_target_properties(pdi_mpi_plugin PROPERTIES CXX_VISIBILITY_PRESET hidden)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_mpi_plugin
		LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
	add_subdirectory(tests/)
endif()
