# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

cmake_minimum_required(VERSION 3.16...3.29)
project(pdi_catalyst_plugin LANGUAGES C CXX)

option(BUILD_CATALYST_PLUGIN   "Build Catalyst plugin" ON)
option(BUILD_FORTRAN           "Enable Fortran support" OFF)
option(BUILD_CATALYST_PARALLEL "Enable Catalyst parallel build" ON)

include(CTest)

# if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
# 	enable_language(Fortran)
# 	set(PDI_COMPONENTS f90)
# endif()

include(GNUInstallDirs)

# PDI
#find_package(PDI REQUIRED COMPONENTS plugins)

# Python
find_package(Python3 REQUIRED COMPONENTS Interpreter Development)

# Catalyst
# find_package(catalyst REQUIRED)
find_package(catalyst REQUIRED)
if("${BUILD_CATALYST_PARALLEL}" AND NOT "${CATALYST_USE_MPI}")
	message(FATAL_ERROR "Catalyst with MPI support required, sequential catalyst only found. Please set -DBUILD_CATALYST_PARALLEL=OFF to disable parallel Catalyst")
endif()

# MPI
if("${CATALYST_USE_MPI}")
	# if("${BUILD_TESTING}" AND "${BUILD_FORTRAN}")
	# 	set(MPI_COMPONENTS Fortran)
	# endif()
	find_package(MPI REQUIRED COMPONENTS C) # ${MPI_COMPONENTS})
	set(CATALYST_DEPS catalyst::catalyst MPI::MPI_C)
endif()

option(CATALYST_IS_PARALLEL "Catalyst is build in parallel" "${CATALYST_USE_MPI}")
message(WARNING "CATALYST_IS_PARALLEL=${CATALYST_IS_PARALLEL}")
message(WARNING "${CATALYST_USE_MPI}=${CATALYST_USE_MPI}")

# PDI
find_package(PDI REQUIRED COMPONENTS plugins) # ${PDI_COMPONENTS})

# The Plugin
add_library(pdi_catalyst_plugin MODULE pdi_catalyst_plugin.cxx)
if("${CATALYST_USE_MPI}")
    target_compile_definitions(pdi_catalyst_plugin PUBLIC CATALYST_IS_PARALLEL=ON)
endif()
target_link_libraries(pdi_catalyst_plugin PDI::PDI_plugins ${CATALYST_DEPS})

# Installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_catalyst_plugin
                LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

# Tests
if("${BUILD_TESTING}")
    enable_testing()
    add_subdirectory(tests)
    add_subdirectory(tests_ghost)
endif()