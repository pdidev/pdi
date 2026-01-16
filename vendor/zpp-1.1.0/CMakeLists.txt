##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

cmake_minimum_required(VERSION 3.16...3.28)

# if we are added by add_subdirectory(), this is an error
if("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_SOURCE_DIR}")
	message(FATAL_ERROR "Zpp can either be used with add_subdirectory with cmake or installed using python")
endif()

# if we are added by add_subdirectory(), behave as with find_package
find_package(Python 3.8 REQUIRED COMPONENTS Interpreter)
configure_file(bin/zpp.in zpp @ONLY)
set(ZPP_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/zpp")
set(ZPP_EXECUTABLE "${CMAKE_CURRENT_BINARY_DIR}/zpp" PARENT_SCOPE)
include("${CMAKE_CURRENT_SOURCE_DIR}/zpp/cmake/Zpp.cmake")
