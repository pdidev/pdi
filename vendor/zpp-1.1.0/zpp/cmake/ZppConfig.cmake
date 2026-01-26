##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

cmake_policy(PUSH)
cmake_minimum_required(VERSION 3.16...3.28)

# Hook for python to define the real relative path
#@PYTHON_INSERT_ZPP_EXECUTABLE@

# Compute the installation prefix relative to this file.
if(NOT DEFINED "ZPP_EXECUTABLE")
	get_filename_component(_ZPP_BIN_DIR "${CMAKE_CURRENT_LIST_DIR}" PATH)
	get_filename_component(_ZPP_BIN_DIR "${_ZPP_BIN_DIR}" PATH)
	if(_ZPP_BIN_DIR STREQUAL "/")
		set(_ZPP_BIN_DIR "")
	endif()
	set(ZPP_EXECUTABLE "${_ZPP_BIN_DIR}/bin/zpp")
endif()

include("${CMAKE_CURRENT_LIST_DIR}/Zpp.cmake")

cmake_policy(POP)
