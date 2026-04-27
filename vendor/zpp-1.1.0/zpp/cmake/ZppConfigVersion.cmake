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

include("${CMAKE_CURRENT_LIST_DIR}/ZppConfig.cmake")

execute_process(COMMAND "${ZPP_EXECUTABLE}" "--version"
	RESULT_VARIABLE _ZPP_VERSION_RESULT
	OUTPUT_VARIABLE PACKAGE_VERSION
	ERROR_VARIABLE _ZPP_VERSION_ERROR
	OUTPUT_STRIP_TRAILING_WHITESPACE
)
if(_ZPP_VERSION_RESULT AND NOT "${_ZPP_VERSION_RESULT}" EQUAL 0 AND NOT ZPP_FIND_QUIETLY)
	message("$ ${ZPP_EXECUTABLE} --version\n${PACKAGE_VERSION}\n${_ZPP_VERSION_ERROR}\n\$?=${_ZPP_VERSION_RESULT}")
endif()

# Check whether the requested PACKAGE_FIND_VERSION is compatible
if("${PACKAGE_VERSION}" VERSION_LESS "${PACKAGE_FIND_VERSION}")
  set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()
  set(PACKAGE_VERSION_COMPATIBLE TRUE)
  if ("${PACKAGE_VERSION}" VERSION_EQUAL "${PACKAGE_FIND_VERSION}")
    set(PACKAGE_VERSION_EXACT TRUE)
  endif()
endif()

cmake_policy(POP)
