# SPDX-FileCopyrightText: 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)
project(PDI LANGUAGES C)

option(PDI_MOCK_PARACONF_TARGET "Provide a Mock paraconf target in SubdirMock PDI" OFF)

include("${CMAKE_CURRENT_SOURCE_DIR}/PDIConfig.cmake")
