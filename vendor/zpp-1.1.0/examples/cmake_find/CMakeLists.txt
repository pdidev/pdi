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
project(ZppExample Fortran)

# we add the directory where ZPP is installed, not required if ZPP is installed to a standard directory
list(INSERT CMAKE_PREFIX_PATH 0 "${CMAKE_CURRENT_SOURCE_DIR}/../../")

find_package(Zpp REQUIRED)

zpp_preprocess(SRC_Zpp example.F90.zpp)
add_executable(example ${SRC_Zpp})
