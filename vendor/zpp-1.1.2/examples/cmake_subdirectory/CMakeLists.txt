##############################################################################
# SPDX-FileCopyrightText: 2014-2026 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2026 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2026 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2026 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2026 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

cmake_minimum_required(VERSION 3.22...4.2)
project(ZppExample Fortran)

add_subdirectory(../../ zpp EXCLUDE_FROM_ALL)

zpp_preprocess(SRC_Zpp example.F90.zpp)
add_executable(example ${SRC_Zpp})
