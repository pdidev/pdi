# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

find_package(Python3 3.10 REQUIRED COMPONENTS Interpreter)
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../../cmake/runtest-dir")

add_test(NAME set_value_01_py_share COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_01_share.py" "${CMAKE_CURRENT_SOURCE_DIR}/../set_value_test_01_share.yml")
add_test(NAME set_value_02_py_share COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_02_share.py" "${CMAKE_CURRENT_SOURCE_DIR}/../set_value_test_02_share.yml")
add_test(NAME set_value_03_py COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03.py" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03.yml")
add_test(NAME set_value_04_py COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.py" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.yml")
