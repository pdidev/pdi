#=============================================================================
# Copyright (C) 2015-2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.16)
#project(pdi_init LANGUAGES C)

#find_package(spdlog)
find_package(MPI REQUIRED COMPONENTS C)
#find_package(paraconf 1.0.0 REQUIRED COMPONENTS C)
#find_package(PDI 1.9.0 REQUIRED COMPONENTS C)

set(CMAKE_C_STANDARD 99)

add_executable(tuto_main_ghost_attributes main.c)
target_link_libraries(tuto_main_ghost_attributes m MPI::MPI_C paraconf::paraconf PDI::pdi)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/test_tuto_september_2025_ghost_attributes)
configure_file(config_init.yml config.yml)

find_package(Python3 COMPONENTS Interpreter)
add_test(NAME Test_tuto_september_2025_ghost_attributes COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test_tuto_september_2025_ghost_attributes/run_test.py" "${CMAKE_BINARY_DIR}/test_tuto_september_2025_ghost_attributes/" "${CMAKE_SOURCE_DIR}/test_tuto_september_2025_ghost_attributes/")
add_test(NAME Test_tuto_september_2025_ghost_attributes_MPI COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test_tuto_september_2025_ghost_attributes/run_test_mpi.py" "${CMAKE_BINARY_DIR}/test_tuto_september_2025_ghost_attributes/" "${CMAKE_SOURCE_DIR}/test_tuto_september_2025_ghost_attributes/" "${MPIEXEC_EXECUTABLE}")
