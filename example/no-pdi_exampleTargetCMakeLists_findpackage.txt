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

cmake_minimum_required(VERSION 3.9)
project(pdi_examples LANGUAGES C)

find_package(spdlog)
find_package(MPI REQUIRED COMPONENTS C)
find_package(paraconf 1.0.0 REQUIRED COMPONENTS C)
find_package(PDI 1.0.0 REQUIRED COMPONENTS C)

set(CMAKE_C_STANDARD 99)

add_executable(ex1 ex1.c)
target_link_libraries(ex1 m MPI::MPI_C paraconf::paraconf)

add_executable(ex2 ex2.c)
target_link_libraries(ex2 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex3 ex3.c)
target_link_libraries(ex3 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex4 ex4.c)
target_link_libraries(ex4 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex5 ex5.c)
target_link_libraries(ex5 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex6 ex6.c)
target_link_libraries(ex6 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex7 ex7.c)
target_link_libraries(ex7 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex8 ex8.c)
target_link_libraries(ex8 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex9 ex9.c)
target_link_libraries(ex9 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex10 ex10.c)
target_link_libraries(ex10 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_executable(ex11 ex11.c)
set_target_properties(ex11 PROPERTIES ENABLE_EXPORTS TRUE)
target_link_libraries(ex11 m MPI::MPI_C paraconf::paraconf PDI::pdi)

add_subdirectory(ex_deisa/)
