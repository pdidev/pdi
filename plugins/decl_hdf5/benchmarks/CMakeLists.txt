#=============================================================================
# Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.10...3.25)

project(decl_hdf5_benchmarks)

find_package(benchmark 1.5.0 REQUIRED)
# find_package(HDF5 1.8.0 REQUIRED COMPONENTS C)

include(CTest)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED TRUE)
set(CMAKE_INCLUDE_CURRENT_DIR TRUE)

if("x${BENCHMARK_RESULT_PATH}" STREQUAL "x")
  set(BENCHMARK_RESULT_PATH "${CMAKE_BINARY_DIR}/benchmarks")
endif()

set(decl_hdf5_benchmark_tests_SRC matrix.cxx record.cxx)
        
add_executable(decl_hdf5_benchmarks ${decl_hdf5_benchmark_tests_SRC})
target_link_libraries(decl_hdf5_benchmarks
                      benchmark::benchmark
                      benchmark::benchmark_main
                      PDI::PDI_C
                      ${HDF5_DEPS})

set(RUNTEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/runtest-dir")
add_test(NAME decl_hdf5_benchmarks
         COMMAND "${RUNTEST_DIR}" sh -c "$<TARGET_FILE:decl_hdf5_benchmarks> --benchmark_format=json > ${BENCHMARK_RESULT_PATH}/decl_hdf5_benchmark_result.json")
set_property(TEST decl_hdf5_benchmarks PROPERTY TIMEOUT 300)