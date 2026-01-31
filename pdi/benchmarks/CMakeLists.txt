#=============================================================================
# Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.22...4.2)

project(PDI_benchmarks)

if(NOT TARGET GTest::gtest)
  option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
  add_subdirectory("../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
if(NOT TARGET benchmark::benchmark)
  option(BENCHMARK_ENABLE_TESTING "Enable testing of the benchmark library." OFF)
  option(BENCHMARK_ENABLE_WERROR "Build Release candidates with -Werror." OFF)
  option(BENCHMARK_ENABLE_INSTALL "Enable installation of benchmark. (Projects embedding benchmark may want to turn this OFF.)" OFF)
  add_subdirectory("../../vendor/benchmark-4ed29ae/" "benchmark" EXCLUDE_FROM_ALL)
endif()

include(CTest)

if("x${BENCHMARK_RESULT_PATH}" STREQUAL "x")
  set(BENCHMARK_RESULT_PATH "${CMAKE_BINARY_DIR}/benchmarks")
endif()

set(PDI_benchmark_SRC
          PDI_callbacks.cxx
          PDI_context.cxx
          PDI_datatype_template.cxx
          PDI_example.cxx
          PDI_expression.cxx
          PDI_logger.cxx)
        
add_executable(PDI_benchmarks ${PDI_benchmark_SRC})
target_compile_features(PDI_benchmarks PUBLIC cxx_std_17)
target_include_directories(PDI_benchmarks PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../src")
target_link_libraries(PDI_benchmarks
                      benchmark::benchmark
                      benchmark::benchmark_main
                      PDI::PDI_C
                      PDI::PDI_plugins)

set(RUNTEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/cmake/runtest-dir")
add_test(NAME PDI_benchmarks
         COMMAND "${RUNTEST_DIR}" sh -c "$<TARGET_FILE:PDI_benchmarks> --benchmark_format=json > ${BENCHMARK_RESULT_PATH}/PDI_benchmark_result.json")
set_property(TEST PDI_benchmarks PROPERTY TIMEOUT 300)
