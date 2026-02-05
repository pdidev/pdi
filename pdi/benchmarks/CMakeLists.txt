# SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

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
