# SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

project(decl_hdf5_benchmarks)

if(NOT TARGET GTest::gtest)
  option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
  add_subdirectory("../../../vendor/googletest-56efe39/" "googletest" EXCLUDE_FROM_ALL)
endif()
if(NOT TARGET benchmark::benchmark)
  option(BENCHMARK_ENABLE_TESTING "Enable testing of the benchmark library." OFF)
  option(BENCHMARK_ENABLE_WERROR "Build Release candidates with -Werror." OFF)
  option(BENCHMARK_ENABLE_INSTALL "Enable installation of benchmark. (Projects embedding benchmark may want to turn this OFF.)" OFF)
  add_subdirectory("../../../vendor/benchmark-4ed29ae/" "benchmark" EXCLUDE_FROM_ALL)
endif()

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
