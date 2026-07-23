# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

set(RUNTEST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../cmake/runtest-dir")

# Creation of executable
add_executable(TestPDICatalyst
  grid.cxx
  grid.h
  attributes.h
  attributes.cxx
  main.cxx
  pdi_adaptor.h
  pdi_adaptor.cxx)

# MPI
find_package(MPI COMPONENTS C CXX REQUIRED)

target_link_libraries(TestPDICatalyst
  PRIVATE PDI::PDI_C MPI::MPI_C MPI::MPI_CXX)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/test_CxxFullExample)
configure_file(pdi.yml.in pdi.yml)

find_package(Python3 COMPONENTS Interpreter)
add_test(NAME TestPDICatalystSerial COMMAND "${RUNTEST_DIR}" ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test_CxxFullExample/run_test.py" "${CMAKE_BINARY_DIR}/test_CxxFullExample/" "${CMAKE_SOURCE_DIR}/test_CxxFullExample/")

# test with MPI
if(${CATALYST_USE_MPI})
  add_test(NAME TestPDICatalystMPI COMMAND "${RUNTEST_DIR}" ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test_CxxFullExample/run_test_mpi.py" "${CMAKE_BINARY_DIR}/test_CxxFullExample/" "${CMAKE_SOURCE_DIR}/test_CxxFullExample/" "${MPIEXEC_EXECUTABLE}")
  set_property(TEST TestPDICatalystMPI PROPERTY TIMEOUT 15)
endif()