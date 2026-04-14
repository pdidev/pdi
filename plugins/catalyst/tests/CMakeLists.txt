# SPDX-FileCopyrightText: Copyright (c) 2024-2025 Kitware SAS
# SPDX-FileCopyrightText: Copyright (c) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

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
  PRIVATE PDI::PDI_C MPI::MPI_C)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/tests)
configure_file(pdi.yml.in pdi.yml)

find_package(Python3 COMPONENTS Interpreter)
add_test(NAME TestPDICatalystSerial COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/tests/run_test.py" "${CMAKE_BINARY_DIR}/tests/" "${CMAKE_SOURCE_DIR}/tests/")

# test with MPI
if(${CATALYST_USE_MPI})
  add_test(NAME TestPDICatalystMPI COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/tests/run_test_mpi.py" "${CMAKE_BINARY_DIR}/tests/" "${CMAKE_SOURCE_DIR}/tests/" "${MPIEXEC_EXECUTABLE}")
endif()
