# SPDX-FileCopyrightText: Copyright (c) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-License-Identifier: Apache 2.0

# Creation of executable
add_executable(ExampleParaviewCatalyst main.c)

# MPI
find_package(MPI COMPONENTS C CXX REQUIRED)

target_link_libraries(ExampleParaviewCatalyst
  PRIVATE PDI::PDI_C MPI::MPI_C MPI::MPI_CXX)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/examples)
configure_file(rectilinear_mesh.yml rectilinear_mesh.yml)
configure_file(structured_mesh.yml structured_mesh.yml)
configure_file(uniform_mesh.yml uniform_mesh.yml)
configure_file(unstructured_mesh.yml unstructured_mesh.yml)