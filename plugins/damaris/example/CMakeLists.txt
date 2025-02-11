add_subdirectory (storage)
add_subdirectory (paraview)
add_subdirectory (python)

add_executable (exemple exemple.c)
target_link_libraries (exemple PDI::PDI_C MPI::MPI_CXX)
set_target_properties (exemple PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")