# add_subdirectory (storage)
# add_subdirectory (paraview)
# add_subdirectory (python)

add_executable (example example.c)
target_link_libraries (example PDI::PDI_C MPI::MPI_CXX)
# set_target_properties (example PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")