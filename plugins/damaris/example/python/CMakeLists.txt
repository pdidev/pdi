add_definitions(-DPARALLEL)

add_executable (3dmesh_py 3dmesh_py.c)
set_property(TARGET 3dmesh_py PROPERTY C_STANDARD 99)
target_link_libraries (3dmesh_py PDI::PDI_C MPI::MPI_CXX)
set_target_properties (3dmesh_py PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")
#INSTALL(TARGETS 3dmesh_py RUNTIME DESTINATION bin)

add_executable (3dmesh_py_domains 3dmesh_py_domains.c)
set_property(TARGET 3dmesh_py_domains PROPERTY C_STANDARD 99)
target_link_libraries (3dmesh_py_domains PDI::PDI_C MPI::MPI_CXX)
set_target_properties (3dmesh_py_domains PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")
#INSTALL(TARGETS 3dmesh_py_domains RUNTIME DESTINATION bin)
