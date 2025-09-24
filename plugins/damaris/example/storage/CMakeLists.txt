add_definitions(-DPARALLEL)

add_executable (2dmesh 2dmesh.c)
target_link_libraries (2dmesh PDI::PDI_C MPI::MPI_CXX)
set_target_properties (2dmesh PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")
#INSTALL(TARGETS 2dmesh RUNTIME DESTINATION bin)