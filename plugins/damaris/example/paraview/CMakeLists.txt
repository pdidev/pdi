add_definitions(-DPARALLEL)

add_executable (image image.cpp)
target_link_libraries (image PDI::PDI_C MPI::MPI_CXX)
set_target_properties (image PROPERTIES LINK_FLAGS "-Wl,-export-dynamic")
#INSTALL(TARGETS image RUNTIME DESTINATION bin)