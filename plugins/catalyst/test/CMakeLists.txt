add_executable(TestPDICatalyst
  Grid.cxx
  Grid.h
  Attributes.h
  Attributes.cxx
  main.cxx
  PDIAdaptor.h
  PDIAdaptor.cxx
  pdi.yml.in
  catalyst_pipeline.py
  catalyst_pipeline_with_rendering.py)


find_package(MPI COMPONENTS C CXX REQUIRED)
target_link_libraries(TestPDICatalyst
  PRIVATE
    MPI::MPI_C
    MPI::MPI_CXX
    paraconf::paraconf
    PDI::pdi)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/test)
configure_file(pdi.yml.in pdi.yml)

add_test(NAME TestPDICatalyst COMMAND TestPDICatalyst ${CMAKE_BINARY_DIR}/test/pdi.yml)
add_test(NAME TestPDICatalystMPI COMMAND mpirun -np 4 TestPDICatalyst ${CMAKE_BINARY_DIR}/test/pdi.yml)
