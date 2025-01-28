
add_executable(TestPDICatalyst
  Grid.cxx
  Grid.h
  Attributes.h
  Attributes.cxx
  main.cxx
  PDIAdaptor.h
  PDIAdaptor.cxx)


find_package(MPI COMPONENTS C CXX REQUIRED)
target_link_libraries(TestPDICatalyst
  PRIVATE
    MPI::MPI_C
    MPI::MPI_CXX
    paraconf::paraconf
    PDI::pdi)

set(CATALYST_SCRIPT_FOLDER ${CMAKE_SOURCE_DIR}/test)
configure_file(pdi.yml.in pdi.yml)

find_package(Python3 COMPONENTS Interpreter)
add_test(NAME TestPDICatalyst COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test/run_test.py" "${CMAKE_BINARY_DIR}/test/" "${CMAKE_SOURCE_DIR}/test/")
add_test(NAME TestPDICatalystMPI COMMAND ${Python3_EXECUTABLE} "${CMAKE_SOURCE_DIR}/test/run_test_mpi.py" "${CMAKE_BINARY_DIR}/test/" "${CMAKE_SOURCE_DIR}/test/" "${MPIEXEC_EXECUTABLE}")
