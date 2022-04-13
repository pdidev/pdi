add_executable(sim simulation_pdi.cxx)
target_link_libraries(sim PDI::PDI_C MPI::MPI_CXX)
INSTALL(TARGETS sim RUNTIME DESTINATION bin)


# # TODO: use as subproject that does find_package on melissa...
# add_executable(simulation_pdi1 simulation_pdi.cxx)
# target_link_libraries(simulation_pdi1 PDI::PDI_C MPI::MPI_CXX)
# # INSTALL(TARGETS simulation_pdi RUNTIME DESTINATION bin)

# add_executable(simulation1 simulation.cxx)
# target_link_libraries(simulation11 melissa_da_api)
# INSTALL(TARGETS simulation1 RUNTIME DESTINATION bin)

# add a stateful simulation for testing
# add_executable(simulation1-stateful simulation.cxx)
# target_compile_definitions(simulation1-stateful PRIVATE -DBE_STATEFUL)
# target_link_libraries(simulation1-stateful melissa_da_api)
# INSTALL(TARGETS simulation1-stateful RUNTIME DESTINATION bin)

# add_executable(simulation1-hidden simulation.cxx)
# target_compile_definitions(simulation1-hidden PRIVATE -DUSE_HIDDEN_STATE -DBE_STATEFUL)
# target_link_libraries(simulation1-hidden melissa_da_api)
# INSTALL(TARGETS simulation1-hidden RUNTIME DESTINATION bin)

# add_executable(simulation1-hidden-index-map simulation.cxx)
# target_compile_definitions(simulation1-hidden-index-map PRIVATE -DWITH_INDEX_MAP -DUSE_HIDDEN_STATE -DBE_STATEFUL)
# target_link_libraries(simulation1-hidden-index-map melissa_da_api)
# INSTALL(TARGETS simulation1-hidden-index-map RUNTIME DESTINATION bin)

# add_executable(simulation1-index-map simulation.cxx)
# target_compile_definitions(simulation1-index-map PRIVATE -DWITH_INDEX_MAP)
# target_link_libraries(simulation1-index-map melissa_da_api)
# INSTALL(TARGETS simulation1-index-map RUNTIME DESTINATION bin)

# add_executable(simulation1-deadlock simulation.cxx)
# target_compile_definitions(simulation1-deadlock PRIVATE -DDEADLOCK)
# target_link_libraries(simulation1-deadlock melissa_da_api)
# INSTALL(TARGETS simulation1-deadlock RUNTIME DESTINATION bin)



# add_test(NAME "test-example-simulation1" COMMAND ${in_env} python3 -u ./script.py ${test_name} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# python_test("test-check-stateless")

# python_test("test-index-map")

# python_test("test-hidden-index-map")

# python_test("test-empty-index-map")

# python_test("test-empty-hidden-index-map")
