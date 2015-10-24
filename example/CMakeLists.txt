
add_executable(pdi_example 
	example.c
)
target_link_libraries(pdi_example pdi)
set_target_properties(pdi PROPERTIES
	LINK_FLAGS ${MPI_C_LINK_FLAGS}
	COMPILE_FLAGS ${MPI_C_COMPILE_FLAGS}
)