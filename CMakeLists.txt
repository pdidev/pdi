# Header
cmake_minimum_required(VERSION 3.0)
cmake_policy(SET CMP0054 NEW)
list(APPEND CMAKE_MODULE_PATH "cmake")

set(BUILD_SHARED_LIBS ON)

find_package(MPI)
add_subdirectory(vendor/paraconf)

include_directories(include/ ${MPI_C_INCLUDE_PATH})

set(SRC src/pdi.c)
add_library(pdi ${SRC})
target_link_libraries(pdi ${MPI_C_LIBRARIES} paraconf)
set_target_properties(pdi PROPERTIES
	LINK_FLAGS "${MPI_C_LINK_FLAGS}"
	COMPILE_FLAGS "${MPI_C_COMPILE_FLAGS}"
)

set(SRC_TEST example/example.c)
add_executable(pdi_example ${SRC_TEST})
target_link_libraries(pdi_example ${MPI_C_LIBRARIES} pdi paraconf)
set_target_properties(pdi PROPERTIES
	LINK_FLAGS ${MPI_C_LINK_FLAGS}
	COMPILE_FLAGS ${MPI_C_COMPILE_FLAGS}
)