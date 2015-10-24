cmake_minimum_required(VERSION 3.0)
project(pdi LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

find_package(MPI)
find_package(LibYaml)

file(READ VERSION Paraconf_VERSION)

add_library(paraconf
	src/paraconf.c
)
target_link_libraries(paraconf mpi yaml)
target_include_directories(paraconf
	PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)

configure_file(cmake/ParaconfConfigVersion.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfigVersion.cmake" @ONLY)
configure_file(cmake/ParaconfConfig.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfig.cmake" @ONLY)

install(TARGETS paraconf
	EXPORT ParaconfLib
	ARCHIVE DESTINATION lib
	LIBRARY DESTINATION lib
	PUBLIC_HEADER DESTINATION include
	INCLUDES DESTINATION include
)
install(FILES include/paraconf.h
	DESTINATION include
)
install(EXPORT ParaconfLib DESTINATION "lib/CMake/Paraconf" )
install(FILES
		"cmake/FindMPI.cmake"
		"cmake/FindLibYaml.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfigVersion.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfig.cmake"
	DESTINATION "lib/CMake/Paraconf"
)
