cmake_minimum_required(VERSION 3.0)
cmake_policy(SET CMP0054 NEW)

set(Paraconf_VERSION "0.0.0")

set(SRC src/paraconf.cpp)

add_library(paraconf ${SRC})
target_link_libraries(paraconf /usr/lib/x86_64-linux-gnu/libyaml.so)
target_include_directories(paraconf
	PUBLIC
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
		/usr/include
)

set(WORK_DIR "${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY}")
configure_file(cmake/ParaconfConfigVersion.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfigVersion.cmake" @ONLY)
configure_file(cmake/ParaconfConfig.cmake.in "${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfig.cmake" @ONLY)
configure_file(cmake/ParaconfConfig.cmake.in "${WORK_DIR}/ParaconfConfig.cmake" @ONLY)

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
		"${CMAKE_CURRENT_BINARY_DIR}/ParaconfConfigVersion.cmake"
		"${WORK_DIR}/ParaconfConfig.cmake"
	DESTINATION "lib/CMake/Paraconf"
)
