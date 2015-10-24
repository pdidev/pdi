# Header
cmake_minimum_required(VERSION 3.0)
project(paraconf LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_SHARED_LIBS "" ON)
option(WITH_TESTS "" ON)

add_subdirectory(vendor/paraconf)

add_library(pdi src/pdi.c)
target_link_libraries(pdi paraconf)
target_include_directories(paraconf
	PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)

if("${WITH_TESTS}")
	add_subdirectory(example/)
endif()
