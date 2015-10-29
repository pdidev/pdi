cmake_minimum_required(VERSION 3.0)
project(PDI LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

option(BUILD_SHARED_LIBS 
	"Build PDI as a shared library instead of a static one"
	ON
)
option(WITH_TESTS
	"Build tests in addition to the actual library"
	ON
)
option(USE_SYSTEM_PARACONF
	"Use an already installed paraconf instead of building the embedded one"
	OFF
)

find_package(MPI REQUIRED)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf)
else()
	add_subdirectory(vendor/paraconf)
endif()

add_library(pdi src/api.c src/config.c)
target_link_libraries(pdi paraconf mpi)
target_include_directories(pdi
	PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
)

if("${WITH_TESTS}")
	add_subdirectory(example/)
endif()
