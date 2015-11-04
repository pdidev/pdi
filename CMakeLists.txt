cmake_minimum_required(VERSION 3.0)
# enable C++ to work-around cmake issue #15829 https://cmake.org/Bug/view.php?id=15829
project(pdi LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")


# Build options

option(BUILD_SHARED_LIBS 
	"Build pdi as a shared library instead of a static one"
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


# used modules

include(GenerateExportHeader)
find_package(MPI REQUIRED)
if("${USE_SYSTEM_PARACONF}")
	find_package(paraconf)
else()
	add_subdirectory(vendor/paraconf)
endif()


# the version file

file(READ VERSION pdi_VERSION)

add_library(pdi
	src/api.c
	src/conf.c
)
generate_export_header(pdi)
set_target_properties(pdi PROPERTIES C_VISIBILITY_PRESET hidden)
target_include_directories(pdi
	PUBLIC
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include/>
		$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/>
		$<INSTALL_INTERFACE:${CMAKE_INSTALL_PREFIX}/include/>
)
target_link_libraries(pdi paraconf mpi)


# the test example

if("${WITH_TESTS}")
	add_subdirectory(example/)
endif()


# installation

configure_file(cmake/pdiConfigVersion.cmake.in
	"${CMAKE_CURRENT_BINARY_DIR}/pdiConfigVersion.cmake"
	@ONLY
)
install(TARGETS pdi
	EXPORT pdi
	ARCHIVE DESTINATION lib
	LIBRARY DESTINATION lib
	PUBLIC_HEADER DESTINATION include
	INCLUDES DESTINATION include
)
install(FILES include/pdi.h
	DESTINATION include
)
install(EXPORT pdi DESTINATION "lib/CMake/pdi" )
install(FILES
		"cmake/FindMPI.cmake"
		"cmake/FindLibYaml.cmake"
		"cmake/pdiConfig.cmake"
		"${CMAKE_CURRENT_BINARY_DIR}/pdiConfigVersion.cmake"
	DESTINATION "lib/CMake/pdi"
)
