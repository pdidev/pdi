cmake_minimum_required(VERSION 3.22)

project(example_damaris_only VERSION 1.0.0)

add_executable(example_damaris_only example_damaris_only.c)


set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../cmake")
include(GNUInstallDirs)

# MPI
find_package(MPI REQUIRED)

find_package(Damaris 1.12.0 REQUIRED)
include_directories(${Damaris_INCLUDE_DIRS} ${Damaris_INCLUDE_DIRS}/damaris)
        
target_link_directories(${PROJECT_NAME} 
    PUBLIC ${Damaris_LIBRARIES_PATH}
)

target_link_libraries(${PROJECT_NAME} damaris)
