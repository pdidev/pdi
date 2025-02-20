# Set the default search paths
set(PDI_DIRS "${CMAKE_CURRENT_LIST_DIR}/..")

# Search for a directory in CMAKE_MODULE_PATH that includes "no-pdi",
# which is passed as an argument to the cmake command
set(NO_PDI_PATH "")
foreach(module_path IN LISTS CMAKE_MODULE_PATH)
  if(module_path MATCHES ".*no-pdi.*")
    set(NO_PDI_PATH "${module_path}")
    break()
  endif()
endforeach()

if(EXISTS ${NO_PDI_PATH})
    # Set the include directories to the specific folder path
    set(PDI_INCLUDE_DIRS ${NO_PDI_PATH})
    cmake_print_variables(NO_PDI_PATH)
    set(PDI_LIBRARIES "")
    set(PDI_FOUND TRUE)
    include(${NO_PDI_PATH}/no-pdi.cmake)
else()
    # If the specific folder does not exist, try to find the package using the default search paths
    find_path(PDI_INCLUDE_DIRS NAMES PDIConfig.cmake PATHS "<path>/<to>/<pdi>/<root>/pdi/build/staging/share/pdi/cmake/")
    cmake_print_variables(PDI_INCLUDE_DIRS)
    if(PDI_INCLUDE_DIRS)
        include(${PDI_INCLUDE_DIRS}/PDIConfig.cmake)
        set(PDI_FOUND TRUE)
    else()
        set(PDI_FOUND FALSE)
    endif()
endif()

# Handle the REQUIRED and QUIET options
if(PDI_FOUND)
    if(NOT PDI_FIND_QUIETLY)
        message(STATUS "Found PDI: ${PDI_INCLUDE_DIRS}.")
    endif()
else()
    if(PDI_FIND_REQUIRED)
        message(FATAL_ERROR "Could not find PDI.")
    endif()
endif()