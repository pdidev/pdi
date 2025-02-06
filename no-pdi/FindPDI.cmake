# Set the default search paths
set(PDI_DIRS "${CMAKE_CURRENT_LIST_DIR}/..")

# Check if the specific folder exists
set(NO_PDI_PATH "${CMAKE_MODULE_PATH}")

include(CMakePrintHelpers)
cmake_print_variables(PDI_DIRS)
cmake_print_variables(NO_PDI_PATH)

if(EXISTS ${NO_PDI_PATH})
    # Set the include directories to the specific folder path
    message("DEBUG : NO-PDI - NO-PDI")
    set(PDI_INCLUDE_DIRS ${NO_PDI_PATH})
    cmake_print_variables(NO_PDI_PATH)
    set(PDI_LIBRARIES "")
    set(PDI_FOUND TRUE)
    include(${NO_PDI_PATH}/no-pdi.cmake)
else()
    # If the specific folder does not exist, try to find the package using the default search paths
    message("DEBUG : NO-PDI - PDI")
    # find_path(PDI_INCLUDE_DIRS NAMES PDIConfig.cmake PATHS ${PDI_DIRS})
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