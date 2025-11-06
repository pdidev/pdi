# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
FindDamaris
--------

Find Damaris, a library for data managmement and visualisation that provides 
asynchronous, in situ processing capabilities to MPI based simulation codes.
#]=======================================================================]

#include(${CMAKE_CURRENT_LIST_DIR}/SelectLibraryConfigurations.cmake)
#include(${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)
include(FindPackageHandleStandardArgs)
include(DamarisPluginUtils)

set(Damaris_BASE_DIR /usr/local/lib/damaris /usr/lib/damaris /opt/damaris)
set(Damaris_VERSIONS 1.12.0)
set(MIN_REQUIRED_VERSIONS "1.12.0")

#Find Damaris base install dir
set(Damaris_CANDIDATES ${Damaris_ROOT})
foreach(base_dir ${Damaris_BASE_DIR})  
  if(EXISTS "${base_dir}" AND IS_DIRECTORY "${base_dir}")
    list(APPEND Damaris_CANDIDATES ${base_dir}) #In case no version dir exists
    
    if(DEFINED Damaris_VERSION)
        message(STATUS "Provided damaris version: ${Damaris_VERSION}")
        string(REGEX MATCH "^[0-9]+\\.[0-9]+\\.[0-9]+$" IS_VERSION "${Damaris_VERSION}")

        if(IS_VERSION AND (EXISTS "${base_dir}/${Damaris_VERSION}" AND IS_DIRECTORY "${base_dir}/${Damaris_VERSION}"))
            version_greater_equal(${Damaris_VERSION} ${MIN_REQUIRED_VERSIONS} IS_GE)

            if(IS_GE)
                list(APPEND Damaris_CANDIDATES ${base_dir}/${FOLDER_NAME})
            else()
              message(STATUS "damaris_plugin requires at least version \"1.12.0\", provided: ${Damaris_VERSION}")
            endif()
        elseif(IS_VERSION)
          message(STATUS "No Damaris installation with version \"${Damaris_VERSION}\" folder!")
        endif()
    else()
      # Get all subdirectories
      file(GLOB SUBFOLDERS LIST_DIRECTORIES true "${base_dir}/*")

      foreach(SUB "${SUBFOLDERS}")
        # Get the folder name only (without path)
        get_filename_component(FOLDER_NAME "${SUB}" NAME)

        # Check if the folder name matches x.y.z
        string(REGEX MATCH "^[0-9]+\\.[0-9]+\\.[0-9]+$" IS_VERSION "${FOLDER_NAME}")

        if(IS_VERSION)
            version_greater_equal(${FOLDER_NAME} ${MIN_REQUIRED_VERSIONS} IS_GE)

            if(IS_GE)
                list(APPEND Damaris_CANDIDATES ${base_dir}/${FOLDER_NAME})
            #else()
            endif()
        endif()
      endforeach()
    endif()

    #foreach(version ${Damaris_VERSIONS})
      #list(APPEND Damaris_CANDIDATES ${base_dir}/${version})
    #endforeach()
  endif()
endforeach()

set(Damaris_INC_SUFFIXES)
set(Damaris_LIB_SUFFIXES)
foreach(version ${Damaris_VERSIONS})
  list(APPEND Damaris_INC_SUFFIXES include)
  list(APPEND Damaris_LIB_SUFFIXES lib)
endforeach()

find_path(Damaris_INCLUDE_DIRS Damaris.h 
        HINTS ${Damaris_CANDIDATES} 
        PATH_SUFFIXES ${Damaris_INC_SUFFIXES} 
	ENV CPLUS_INCLUDE_PATH)
find_library(Damaris_LIBRARIES damaris
        HINTS ${Damaris_CANDIDATES}
        PATH_SUFFIXES ${Damaris_LIB_SUFFIXES} 
	ENV LIBRARY_PATH LD_LIBRARY_PATH)

find_package_handle_standard_args(Damaris DEFAULT_MSG Damaris_INCLUDE_DIRS 
		Damaris_LIBRARIES)
mark_as_advanced(Damaris_INCLUDE_DIRS Damaris_LIBRARIES)
if(Damaris_INCLUDE_DIRS)
 	set(Damaris_FOUND TRUE)
	get_filename_component(Damaris_LIBRARIES_PATH ${Damaris_LIBRARIES} DIRECTORY)
endif(Damaris_INCLUDE_DIRS)